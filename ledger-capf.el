;;; ledger-capf.el --- arx cape ledger config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 22, 2021
;; Modified: December 22, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/ledger-capf
;; Package-Requires: ((emacs 29.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  arx cape ledger config
;;
;;; Code:

(defvar cape-legder--properties
  (list :annotation-function (lambda (_) " Ledger")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `ledger-capf'.")

(defcustom ledger-capf-acct-currency-symbol ""
  "Currency symbol to be appended to the end of the account name."
  :type 'string
  :group 'ledger-capf-acct)

(defcustom ledger-capf-acct-master-file "~/.ledgerrc"
  "Location of the file containing account definitions."
  :type 'file
  :group 'ledger-capf-acct)

(defun ledger-capf--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun ledger-capf--interactive (capf)
  "Complete with CAPF."
  (let ((completion-at-point-functions (list capf)))
    (or (completion-at-point) (user-error "%s: No completions" capf))))

(cl-defun ledger-capf--table-with-properties (table &key category (sort t) &allow-other-keys)
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (if (or (not table) (and (not category) sort))
      table
    (let ((metadata `(metadata
                      ,@(and category `((category . ,category)))
                      ,@(and (not sort) '((display-sort-function . identity)
                                          (cycle-sort-function . identity))))))
      (lambda (str pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action table str pred))))))

(defun ledger-capf--input-valid-p (old-input new-input cmp)
  "Return non-nil if the NEW-INPUT is valid in comparison to OLD-INPUT.
The CMP argument determines how the new input is compared to the old input.
- never: Never treat the input as valid.
- prefix/nil: The old input is a prefix of the new input.
- equal: The old input is equal to the new input.
- substring: The old input is a substring of the new input."
  ;; Treat input as not changed if it contains space to allow
  ;; Orderless completion style filtering.
  (or (string-match-p "\\s-" new-input)
      (pcase-exhaustive cmp
        ('never nil)
        ((or 'prefix 'nil) (string-prefix-p old-input new-input))
        ('equal (equal old-input new-input))
        ('substring (string-match-p (regexp-quote old-input) new-input)))))

(defun ledger-capf--cached-table-props (beg end fun valid)
  "Create caching completion table.
BEG and END are the input bounds.
FUN is the function which computes the candidates.
VALID is the input comparator, see `ledger-capf--input-valid-p'."
  (let ((input 'init)
        (beg (copy-marker beg))
        (end (copy-marker end t))
        (table nil))
    (lambda (str pred action)
      (let ((new-input (buffer-substring beg end)))
        (when (or (eq input 'init) (not (ledger-capf--input-valid-p input new-input valid)))
          ;; NOTE: We have to make sure that the completion table is interruptible.
          ;; An interruption should not happen between the setqs.
          (setq table (funcall fun new-input)
                input new-input)))
      (complete-with-action action table str pred))))

(defun ledger-capf--acct-regexp-filter (regexp list type)
  "Use REGEXP to filter LIST of strings.
REGEXP determines how to parse the master file.
LIST is the data from the master file to be filtered.
TYPE is what kind of data we are processing."
  (let (new)
    (dolist (string list)
      (when (string-match regexp string)
        (let ((matched-string (match-string 1 string)))
          ;; set text property either to "account" or "payee"
          ;; use the text property in post completion.
          (setq matched-string (propertize matched-string 'type type))
          (setq new (cons matched-string new)))))
    new))


;;;###autoload
(defun ledger-capf--acct-get-all-accts ()
  "Read the master file that has the account descriptions."
  (ledger-capf--acct-regexp-filter
   "^account \\(.*\\)"
   (mapcar #'(lambda (s)  s)
           (with-temp-buffer
             (insert-file-contents ledger-capf-acct-master-file)
             (split-string (buffer-string) "\n" t)))
   "account"))

;;;###autoload
(defun ledger-capf--acct-get-all-payees ()
  "Read the master file that has the payee descriptions."
  (ledger-capf--acct-regexp-filter
   "^payee \\(.*\\)"
   (mapcar #'(lambda (s)  s)
           (with-temp-buffer
             (insert-file-contents ledger-capf-acct-master-file)
             (split-string (buffer-string) "\n" t)))
   "payee"))

(defun ledger-capf--acct-fuzzy-word-match (prefix candidate)
  "Return non-nil if each (partial) word in PREFIX is also in CANDIDATE."
  (eq nil
      (memq nil
            (mapcar
             #'(lambda (pre) (string-match-p (regexp-quote pre) candidate))
             (split-string prefix)))))

(defun ledger-capf--acct-get-payees-p (arg)
  "Determine is user needs to be prompted with payee information.
If the line starts with a date (new transaction), or if a meta data
tag (Payee:) is present before point, return true
ARG is the string the user has typed into the buffer"
  (interactive)
  (let ((case-fold-search nil))
    ;; must append the user entered string as that too will be present in the buffer
    (if (or (looking-back (concat "Payee\:[\t\r\v\f ]*" arg) nil)
            (string-match-p "^[0-9][0-9][0-9][0-9][\-/][0-9][0-9][\-/][0-9][0-9]" (thing-at-point 'line)))
        t nil)))

(defun ledger-capf--acct-get-accts-p (arg)
  "Determine whether the user should be prompted for accounts.
If the only characters preceeding the ARG are spaces, then the
user is likely to be entering account information."
  (interactive)
  (if (string-match-p "^[ \t\r\f\v]*$"
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (- ( point ) (length arg))))
      t nil))

;;;###autoload
(defun ledger-capf--accts (str)
  "Cape backend to prompt for account names."
  (if (ledger-capf--acct-get-payees-p str)
      (ledger-capf--acct-get-all-payees)
    (if  (ledger-capf--acct-get-accts-p str)
        (ledger-capf--acct-get-all-accts))))

;;;###autoload
(defun ledger-capf (&optional interactive)
  "Complete with Ledger at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (ledger-capf--interactive #'ledger-capf)
    (let ((bounds (ledger-capf--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(ledger-capf--table-with-properties
          (ledger-capf--cached-table-props (car bounds) (cdr bounds) #'ledger-capf--accts 'substring)
          :category 'ledger-capf)
        :exclusive no
        :exit-function (lambda (x _status)
                         (if (string= "account" (get-text-property 0 'type x))
                             (progn
                               (delete-region(+ (line-beginning-position) (current-indentation)) (point))
                               (insert (concat x "      " ledger-capf-acct-currency-symbol))
                               (re-search-backward ledger-capf-acct-currency-symbol))))
        ,@cape-legder--properties))))

(cl-pushnew '(ledger-capf (styles orderless)) completion-category-overrides)


(provide 'ledger-capf)
;;; ledger-capf.el ends here
