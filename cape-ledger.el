;;; cape-ledger.el --- arx cape ledger config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 22, 2021
;; Modified: December 22, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/cape-ledger
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

(defvar cape--ledger-properties
  (list :annotation-function (lambda (_) " Ledger")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `cape-ledger'.")

(defcustom cape-ledger-acct-currency-symbol ""
  "Currency symbol to be appended to the end of the account name."
  :type 'string
  :group 'cape-ledger-acct)

(defcustom cape-ledger-acct-master-file "~/.ledgerrc"
  "Location of the file containing account definitions."
  :type 'file
  :group 'cape-ledger-acct)

(defun cape-ledger-acct--regexp-filter (regexp list type)
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
(defun cape-ledger-acct--get-all-accts ()
  "Read the master file that has the account descriptions."
  (cape-ledger-acct--regexp-filter
   "^account \\(.*\\)"
   (mapcar #'(lambda (s)  s)
           (with-temp-buffer
             (insert-file-contents cape-ledger-acct-master-file)
             (split-string (buffer-string) "\n" t)))
   "account"))

;;;###autoload
(defun cape-ledger-acct--get-all-payees ()
  "Read the master file that has the payee descriptions."
  (cape-ledger-acct--regexp-filter
   "^payee \\(.*\\)"
   (mapcar #'(lambda (s)  s)
           (with-temp-buffer
             (insert-file-contents cape-ledger-acct-master-file)
             (split-string (buffer-string) "\n" t)))
   "payee"))

(defun cape-ledger-acct--fuzzy-word-match (prefix candidate)
  "Return non-nil if each (partial) word in PREFIX is also in CANDIDATE."
  (eq nil
      (memq nil
            (mapcar
             #'(lambda (pre) (string-match-p (regexp-quote pre) candidate))
             (split-string prefix)))))

(defun cape-ledger-acct--get-payees-p (arg)
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

(defun cape-ledger-acct--get-accts-p (arg)
  "Determine whether the user should be prompted for accounts.
If the only characters preceeding the ARG are spaces, then the
user is likely to be entering account information."
  (interactive)
  (if (string-match-p "^[ \t\r\f\v]*$"
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (- ( point ) (length arg))))
      t nil))

;;;###autoload
(defun cape-ledger--accts (str)
  "Cape backend to prompt for account names."
  (if (cape-ledger-acct--get-payees-p str)
      (cape-ledger-acct--get-all-payees)
    (if  (cape-ledger-acct--get-accts-p str)
        (cape-ledger-acct--get-all-accts))))

;;;###autoload
(defun cape-ledger (&optional interactive)
  "Complete with Ledger at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-ledger)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties
          (cape--cached-table-props (car bounds) (cdr bounds) #'cape-ledger--accts 'substring)
          :category 'cape-ledger)
        :exclusive no
        :exit-function (lambda (x _status)
                         (if (string= "account" (get-text-property 0 'type x))
                             (progn
                               (delete-region(+ (line-beginning-position) (current-indentation)) (point))
                               (insert (concat x "      " cape-ledger-acct-currency-symbol))
                               (re-search-backward cape-ledger-acct-currency-symbol))))
        ,@cape--ledger-properties))))

(cl-pushnew '(cape-ledger (styles orderless)) completion-category-overrides)


(provide 'cape-ledger)
;;; cape-ledger.el ends here
