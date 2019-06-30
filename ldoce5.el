;;; ldoce5.el --- Longman Dictionary of Contemporary English  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/ldoce5.el
;; Version: 0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs interface to LDOCE5.

;;; Code:

(require 'subr-x)
(require 'dom)

(defconst ldoce5--load-dir (file-name-directory
                            (or load-file-name buffer-file-name))
  "The directory where this file lives.")

(defvar ldoce5-python-interpreter "python3"
  "The Python 3 interpreter.")

(defun ldoce5--search (word)
  "Search WORD and return parsed XML."
  (pcase (assoc word (ldoce5--list))
    (`(,_ ,_pos ,location)
     (with-temp-buffer
       (if (zerop (call-process
                   ldoce5-python-interpreter nil t nil
                   (expand-file-name "ldoce5.py" ldoce5--load-dir)
                   "lookup" location))
           (libxml-parse-xml-region (point-min) (point-max))
         (error "ldoce5--search failed: %s" (buffer-string)))))
    (_ (user-error "%s is not known" word))))

(defun ldoce5--find (location)
  (with-temp-buffer
    (if (zerop (call-process
                ldoce5-python-interpreter nil t nil
                (expand-file-name "ldoce5.py" ldoce5--load-dir)
                "lookup" location))
        (libxml-parse-xml-region (point-min) (point-max))
      (error "ldoce5--find failed: %s" (buffer-string)))))

(defvar ldoce5--list nil)

(defun ldoce5--list ()
  (unless ldoce5--list
    (message "[LDOCE5] Building word list...")
    (setq ldoce5--list
          (mapcar
           (lambda (line)
             (split-string line "|"))
           (process-lines ldoce5-python-interpreter
                          (expand-file-name "ldoce5.py" ldoce5--load-dir)
                          "list"))))
  ldoce5--list)

(defun ldoce5--Head (head)
  "Format <Head>."
  (let ((HYPHENATION (string-trim (dom-text (dom-child-by-tag (dom-child-by-tag head 'HWD) 'BASE))))
        (HOMNUM      (string-trim (dom-text (dom-child-by-tag head 'HOMNUM))))
        (PronCodes   (string-trim (dom-texts (dom-child-by-tag head 'PronCodes) "")))
        (POS (mapconcat (lambda (dom)
                          (string-trim (dom-texts dom "")))
                        (dom-by-tag head 'POS)
                        ", ")))
    (setq HYPHENATION HYPHENATION
          HOMNUM (if (string-empty-p HOMNUM)
                     nil
                   (propertize (propertize HOMNUM 'display '((height 0.8) (raise 0.5)))))
          PronCodes (if (string-empty-p PronCodes)
                        nil
                      PronCodes)
          POS (if (string-empty-p POS)
                  nil
                POS))
    (string-join (delq nil (list (concat HYPHENATION HOMNUM) PronCodes POS)) " ")))

(defun ldoce5--Sense (sense)
  (with-temp-buffer
    (insert (propertize (dom-text (car (dom-by-class sense "sensenum")))
                        'face '(:foreground "blue")))
    (when-let ((dom (dom-child-by-tag sense 'Inflections)))
      (insert " " (string-trim (dom-texts dom ""))))
    (when-let ((dom (dom-child-by-tag sense 'GRAM)))
      (insert " " (propertize
                   (string-trim (dom-texts dom ""))
                   'face '(:foreground "green yellow"))))
    (when-let ((dom (dom-child-by-tag sense 'SIGNPOST)))
      (insert " " (propertize (car (cddr dom)) 'face '(:background "forest green"))))
    (when-let ((dom (dom-child-by-tag sense 'LEXUNIT)))
      (insert " " (propertize (string-trim (dom-texts dom ""))
                              'face '(:background "dark green"))))
    (when-let ((dom (dom-child-by-tag sense 'DEF)))
      (insert " " (string-trim (dom-texts dom ""))))
    (insert "\n")
    (dolist (dom (dom-by-tag sense 'EXAMPLE))
      (insert " " (dom-texts (dom-child-by-tag dom 'BASE) "") "\n"))
    (buffer-string)))

(defun ldoce5--read-word ()
  (let* ((default (cond ((use-region-p)
                         (buffer-substring (region-beginning) (region-end)))
                        (t
                         (thing-at-point 'word))))
         (prompt (if default
                     (format "LDOCE5 Lookup (default %s): " default)
                   "LDOCE5 Lookup: ")))
    (completing-read prompt (ldoce5--list) nil t nil nil default)))

(defun ldoce5--display (dom)
  (with-current-buffer (get-buffer-create "*LDOCE5*")
    (erase-buffer)
    (insert (ldoce5--Head (dom-child-by-tag dom 'Head)) "\n")
    (insert (string-join (mapcar #'ldoce5--Sense (dom-by-tag dom 'Sense)) "\n"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun ldoce5-lookup (word)
  (interactive (list (ldoce5--read-word)))
  (let ((dom (ldoce5--search word)))
    (ldoce5--display dom)))

(defvar ldoce5-helm--list nil
  "Internal cache variable for ldoce5 words.")

(declare-function helm "helm")
(declare-function helm-make-source "helm-source")

;;;###autoload
(defun ldoce5-helm ()
  (interactive)
  (require 'helm)
  (unless ldoce5-helm--list
    (setq ldoce5-helm--list
          (mapcar
           (pcase-lambda (`(,word ,pos ,location))
             (cons (format "%-20s %s" word pos)
                   location))
           (ldoce5--list))))
  (helm
   :sources
   (helm-make-source "LDOCE5" 'helm-source-sync
     :candidates ldoce5-helm--list
     :action (lambda (location) (ldoce5--display (ldoce5--find location))))))

(provide 'ldoce5)
;;; ldoce5.el ends here
