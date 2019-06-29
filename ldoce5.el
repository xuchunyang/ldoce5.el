;;; ldoce5.el --- Longman Dictionary of Contemporary English  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: english dictionary

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

(defconst ldoce5--load-dir (file-name-directory
                            (or load-file-name buffer-file-name))
  "The directory where this file lives.")

(defvar ldoce5-python-interpreter "python3"
  "The Python 3 interpreter.")

(defun ldoce5--search (word)
  "Search WORD and return parsed XML."
  (with-temp-buffer
    (if (zerop (call-process
                ldoce5-python-interpreter nil t nil
                (expand-file-name "search.py" ldoce5--load-dir) word))
        (libxml-parse-xml-region (point-min) (point-max))
      (error "ldoce5--search failed: %s" (buffer-string)))))

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
    (when-let ((dom (dom-child-by-tag sense 'GRAM)))
      (insert " " (propertize
                   (string-trim (dom-texts dom ""))
                   'face '(:foreground "green yellow"))))
    (when-let ((dom (dom-child-by-tag sense 'SIGNPOST)))
      (insert " " (propertize (caddr dom) 'face '(:background "forest green"))))
    (when-let ((dom (dom-child-by-tag sense 'LEXUNIT)))
      (insert " " (propertize (caddr dom) 'face '(:background "dark green"))))
    (when-let ((dom (dom-child-by-tag sense 'DEF)))
      (insert " " (dom-texts dom "")))
    (insert "\n")
    (dolist (dom (dom-by-tag sense 'EXAMPLE))
      (insert " " (dom-texts (dom-child-by-tag dom 'BASE) "") "\n"))
    (buffer-string)))

(defun ldoce5-lookup (word)
  (interactive "sWord: ")
  (let ((dom (ldoce5--search word)))
    (with-current-buffer (get-buffer-create "*LDOCE5*")
      (erase-buffer)
      (insert (ldoce5--Head (dom-child-by-tag dom 'Head)) "\n")
      (insert (string-join (mapcar #'ldoce5--Sense (dom-by-tag dom 'Sense)) "\n"))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'ldoce5)
;;; ldoce5.el ends here
