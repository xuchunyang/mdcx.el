;;; mdcx.el --- Markdown Code block Executor         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Created: 2020-02-27
;; Homepage: https://github.com/xuchunyang/mdcx.el
;; Package-Requires: ((emacs "25") (markdown-mode "2.3"))
;; Version: 0

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

;; XXX add something here

;;; Code:

(require 'markdown-mode)

(defun mdcx-code-block-at-pos (&optional pos)
  (pcase (markdown-get-enclosing-fenced-block-construct pos)
    ((and `(,code-begin ,code-end . ,_)
          (let lang-begin
            (progn (goto-char code-begin)
                   (line-beginning-position 2)))
          (let lang-end
            (progn (goto-char code-end)
                   (line-beginning-position)))
          (let (and (pred identity) lang)
            (markdown-code-block-lang)))
     (list lang lang-begin lang-end))))

(defun mdcx-next-result-block ()
  (catch 'result
    (while (not (eobp))
      (forward-line)
      (when (markdown-code-block-at-pos (point))
        (pcase (mdcx-code-block-at-pos (point))
          ((and `("result" . ,_) res)
           (throw 'result res))
          (_
           (throw 'result nil)))))))

(defun mdcx-insert-result (result)
  (pcase (save-excursion (mdcx-next-result-block))
    ('nil
     (insert (format "\n\n```result\n%s\n```\n" result)))
    (`(,_ ,b ,e)
     (delete-region b e)
     (goto-char b)
     (insert result))))

(defun mdcx-run-shell (src)
  (shell-command-to-string src))

(defun mdcx-run (lang src)
  (let ((fun (intern-soft (format "mdcx-run-%s" lang))))
    (if (fboundp fun)
        (funcall fun src)
      (user-error "No idea how to run %s code, defun `%s' to support it"
                  lang fun))))

(defun mdcx ()
  (interactive)
  (pcase (save-excursion (mdcx-code-block-at-pos))
    (`(,lang ,lang-begin ,lang-end)
     (save-excursion
       (goto-char lang-end)
       (goto-char (line-end-position))
       (mdcx-insert-result
        (mdcx-run lang
                  (buffer-substring-no-properties
                   lang-begin (1- lang-end))))))
    (_ (user-error "Not inside a GFM fenced code block"))))

(provide 'mdcx)
;;; mdcx.el ends here
