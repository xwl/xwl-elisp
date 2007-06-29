;;; qterm.el --- qterm replying editor hook

;; Copyright (C) 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.22

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'qterm)

;;; Code:
(require 'ansit)

(defvar qterm-faces '("朗声" "鬼叫" "喃喃" "轻声" "一声大喝" "大叫" 
                      "柔柔" "哭着" "大吼"))

(defvar qterm-log-file "/tmp/qterm.log")

(defvar qterm-signature ""
  "Could be either a string or a lisp function returning a string.")

(defun qterm-reply-hook ()
  (when (string= (buffer-file-name) qterm-log-file)
    (let ((inhibit-read-only t))
      ;; quote
      (goto-char (point-max))
      (delete-blank-lines)
      (goto-char (point-min))
      (insert "> ")
      (while (zerop (forward-line 1))
        (insert "> "))
      ;; author title
      (let ((author "")
            (face (nth (random (length qterm-faces)) qterm-faces)))
        (goto-char (point-min))
        (re-search-forward "[a-zA-Z0-9]+.*)"
                           (save-excursion (end-of-line)
                                           (point))
                           t
                           1)
        (setq author (match-string 0))
        (goto-char (point-min))
        (kill-line 4)
        (insert (format "%s[ %s %s道: ]%s\n\n" 
                        ansit-color-yellow author face ansit-color-close)))
      ;; qmd
      (goto-char (point-max))
      (cond ((stringp qterm-signature)
             (insert qterm-signature))
            (t
             (insert (funcall qterm-signature))))
      (goto-char (point-min)))))

(add-hook 'find-file-hook 'qterm-reply-hook)

(provide 'qterm)

;;; qterm.el ends here
