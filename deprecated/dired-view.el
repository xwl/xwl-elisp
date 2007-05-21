;;; dired-view.el --- dired view mode

;; Copyright (C) 2006, 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.2
;; Last updated: 2007/05/21 16:45:29

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

;;; Commentary:

;; When browsing files in dired buffer, it would be convenient to be
;; able to jump to a file by typing that filename's first
;; character. This is what this extension does.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;       (require 'dired-search)
;;
;; To enable it by default,
;;
;;       (add-hook 'dired-mode-hook 'dired-search-minor-mode-on)
;;
;; Also, you could define keys to toggle it,
;;
;;       (define-key dired-mode-map (kbd ";") 'dired-search-minor-mode-toggle)
;;       (define-key dired-mode-map (kbd ":") 'dired-search-minor-mode-dired-toggle)

;;; Code:

(define-minor-mode dired-isearch-minor-mode
  "Toggle dired-isearch-minor-mode.

With dired-isearch-minor-mode enabled, you could jump to files based on
filenames' first character.
\\{dired-isearch-minor-mode-map}."
  nil " Dired-Isearch"
  '((" a" . (lambda () (interactive) (dired-isearch-jump "a")))
    (" b" . (lambda () (interactive) (dired-isearch-jump "b")))
    (" c" . (lambda () (interactive) (dired-isearch-jump "c")))
    (" d" . (lambda () (interactive) (dired-isearch-jump "d")))
    (" e" . (lambda () (interactive) (dired-isearch-jump "e")))
    (" f" . (lambda () (interactive) (dired-isearch-jump "f")))
    (" g" . (lambda () (interactive) (dired-isearch-jump "g")))
    (" h" . (lambda () (interactive) (dired-isearch-jump "h")))
    (" i" . (lambda () (interactive) (dired-isearch-jump "i")))
    (" j" . (lambda () (interactive) (dired-isearch-jump "j")))
    (" k" . (lambda () (interactive) (dired-isearch-jump "k")))
    (" l" . (lambda () (interactive) (dired-isearch-jump "l")))
    (" m" . (lambda () (interactive) (dired-isearch-jump "m")))
    (" n" . (lambda () (interactive) (dired-isearch-jump "n")))
    (" o" . (lambda () (interactive) (dired-isearch-jump "o")))
    (" p" . (lambda () (interactive) (dired-isearch-jump "p")))
    (" q" . (lambda () (interactive) (dired-isearch-jump "q")))
    (" r" . (lambda () (interactive) (dired-isearch-jump "r")))
    (" s" . (lambda () (interactive) (dired-isearch-jump "s")))
    (" t" . (lambda () (interactive) (dired-isearch-jump "t")))
    (" u" . (lambda () (interactive) (dired-isearch-jump "u")))
    (" v" . (lambda () (interactive) (dired-isearch-jump "v")))
    (" w" . (lambda () (interactive) (dired-isearch-jump "w")))
    (" x" . (lambda () (interactive) (dired-isearch-jump "x")))
    (" y" . (lambda () (interactive) (dired-isearch-jump "y")))
    (" z" . (lambda () (interactive) (dired-isearch-jump "z")))
    (" A" . (lambda () (interactive) (dired-isearch-jump "A")))
    (" B" . (lambda () (interactive) (dired-isearch-jump "B")))
    (" C" . (lambda () (interactive) (dired-isearch-jump "C")))
    (" D" . (lambda () (interactive) (dired-isearch-jump "D")))
    (" E" . (lambda () (interactive) (dired-isearch-jump "E")))
    (" F" . (lambda () (interactive) (dired-isearch-jump "F")))
    (" G" . (lambda () (interactive) (dired-isearch-jump "G")))
    (" H" . (lambda () (interactive) (dired-isearch-jump "H")))
    (" I" . (lambda () (interactive) (dired-isearch-jump "I")))
    (" J" . (lambda () (interactive) (dired-isearch-jump "J")))
    (" K" . (lambda () (interactive) (dired-isearch-jump "K")))
    (" L" . (lambda () (interactive) (dired-isearch-jump "L")))
    (" M" . (lambda () (interactive) (dired-isearch-jump "M")))
    (" N" . (lambda () (interactive) (dired-isearch-jump "N")))
    (" O" . (lambda () (interactive) (dired-isearch-jump "O")))
    (" P" . (lambda () (interactive) (dired-isearch-jump "P")))
    (" Q" . (lambda () (interactive) (dired-isearch-jump "Q")))
    (" R" . (lambda () (interactive) (dired-isearch-jump "R")))
    (" S" . (lambda () (interactive) (dired-isearch-jump "S")))
    (" T" . (lambda () (interactive) (dired-isearch-jump "T")))
    (" U" . (lambda () (interactive) (dired-isearch-jump "U")))
    (" V" . (lambda () (interactive) (dired-isearch-jump "V")))
    (" W" . (lambda () (interactive) (dired-isearch-jump "W")))
    (" X" . (lambda () (interactive) (dired-isearch-jump "X")))
    (" Y" . (lambda () (interactive) (dired-isearch-jump "Y")))
    (" Z" . (lambda () (interactive) (dired-isearch-jump "Z")))
    (" 1" . (lambda () (interactive) (dired-isearch-jump "1")))
    (" 2" . (lambda () (interactive) (dired-isearch-jump "2")))
    (" 3" . (lambda () (interactive) (dired-isearch-jump "3")))
    (" 4" . (lambda () (interactive) (dired-isearch-jump "4")))
    (" 5" . (lambda () (interactive) (dired-isearch-jump "5")))
    (" 6" . (lambda () (interactive) (dired-isearch-jump "6")))
    (" 7" . (lambda () (interactive) (dired-isearch-jump "7")))
    (" 8" . (lambda () (interactive) (dired-isearch-jump "8")))
    (" 9" . (lambda () (interactive) (dired-isearch-jump "9")))
    (" 0" . (lambda () (interactive) (dired-isearch-jump "0"))))
  (setq dired-isearch-last-arg "")
  (setq dired-isearch-last-arg-count 0))

(defvar dired-isearch-last-arg ""
  "Last isearched arg.")

(defvar dired-isearch-last-arg-count 0
  "How many times we've isearched a same arg till now.")

(defun dired-isearch (arg)
  "isearch Jump to filename startting with ARG."
  (interactive)
  (let ((old-arg dired-isearch-last-arg)
        (old-count dired-isearch-last-arg-count))
    (unless (string-equal dired-isearch-last-arg arg)
      (setq dired-isearch-last-arg-count 0
            dired-isearch-last-arg arg))
    (let* ((count dired-isearch-last-arg-count)
           (filename
            (catch 'return
              (progn
                (mapc
                 (lambda (name)
                   (when (string-equal arg (substring name 0 1))
                     (if (zerop count)
                         (throw 'return name)
                       (setq count (1- count)))))
                 ;; (directory-files (dired-current-directory))
                 (remove
                  ""
                  (split-string
                   (shell-command-to-string
                    (concat "ls "       ; possible caveats here
                            (replace-regexp-in-string
                             "l" "" dired-listing-switches)))
                   "\n")))
                nil))))
      (cond (filename                   ; success
             (goto-char (point-min))
             (isearch-forward filename nil t)
             (backward-char (length (match-string 0)))
             (when (string-equal dired-isearch-last-arg arg)
               (setq dired-isearch-last-arg-count
                     (1+ dired-isearch-last-arg-count))))
            ((and (zerop count)         ; wrap around
                  (> dired-isearch-last-arg-count 0))
             (setq dired-isearch-last-arg ""
                   dired-isearch-last-arg-count 0)
             (dired-isearch arg))
            (t                          ; not found
             (setq dired-isearch-last-arg old-arg
                   dired-isearch-last-arg-count old-count)
             (message "file not found"))))))

(provide 'dired-isearch)

;;; dired-isearch.el ends here
