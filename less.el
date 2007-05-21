;;; less.el --- less style view mode

;; Copyright (C) 2005, 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.2
;; Last updated: 2007/05/21 16:38:10

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

;; View file like a simple `less'. Provide limited less keys, mainly j,
;; k, f, b, g, G, etc.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'less)
;;
;; Then use `M-x less-minor-mode' to toggle `less-minor-mode'.

;; There's also a `view-less.el' in XEmacs. But it does too much for me,
;; i just wanna less keys like j, k, f, b, g, G, not to mess with other
;; keys in major mode.

;;; Code:

(define-minor-mode less-minor-mode
  "Toggle less-minor-mode.

With less-minor-mode enabled, you could use `less' like keys to view files.
\\{less-minor-mode-map}."
  nil " Less"
  '(("j" . less-scroll-up-one-line)
    ("k" . less-scroll-down-one-line)
    ("f" . scroll-up)
    ("b" . scroll-down)
    ("g" . beginning-of-buffer)
    ("G" . end-of-buffer)
    (" " . scroll-up)
    ("" . scroll-down)
    ("e" . less-quit))
    (set (make-local-variable 'buffer-read-only) less-minor-mode))

(defun less-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun less-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun less-quit ()
  "Quit `less-minor-mode'."
  (interactive)
  (less-minor-mode -1))

(defun auto-less-minor-mode ()
  "Auto enter `less-minor-mode' when visiting read-only files. You can
add this to `find-file-hooks'."
  (unless (file-writable-p buffer-file-name)
    (less-minor-mode 1)))

(defun less-minor-mode-on ()
  "Turn on `less-minor-mode'."
  (less-minor-mode 1))

(defun less-minor-mode-off ()
  "Turn off `less-minor-mode'."
  (less-minor-mode -1))

(provide 'less)

;;; less.el ends here
