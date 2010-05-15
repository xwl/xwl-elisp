;;; less.el --- less style view mode

;; Copyright (C) 2005, 2007, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(defcustom auto-less-exclude-regexp (regexp-opt
                                     '("*Org Agenda*" "*code-conversion-work*"))
  "Do not turn on `auto-less-minor-mode' for matched files and buffers."
  :type 'string
  :group 'convenience)

(defcustom auto-less-exclude-modes '(shell-mode term-mode comint-mode)
  "Do not turn on `auto-less-minor-mode' for these major modes."
  :type 'list
  :group 'convenience)

;;;###autoload
(define-global-minor-mode global-less-minor-mode
  less-minor-mode auto-less-minor-mode
  :group 'convenience)

;;;###autoload
(define-minor-mode less-minor-mode
  "Toggle less-minor-mode.

With less-minor-mode enabled, you could use `less' like keys to view files.
\\{less-minor-mode-map}."
  nil " Less"
  '(("j" . less-scroll-up-line)
    ("k" . less-scroll-down-line)
    ("f" . scroll-up)
    ("b" . scroll-down)
    ("g" . beginning-of-buffer)
    ("G" . end-of-buffer)
    (" " . scroll-up)
    ("" . scroll-down)
    ("e" . less-quit))
    (set (make-local-variable 'buffer-read-only) less-minor-mode))

;;;###autoload
(defun less-scroll-up-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun less-scroll-down-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun less-quit ()
  "Quit `less-minor-mode'."
  (interactive)
  (less-minor-mode -1))

;;;###autoload
(defun auto-less-minor-mode ()
  "Turn on `less-minor-mode' for files not matching `auto-less-exclude-regexp'.

This is a useful hook to add to `find-file-hook'."
  (unless (or (and (not (string= auto-less-exclude-regexp ""))
                   (string-match auto-less-exclude-regexp
                                 (or buffer-file-name (buffer-name))))
              (memq major-mode auto-less-exclude-modes)
              (minibufferp (current-buffer))
              (buffer-modified-p))
    (less-minor-mode 1)))

;;;###autoload
(defun less-minor-mode-on ()
  "Turn on `less-minor-mode'."
  (less-minor-mode 1))

;;;###autoload
(defun less-minor-mode-off ()
  "Turn off `less-minor-mode'."
  (less-minor-mode -1))

(provide 'less)

;;; less.el ends here
