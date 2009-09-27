;;; scheme-extensions.el --- Some scheme extensions for Emacs

;; Copyright (C) 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'scheme-extensions)

;; FIXME: elisp doesn't support tail recursive, does it?

;;; Code:

(defun se-flatmap (procedure sequence)
  "map and append. e.g., map: '((a b) (c)) => flatmap: '(a b c)."
  (se-accumulate 'append '() (mapcar procedure sequence)))

(defun se-accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (se-accumulate op initial (cdr sequence)))))

(provide 'scheme-extensions)

;;; scheme-extensions.el ends here
