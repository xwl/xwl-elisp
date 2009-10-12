;;; xwl-subr.el --- Some scheme extensions for Emacs

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
;;           (require 'xwl-subr)

;; FIXME: elisp doesn't support tail recursive, does it?

;;; Code:

(defun xs-flatmap (procedure sequence)
  "map and append. e.g., map: '((a b) (c)) => flatmap: '(a b c)."
  (xs-accumulate 'append '() (mapcar procedure sequence)))

(defun xs-accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (xs-accumulate op initial (cdr sequence)))))

;; ;; These two are useful for customizing, so that the result is not affected even
;; ;; being evaled for multiple times. 
;; (defun xs-uniq-prepend (&rest sequences)
;;   (let ((r (nreverse sequences)))
;;     (unless (symbolp (car r))
;;       (error "Last element should be a symbol"))
;;     (mapc (lambda (path) (add-to-list (car r) path))
;;           (xs-flatmap 'identity (cdr r)))
;;     (symbol-value (car r))))

;; (defun xs-uniq-append (&rest sequences)
;;   (let ((r (nreverse sequences)))
;;     (unless (symbolp (car r))
;;       (error "Last element should be a symbol"))
;;     (mapc (lambda (path) (add-to-list (car r) path t))
;;           (xs-flatmap 'identity (cdr r)))
;;     (symbol-value (car r))))

(defun xs-find-first (predicate sequence)
  (cond ((null sequence)
         nil)
        ((funcall predicate (car sequence))
         (car sequence))
        (t
         (xs-find-first predicate (cdr sequence)))))


(provide 'xwl-subr)

;;; xwl-subr.el ends here
