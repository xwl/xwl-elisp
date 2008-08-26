;;; chicken-scheme-extras.el --- Non-standard macros and special forms support

;; Copyright (C) 2008 William Xu

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

;; Non-standard macros and special forms:
;;
;;    http://chicken.wiki.br/Non-standard%20macros%20and%20special%20forms
;;
;; Add highlighting and proper indention support.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'chicken-scheme-extras)

;;; Code:

(require 'scheme)

;;; Indention

;; define style
(defvar chicken-scheme-extra-define
  '(define-extension define-values define-constant define-inline
     define-macro define-for-syntax define-record define-record-printer
     define-record-type))

(mapc (lambda (i)
        (put i 'scheme-doc-string-elt
             (lambda ()
               ;; The function is called with point right after "define".
               (forward-comment (point-max))
               (if (eq (char-after) ?\() 2 0))))
      chicken-scheme-extra-define)

;; let style
(defvar chicken-scheme-extra-let
  '(when unless  case-lambda  let-optionals let-optionals* and-let* rec
         let-values let*-values letrec-values parameterize receive
         select))

(mapc (lambda (i)
        (put i 'scheme-indent-function 1))
      chicken-scheme-extra-let)

;; if
(put 'if 'scheme-indent-function 2)

;; none
(defvar chicken-scheme-extra-none
  '(optional cut set!-values assert cond-expand eval-when ensure
             include nth-value time use))


;;; Highlighting

(font-lock-add-keywords
 'scheme-mode
 `((,(concat "(\\<\\("
             (regexp-opt
              (mapcar 'symbol-name
                      (append chicken-scheme-extra-define
                              chicken-scheme-extra-let
                              chicken-scheme-extra-none)))
             "\\)\\>")
    1 'font-lock-keyword-face nil t)))

(provide 'chicken-scheme-extras)

;;; chicken-scheme-extras.el ends here
