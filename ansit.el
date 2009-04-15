;;; ansit.el --- ansi it. Make fotified region into ansi color format.

;; Copyleft (C) crazycool@SMTH

;; Author: crazycool <crazycool@SMTH>
;; Maintainer: crazycool <crazycool@SMTH>
;; Location: http://emacs.mysmth.org
;; Version: 1.0
;; Keywords: extensions ansi color

;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Modifiedy by: William Xu <william.xwl@gmail.com>
;; 
;; - Double original `' (only with this can qterm work)
;; - Add `ansit-kill-ring-save'
;; - Redefine `ansit-ansify-this'
;; - Add `ansit-color-close'

;;; Commentary:
;; M-x load-library <RET> ansit <RET>
;; M-x ansit <RET> OR M-x ansit-buffer <RET>
;;
;; command `ansit' will make region between mark and point into
;; an ansi-colored string and copy it (add it to kill ring).
;;
;; command `ansit-buffer' will make region between mark and point
;; into an ansi-colored string and preview it in a new buffer. Preview
;; is done by ansi-color.el, so if you don't have ansi-color installed,
;; no preview appears. `C-x k' or `C-f4' will kill the preview buffer and
;; copy the result into the kill ring.
;;
;; to specify your own colors (take ansit-builtin-color for example)
;; add this to your .emacs:
;; (require 'ansit)
;; (setq ansit-builtin-color ansit-color-magenta)
;;
;; all colors are listed below
;; font-lock color                ansit-color                 default
;; ------------------------------------------------------------------
;; font-lock-builtin-face         ansit-builtin-color         magenta
;; font-lock-comment-face         ansit-comment-color         yellow
;; font-lock-constant-face        ansit-constant-color        red
;; font-lock-doc-face             ansit-doc-color             magenta
;; font-lock-function-name-face   ansit-function-name-color   blue
;; font-lock-keyword-face         ansit-keyword-color         cyan
;; font-lock-preprocessor-face    ansit-preprocessor-color    magenta
;; font-lock-string-face          ansit-string-color          magenta
;; font-lock-type-face            ansit-type-color            green
;; font-lock-variable-name-face   ansit-variable-name-color   green
;; font-lock-warning-face         ansit-warning-color         red

;;; Code:

(require 'ansi-color nil t)

(defvar ansit-color-black         "[1;30m")
(defvar ansit-color-red           "[1;31m")
(defvar ansit-color-green         "[1;32m")
(defvar ansit-color-yellow        "[1;33m")
(defvar ansit-color-blue          "[1;34m")
(defvar ansit-color-magenta       "[1;35m")
(defvar ansit-color-cyan          "[1;36m")
(defvar ansit-color-white         "[1;37m")
(defvar ansit-color-close         "[m")

(defvar ansit-builtin-color       "[1;35m")
(defvar ansit-comment-color       "[1;33m")
(defvar ansit-constant-color      "[1;31m")
(defvar ansit-doc-color           "[1;35m")
(defvar ansit-function-name-color "[1;34m")
(defvar ansit-keyword-color       "[1;36m")
(defvar ansit-preprocessor-color  "[1;35m")
(defvar ansit-string-color        "[1;35m")
(defvar ansit-type-color          "[1;32m")
(defvar ansit-variable-name-color "[1;32m")
(defvar ansit-warning-color       "[1;31m")

(defvar ansit-result-string ""
  "Do not set this variable.")

(defalias 'ansit 'ansit-ansify-this)
(defalias 'ansit-kill-ring-save 'ansit-ansify-this)
(defun ansit-ansify-this (beg end)
  "Fontify region between mark and point, add the result to kill ring."
  (interactive "r")
  (let ((s (ansit-ansify-region beg end)))
    (kill-new s)
    (deactivate-mark)
    s))

(defalias 'ansit-buffer 'ansit-ansify-this-in-new-buffer)
(defun ansit-ansify-this-in-new-buffer ()
  "Fontify region between mark and point, view result in new buffer."
  (interactive)
  (unless (= (mark) (point))
    (setq ansit-result-string (ansit-ansify-region (mark) (point)))
    (with-current-buffer (get-buffer-create "*Ansit*")
      (insert ansit-result-string)
      (when (locate-library "ansi-color")
        (insert "\n\n!!---------------- [preview] ----------------!!\n\n")
        (insert  (ansi-color-apply ansit-result-string)))
      (local-set-key "\C-xk" 'ansit-copy-and-kill-buffer)
      (local-set-key [C-f4]  'ansit-copy-and-kill-buffer)
      (switch-to-buffer (current-buffer)))))

(defun ansit-copy-and-kill-buffer ()
  "Copy result and kill the preview buffer."
  (interactive)
  (kill-new ansit-result-string)
  (kill-buffer (current-buffer))
  (message "Buffer killed. result copied."))

(defun ansit-ansify-region (begin-point end-point)
  "Fontify region between BEGIN-POINT and END-POINT
with ANSI color. Return the result in string."
  (let ((beg (min begin-point end-point))
        (end (max begin-point end-point))
        (tmp nil)
        (str "")
        (result nil)
        (tface nil)
        (color "")
        )
    (unless (= beg end)
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (setq tmp (next-single-property-change (point) 'face))
          ;; buffer-end
          (unless tmp
            (setq tmp end))
          ;; no cross-line properties
          (when (> tmp (line-end-position))
            (setq tmp (+ 1 (line-end-position))))
          ;; skip spaces and tabs
          (save-excursion
            (goto-char tmp)
            (when (looking-at "[ \t]+")
              (re-search-forward "[ \t]+" (line-end-position) t)
              (setq tmp (point))))
          (when (> tmp end)
            (setq tmp end))
          (setq str (buffer-substring-no-properties (point) tmp))
          ;; filter ansi controls
          (while (string-match "" str)
            (setq str (replace-match "^[" t nil str)))
          (setq tface (get-text-property (point) 'face))
          (when (listp tface)
            (setq tface (car tface)))
          (cond
           ((eq tface font-lock-builtin-face)
            (setq color ansit-builtin-color))
           ((eq tface font-lock-comment-face)
            (setq color ansit-comment-color))
           ((eq tface font-lock-constant-face)
            (setq color ansit-constant-color))
           ((eq tface font-lock-doc-face)
            (setq color ansit-doc-color))
           ((eq tface font-lock-function-name-face)
            (setq color ansit-function-name-color))
           ((eq tface font-lock-keyword-face)
            (setq color ansit-keyword-color))
           ((eq tface font-lock-preprocessor-face)
            (setq color ansit-preprocessor-color))
           ((eq tface font-lock-string-face)
            (setq color ansit-string-color))
           ((eq tface font-lock-type-face)
            (setq color ansit-type-color))
           ((eq tface font-lock-variable-name-face)
            (setq color ansit-variable-name-color))
           ((eq tface font-lock-warning-face)
            (setq color ansit-warning-color))
           (t (setq color "[1;37m")))
          ;;(setq result (concat result color str "[m"))
          (setq result (concat result color str))
          (goto-char tmp))
        (concat result "[m")
        result))))

(provide 'ansit)

;;; ansit.el ends here
