;;; dired-isearch.el --- isearch in Dired

;; Copyright (C) 2006, 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Do isearch in Dired but match only at file names.
;;
;; Recommended keybindings:
;;
;;  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
;;  (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
;;  (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
;;  (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)

;;; Bugs:

;; - search string starting with a "^" causes some infinite loop
;; - search string with ".*" matches at non-filenames

;;; Code:

(require 'dired)

;;; User Interfaces

;;;###autoload
(defun dired-isearch-forward (&optional regexp-p no-recursive-edit)
  "In Dired, run `isearch-forward' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-forward regexp-p no-recursive-edit)))

;;;###autoload
(defun dired-isearch-backward (&optional regexp-p no-recursive-edit)
  "In Dired, run `isearch-backward' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-backward regexp-p no-recursive-edit)))

;;;###autoload
(defun dired-isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  "In Dired, run `isearch-forward-regexp' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-forward-regexp not-regexp no-recursive-edit)))

;;;###autoload
(defun dired-isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  "In Dired, run `isearch-backward-regexp' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-backward-regexp not-regexp no-recursive-edit)))


;;; Low Level Functions

;;  by Stefan Monnier

;; (defun dired-isearch-search-fun-function () 'dired-isearch-search)

;; (defun dired-isearch-search (&rest args)
;;   (let ((fun (let ((isearch-search-fun-function nil))
;;                (isearch-search-fun)))
;;         point)
;;     (while (and (setq point (apply fun args))
;;                 ;; Use `help-echo' instead of `dired-filename' so as to
;;                 ;; also work in Tramp dired buffers.
;;                 (not (get-text-property (1- point) 'help-echo)))
;;       ;; (forward-char 1)
;;       )
;;     point))

;; (define-minor-mode dired-isearch-filenames-mode
;;   "Only match filenames in isearch."
;;   :global nil
;;   (if dired-isearch-filenames-mode
;;       (set (make-local-variable 'isearch-search-fun-function)
;;            'dired-isearch-search-fun-function)
;;     (kill-local-variable 'isearch-search-fun-function)))

(defun dired-isearch-search-fun-function ()
  "Return the isearch function in Dired."
  (cond
       (isearch-word
        (if isearch-forward 'dired-word-search-forward 'dired-word-search-backward))
       (isearch-regexp
        (if isearch-forward 'dired-re-search-forward 'dired-re-search-backward))
       (t
        (if isearch-forward 'dired-search-forward 'dired-search-backward))))

(defun dired-search-forward (string &optional bound noerror count)
  "In Dired, run `search-forward' but match only at file names."
  (let ((point (search-forward string bound noerror count)))
    ;; Use 'help-echo instead of 'dired-filename so as to also work
    ;; in tramp dired buffers.
    (while (and point (not (get-text-property (1- point) 'help-echo)))
      (setq point (search-forward string bound noerror count)))
    point))

(defun dired-search-backward (string &optional bound noerror count)
  "In Dired, run `search-backward' but match only at file names."
  (let ((point (search-backward string bound noerror count)))
    (while (and point (not (get-text-property point 'help-echo)))
        (setq point (search-backward string bound noerror count)))
    point))

(defun dired-re-search-forward (regexp &optional bound noerror count)
  "In Dired, run `re-search-forward' but match only at file names."
  (let ((point (re-search-forward regexp bound noerror count)))
    (while (and point (not (get-text-property (1- point) 'help-echo)))
      (setq point (re-search-forward regexp bound noerror count)))
    point))

(defun dired-re-search-backward (regexp &optional bound noerror count)
  "In Dired, run `re-search-backward' but match only at file names."
  (let ((point (re-search-backward regexp bound noerror count)))
    (while (and point (not (get-text-property point 'help-echo)))
      (setq point (re-search-backward regexp bound noerror count)))
    point))

(defun dired-word-search-forward (string &optional bound noerror count)
  "In Dired, run `word-search-forward' but match only at file names."
  (let ((point (word-search-forward string bound noerror count)))
    (while (and point (not (get-text-property (1- point) 'help-echo)))
      (setq point (word-search-forward string bound noerror count)))
    point))

(defun dired-word-search-backward (string &optional bound noerror count)
  "In Dired, run `word-search-backward' but match only at file names."
  (let ((point (word-search-backward string bound noerror count)))
    (while (and point (not (get-text-property point 'help-echo)))
      (setq point (word-search-backward string bound noerror count)))
    point))

(provide 'dired-isearch)

;;; dired-isearch.el ends here
