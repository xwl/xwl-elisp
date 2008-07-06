;;; easy-todo.el --- Manage your todos in an extremely easy way!

;; Copyright (C) 2007, 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.4a
;; Url: http://williamxu.net9.org/ref/easy-todo.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `easy-todo-mode' is a very easy todo manager. It simply adds some
;; highlighted keywords, some special prefix flags(for indicating
;; different status of todo items) upon `text-mode'.

;; It supports four different prefix flags, namely, "^> "(ongoing),
;; "^- "(todo), "^| "(interrupted), "^o "(done). Here's a screenshot:
;;
;; ,----
;; | > (work) Check compress() function
;; | > (easy-todo) Finish easy-todo-mode
;; | | (emms) share music with iTunes user?
;; | o (misc) take a cup of tea
;; `----

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'easy-todo-mode "easy-todo")

;;; Code:

;;; Customizations

(defgroup easy-todo nil
  "Manage todos in an easy way!"
  :prefix "easy-todo-"
  :group 'convenience)

(defcustom easy-todo-ongoing-regexp "^> "
  "Prefix for ongoing items."
  :type 'string
  :group 'easy-todo)

(defcustom easy-todo-todo-regexp "^- "
  "Prefix for todo items."
  :type 'string
  :group 'easy-todo)

(defcustom easy-todo-interrupted-regexp "^| "
  "Prefix for interrupted items."
  :type 'string
  :group 'easy-todo)

(defcustom easy-todo-done-regexp "^o "
  "Prefix for finished items."
  :type 'string
  :group 'easy-todo)

;; ,----
;; | faces
;; `----
;; (defgroup easy-todo-faces nil
;;   "easy-todo status faces."
;;   :group 'easy-todo)

;; (defface easy-todo-ongoing-face '((t (:bold t :foreground "magenta")))
;;   "Face for `easy-todo-ongoing-regexp'."
;;   :group 'easy-todo-faces)

;; (defvar easy-todo-ongoing-face 'easy-todo-ongoing-face)

;; (defface easy-todo-todo-face '((t (:bold t :foreground "green")))
;;   "Face for `easy-todo-todo-regexp'."
;;   :group 'easy-todo-faces)

;; (defvar easy-todo-todo-face 'easy-todo-todo-face)

;; (defface easy-todo-interrupted-face '((t (:bold t :foreground "yellow")))
;;   "Face for `easy-todo-interrupted-regexp'."
;;   :group 'easy-todo-faces)

;; (defvar easy-todo-interrupted-face 'easy-todo-interrupted-face)

;; (defface easy-todo-done-face '((t (:bold t :foreground "purple")))
;;   "Face for `easy-todo-done-regexp'."
;;   :group 'easy-todo-faces)

;; (defvar easy-todo-done-face 'easy-todo-done-face)


;;; Interfaces

;;;###autoload
(define-derived-mode easy-todo-mode text-mode "Easy-Todo"
  "Major mode for managing todos.
\\{easy-todo-mode-map}"
  (setq font-lock-defaults '(easy-todo-font-lock-keywords))
  (run-hooks 'easy-todo-mode-hook))

(defvar easy-todo-regexp-face-alist
  `((,easy-todo-ongoing-regexp     . ,font-lock-constant-face) ; magenta
    (,easy-todo-todo-regexp        . ,font-lock-doc-face)      ; green
    (,easy-todo-interrupted-regexp . ,font-lock-variable-name-face) ; yellow
    (,easy-todo-done-regexp        . ,font-lock-function-name-face))) ; blue

(defvar easy-todo-regexps (mapcar 'car easy-todo-regexp-face-alist))

(defvar easy-todo-matcher (mapconcat 'identity easy-todo-regexps "\\|"))

(defvar easy-todo-font-lock-keywords
  (mapcar (lambda (regexp-face)
            `(,(concat (car regexp-face) ".*")
              (0 ,(cdr regexp-face) t t)))
          easy-todo-regexp-face-alist))

(define-key easy-todo-mode-map (kbd "C-c C-o") 'easy-todo-item-ongoing)
(define-key easy-todo-mode-map (kbd "C-c C-t") 'easy-todo-item-todo)
(define-key easy-todo-mode-map (kbd "C-c C-u") 'easy-todo-item-interrupted)
(define-key easy-todo-mode-map (kbd "C-c C-d") 'easy-todo-item-done)

(define-key easy-todo-mode-map (kbd "C-c C-b") 'easy-todo-sort-buffer)
(define-key easy-todo-mode-map (kbd "C-c C-r") 'easy-todo-sort-region)
(define-key easy-todo-mode-map (kbd "C-c C-k") 'easy-todo-kill-item)

(defun easy-todo-item-ongoing ()
  "Switch item into ongoing status."
  (interactive)
  (easy-todo-item-switch easy-todo-ongoing-regexp))

(defun easy-todo-item-todo ()
  "Switch item into todo status."
  (interactive)
  (easy-todo-item-switch easy-todo-todo-regexp))

(defun easy-todo-item-interrupted ()
  "Switch item into interrupted status."
  (interactive)
  (easy-todo-item-switch easy-todo-interrupted-regexp))

(defun easy-todo-item-done ()
  "Switch item into done status."
  (interactive)
  (easy-todo-item-switch easy-todo-done-regexp))


;;; Low Level Functions

(defun easy-todo-item-switch (regexp)
  "Switch item into status matched by REGEX.
REGEX could be like `easy-todo-ongoing-regexp'."
  (let ((inhibit-read-only t))
    (save-excursion
      (move-beginning-of-line 1)
      (or (re-search-forward easy-todo-matcher (line-end-position) t 1)
          (re-search-backward easy-todo-matcher (point-min) t 1))
      (replace-match (replace-regexp-in-string "^\\^" "" regexp)))))

(defun easy-todo-sort-buffer ()
  "Sort all todo items in buffer."
  (interactive)
  (easy-todo-sort-region (point-min) (point-max)))

(defun easy-todo-sort-region (beg end)
  "Sort todo items by `easy-todo-matcher' between BEG and END.
BEG and END are points."
  (interactive "r")
  (let ((inhibit-read-only t)
        (remaining-flags easy-todo-regexps)
        (pos beg)                     ; position for inserting new items
        item-beg item-end flag
        (orig-pos (point)))
    (goto-char beg)
    (while (and remaining-flags (cdr remaining-flags))
      (setq flag (car remaining-flags))
      (while (setq item-beg (re-search-forward flag end t 1))
        (setq item-beg (- item-beg 2))
        (setq item-end (re-search-forward
                        (mapconcat 'identity (cdr remaining-flags) "\\|") end t 1))
        (if item-end
            (setq item-end (- (point) 2))
          (setq item-end end))
        (goto-char pos)
        (insert (delete-and-extract-region item-beg item-end))
        (setq pos (point)))
      (setq remaining-flags (cdr remaining-flags)))
    (goto-char beg)))                   ; place point at begin

(defun easy-todo-kill-item ()
  "Kill most recent item."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (let (beg end)
        (move-beginning-of-line 1)
        (setq beg (or (re-search-forward easy-todo-matcher (line-end-position) t 1)
                      (re-search-backward easy-todo-matcher (point-min) t 1)))
        (if beg
            (progn
              (setq beg (- beg 2))
              (setq end (re-search-forward easy-todo-matcher (point-max) t 1))
              (if end
                  (setq end (- end 2))
                (setq end (point-max)))
              (kill-region beg end))
          (message "easy-todo item not found here"))))))


(provide 'easy-todo)

;;; easy-todo.el ends here
