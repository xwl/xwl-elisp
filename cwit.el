;;; cwit.el --- Emacs interface to cwit

;; Copyright (C) 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1
;; Last updated: 2008/02/01

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

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'cwit "cwit")
;;
;; To run, just `M-x cwit'.

;; TODO

;; - checking new news at next page?

;; Bugs

;; - Delete the all whitespaces after cwit prompt line would cause
;;   trouble!  In this case, try undo it.

;;; Code:

;;; Customizations

(defgroup cwit nil
  "Emacs interface for cwit."
  :group 'cwit)

(defcustom cwit-mode-hook nil
  "Normal hook run after entering cwit mode."
  :type 'hook
  :group 'cwit)

(defcustom cwit-server "isugamo.local.ce-lab.net"
  "Cwit server address."
  :type 'string
  :group 'cwit)

(defcustom cwit-user-name ""
  "Cwit user name."
  :type 'string
  :group 'cwit)

(defcustom cwit-user-password ""
  "Cwit user password."
  :type 'string
  :group 'cwit)

(defcustom cwit-refresh-interval 120
  "Auto-refresh *cwit* intervals, in seconds."
  :type 'number
  :group 'cwit)

(defcustom cwit-fill-static-center 10
  "Docstring is copied from `erc-fill-static-center':
Column around which all statically filled messages will be
centered.  This column denotes the point where the ' ' character
between <nickname> and the entered text will be put, thus aligning
nick names right and text left."
  :type 'number
  :group 'cwit)


;;; Cwit Mode and Interfaces

(defface cwit-timestamp-face '((t (:bold t :foreground "green")))
  "Cwit timestamp face."
  :group 'cwit)

(defvar cwit-timestamp-face 'cwit-timestamp-face)

(defface cwit-prompt-face
  '((t (:bold t :foreground "Black" :background "lightBlue2")))
  "Cwit face for the prompt."
  :group 'cwit)

(defvar cwit-prompt-face 'cwit-prompt-face)

(defvar cwit-font-lock-keywords
  '(("^[0-9][0-9]:[0-9][0-9]"
     (0 cwit-timestamp-face nil t))
    ("^Cwit>"
     (0 cwit-prompt-face nil t)))
  "Keywords to highlight in cwit mode.")

(defvar cwit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'cwit-send-current-line)
    (define-key map (kbd "M-m") 'cwit-beginning-of-line)
    map)
  "Keymap for `cwit-mode'.")

(define-derived-mode cwit-mode nil "Cwit"
  "Emacs interface for cwit.
\\{cwit-mode-map}"
  (setq font-lock-defaults '(cwit-font-lock-keywords))
  (add-to-list 'global-mode-string 'cwit-mode-line-string)
  (add-hook 'window-configuration-change-hook 'cwit-update-mode-line)
  (insert "====== Welcome to Cwit! ======\n")
  (insert "Cwit> ")
  (setq cwit-input-marker (point-marker))
  (cwit-login)
  ;; setup auto refresh timer
  (when cwit-receive-timer
    (cancel-timer cwit-receive-timer))
  (setq cwit-receive-timer
        (run-at-time t cwit-refresh-interval 'cwit-receive))
  (cwit-make-read-only)
  (run-hooks 'cwit-mode-hook))

;;;###autoload
(defun cwit ()
  "Create a *Cwit* buffer."
  (interactive)
  (let ((cwit-exist-p (get-buffer cwit-buffer)))
    (switch-to-buffer cwit-buffer)
    (unless cwit-exist-p
      (cwit-mode))))

(defun cwit-send-current-line ()
  "Sent current line to cwit."
  (interactive)
  (if (< (point) (marker-position cwit-input-marker))
      (message "Point not in the input area")
    (let* ((beg (marker-position cwit-input-marker))
           (end (point))
           (message (delete-and-extract-region beg end)))
      (if (string-match "\\`[[:space:]]+\\'" message)
          (message "Blank line - ignoring...")
        (cwit-send message)))))

(defun cwit-beginning-of-line ()
  "When cwit prompt is on current line, move point next to it, else same
as `move-beginning-of-line'."
  (interactive)
  (call-interactively 'move-beginning-of-line)
  (when (looking-at "Cwit> ")
    (forward-char (length "Cwit> "))))


;;; Implementations

(defvar cwit-debug-status nil
  "Last http request status.")

(defvar cwit-buffer "*cwit*"
  "Name of the buffer used for the cwit.")

(defvar cwit-last-entry-index 0
  "Last entry's index.")

(defvar cwit-input-marker nil
  "The marker where input should be inserted.")

(defvar cwit-receive-timer nil)

(defvar cwit-mode-line-string ""
  "For indicating whether there are new cwit messages.")

(defvar cwit-unread-message-counter 0)

(defun cwit-login ()
  (let ((url (format "http://%s/users/login" cwit-server))
        (url-request-method "POST")
        (url-request-data (format "user[uid]=%s&user[pass]=%s"
                                  cwit-user-name cwit-user-password)))
    (url-retrieve url 'cwit-login-callback)))

(defun cwit-login-callback (status)
  (setq cwit-debug-status status))

(defun cwit-send (message)
  (let ((url (format "http://%s/cwit/create" cwit-server))
        (url-request-method "POST")
        (url-request-data
         (format "stat[entry]=%s" (url-hexify-string message))))
    (cwit-insert-entry (format-time-string "%H:%M" (current-time))
                       cwit-user-name
                       message)
    (setq cwit-last-entry-index (1+ cwit-last-entry-index))
    (goto-char (marker-position cwit-input-marker))
    (url-retrieve url 'cwit-send-callback)))

(defun cwit-send-callback (status)
  (setq cwit-debug-status status)
  (message "Message sent"))

(defun cwit-receive ()
  (let ((url (format "http://%s/cwit" cwit-server)))
    (url-retrieve url 'cwit-receive-callback)
    (message "Reading cwit news...")))

(defun cwit-receive-callback (status)
  (setq cwit-debug-status status)
  (let ((entries '())
        entry last-entry)
    (while (and (setq entry (cwit-parse-entry))
                (> (car entry) cwit-last-entry-index))
      (unless last-entry
        (setq last-entry entry))
      (setq entries (cons entry entries)
            cwit-unread-message-counter (1+ cwit-unread-message-counter)))
    (when (and (not (equal (buffer-name (current-buffer)) cwit-buffer))
               (> cwit-unread-message-counter 0))
      (setq cwit-mode-line-string (format "cwit(%d)" cwit-unread-message-counter))
      (force-mode-line-update))
    (kill-buffer (current-buffer))
    (with-current-buffer cwit-buffer
      (if entries
          (progn
            (dolist (entry entries)
              (let ((timestamp (substring (nth 1 entry) 11 16))
                    (author (nth 2 entry))
                    (message (nth 3 entry)))
                (cwit-insert-entry timestamp author message)))
            (setq cwit-last-entry-index (car last-entry)))
        (message "No cwit news is good news")))))

(defun cwit-insert-entry (timestamp author message)
  "Note TIMESTAMP should be in the form as: 20:18."
  (let ((inhibit-read-only t))
    (goto-char (marker-position cwit-input-marker))
    (forward-line 0)
    (insert (format "%s <%s> %s\n" timestamp author message))
    (save-excursion
      (let ((end (point))
            (fill-prefix (make-string cwit-fill-static-center 32)))
        (re-search-backward "[0-9]\\{2\\}:[0-9]\\{2\\}" nil t 1)
        (fill-region (point) end)
        (cwit-make-read-only)
        (goto-char (point-max))))))

(defun cwit-parse-entry ()
  "(index timestamp author message)."
  (let (index timestamp author message)
    (when (re-search-forward "<tr id=\"stat_\\(.*\\)\" .*>" nil t 1)
      (setq index (string-to-number (match-string 1))))
    (when (and (re-search-forward "<span class=\"author\">" nil t 1)
               (re-search-forward "<a href=\"/.*\" class=\"url fn\">\\(.*\\)</a>" nil t 1))
      (setq author (match-string 1)))
    (when (re-search-forward "<span class=\"entry-title entry-content\">" nil t 1)
      (skip-chars-forward "[[:space:]]")
      (let ((beg (point)))
        (re-search-forward "</span>" nil t 1)
        (re-search-backward "</span>" nil t 1)
        (skip-chars-backward  "[[:space:]]")
        (setq message
              (decode-coding-string
               (replace-regexp-in-string "\n" " " (buffer-substring beg (point)))
               'utf-8))))
    (when (re-search-forward "<span class=\"published\" title=\"\\(.*\\)\">" nil t 1)
      (setq timestamp (match-string 1)))
    (when (and index timestamp author message)
      (list index timestamp author message))))

(defun cwit-update-mode-line ()
  (when (equal (buffer-name (current-buffer)) cwit-buffer)
    (setq cwit-mode-line-string ""
          cwit-unread-message-counter 0)
    (force-mode-line-update)))

(defun cwit-make-read-only ()
  "Make all the text in the current buffer read-only.
Put this function on `cwit-insert-entry'."
  (put-text-property (point-min) (1- (point-max)) 'read-only t)
  (put-text-property (point-min) (point-max) 'front-sticky t)
  (put-text-property (point-min) (point-max) 'rear-nonsticky t))

(provide 'cwit)

;;; cwit.el ends here