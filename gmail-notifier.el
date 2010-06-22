;;; gmail-notifier.el --- Notify unread gmail on mode line

;; Copyright (C) 2010  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show unread gmail count on mode line, it looks like this: G(2).
;; `G' could be the gmail logo if your emacs supports image.
;; To setup:
;;   (require 'gmail-notifier)
;;   (setq gmail-notifier-username "william.xwl"
;;         gmail-notifier-password "******")
;;   (gmail-notifier-start)

;;; Code:

(require 'xml)
(require 'gnus-util)
(eval-when-compile (require 'cl))

(defgroup gmail-notifier nil
  "Gmail notifier."
  :prefix "gmail-notifier-"
  :group 'mail)

(defcustom gmail-notifier-username nil
  "Gmail username."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-password nil
  "Gmail password."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-curl-command "curl"
  "curl command line including additional options."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-new-mails-hook nil
  "Hooks to run when new mails arrives."
  :type 'list
  :group 'gmail-notifier)

(defcustom gmail-notifier-timer-interval 300
  "Interval(in seconds) for checking gmail."
  :type 'number
  :group 'gmail-notifier)

;; Entry format:
;; '(((author "AUTHOR") (title "TITLE") (summary "SUMMARY") (link "LINK") (date "DATE"))...)
(defvar gmail-notifier-unread-entries nil)

(defconst gmail-notifier-logo-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
            "/* XPM */
static char * gmail_xpm[] = {
\"16 16 8 1\",
\" 	c None\",
\".	c #DA3838\",
\"+	c #E95A5A\",
\"@	c #F28181\",
\"#	c #F9A7A7\",
\"$	c #FFB6B6\",
\"%	c #FFE2E2\",
\"&	c #FFFFFF\",
\"                \",
\"                \",
\"                \",
\"...@########@...\",
\"....$&&&&&&$....\",
\"..@+.$&&&&$.+@..\",
\"..&@+.$&&$.+@&..\",
\"..&&@+.$$.+@&&..\",
\"..&&$@+..+@$&&..\",
\"..&%#$@++@$#%&..\",
\"..%#%&&@@&&%#%..\",
\"..#%&&&&&&&&%#..\",
\"..%&&&&&&&&&&%..\",
\"..############..\",
\"                \",
\"                \"};
"))
  "Image for twitter logo.")

(defconst gmail-notifier-logo
  (if gmail-notifier-logo-image
      (apply 'propertize " " `(display ,gmail-notifier-logo-image))
    "G"))

(defvar gmail-notifier-timer nil)

;;;###autoload
(defun gmail-notifier-start ()
  (interactive)
  (unless gmail-notifier-username
    (setq gmail-notifier-username (read-string "Gmail username: ")))
  (unless gmail-notifier-password
    (setq gmail-notifier-password (read-passwd "Gmail password: ")))
  (add-to-list 'global-mode-string
               '(:eval (gmail-notifier-make-unread-string)) t)
  (setq gmail-notifier-timer
        (run-with-timer 0
                        gmail-notifier-timer-interval
                        'gmail-notifier-get-unread)))

(defun gmail-notifier-stop ()
  (interactive)
  (cancel-timer gmail-notifier-timer)
  (setq gmail-notifier-timer nil)
  (setq global-mode-string
	(remove '(:eval (gmail-notifier-make-unread-string))
		global-mode-string)))

(defun gmail-notifier-get-unread ()
  (gmail-notifier-shell-command-asynchronously-with-callback
   (format "%s -s --user \"%s:%s\" https://mail.google.com/mail/feed/atom"
           gmail-notifier-curl-command
           gmail-notifier-username
           gmail-notifier-password)
   'gmail-notifier-callback))

(defun gmail-notifier-callback ()
  (setq gmail-notifier-unread-entries
        (mapcar
         (lambda (entry)
           `((author ,(caddr (assoc 'name (assoc 'author entry))))
             (title ,(caddr (assoc 'title entry)))
             (summary ,(caddr (assoc 'summary entry)))
             (link ,(cdr (assoc 'href (cadr (assoc 'link entry)))))
             (date ,(caddr (assoc 'issued entry)))))
         (remove-if-not
          (lambda (tag)
            (and (consp tag) (eq (car tag) 'entry)))
          (car (xml-parse-region (point-min) (point-max))))))
  (unless (null gmail-notifier-unread-entries)
    (run-hooks 'gmail-notifier-new-mails-hook))
  (gmail-notifier-make-unread-string)
  (force-mode-line-update)
  (kill-buffer))

(defun gmail-notifier-make-unread-string ()
  (if (null gmail-notifier-unread-entries)
      ""
    (let ((s (format "(%d) " (length gmail-notifier-unread-entries)))
          (map (make-sparse-keymap))
          (url "https://mail.google.com"))
      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)))
      (add-text-properties 0 (length s)
                           `(local-map ,map mouse-face mode-line-highlight
                                       uri ,url help-echo
                                       ,(concat
                                         (gmail-notifier-make-preview-string)
                                         "\nmouse-2: visit web gmail"))
                           s)
      (concat " " gmail-notifier-logo s))))

(defmacro gmail-notifier-shell-command-asynchronously-with-callback (cmd
                                                                     callback)
  "Run CMD asynchronously and apply CALLBACK in the output buffer.
Note: you are suggested to kill process buffer at the end of CALLBACK. "
  `(let* ((buf (generate-new-buffer (concat "*" (replace-regexp-in-string " .*" "" ,cmd) "*")))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel
      p
      (lambda (process event)
        (with-current-buffer (process-buffer process)
          (when (eq (process-status  process) 'exit)
            (let ((inhibit-read-only t)
                  (err (process-exit-status process)))
              (if (zerop err)
                  (funcall ,callback)
                (error "`%s' failed: %d" ,cmd err)))))))))

(defun gmail-notifier-make-preview-string ()
  (mapconcat
   (lambda (entry)
     (let ((s (format "%s - %s, %s, %s"
                      (cadr (assoc 'author entry))
                      (let ((summary (cadr (assoc 'summary entry))))
                        (substring summary 0 (min (length summary) 20)))
                      (let ((summary (cadr (assoc 'summary entry))))
                        (substring summary 0 (min (length summary) 20)))
                      (gnus-user-date (cadr (assoc 'date entry))))))
       ;; (add-text-properties 0 (length s)
       ;;                      `(mouse-face mode-line-highlight
       ;;                                   uri ,(cadr (assoc 'link entry)))
       ;;                      s)
       s))
   gmail-notifier-unread-entries
   "\n"))

(provide 'gmail-notifier)
;;; gmail-notifier.el ends here
