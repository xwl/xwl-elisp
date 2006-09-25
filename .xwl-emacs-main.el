;; scripts to run for main Emacs

(defun xwl-auto-save-hook ()
;;  (xwl-auto-save-desktop)
  (emms-score-save-hash)
)

(defun xwl-check-holidays ()
  (when (check-calendar-holidays (calendar-current-date))
    (calendar)
    (with-current-buffer "*Calendar*"
      (calendar-cursor-holidays))))

(defun xwl-runneing-daily ()
  "Staffs to run daily."
  (xwl-check-holidays)
  (plan))

;; timer when idle
(setq xwl-run-when-idle-hook nil)  ; Functions to run when Emacs is idle.

  ;; Avoid influencing watching movies.
;;   (unless emms-player-playing-p
;;     (gnus-group-get-new-news))

(add-hook 'xwl-run-when-idle-hook 'xwl-fortune-of-the-day)

(defun xwl-emacs-main ()
  (interactive)

  (shell-command "sudo ~/bin/.xwl-after-start-hook")

  (setq display-time-mail-file 'no-check)

  (server-start)

  (add-hook 'auto-save-hook 'xwl-auto-save-hook) ; auto save


  (run-with-idle-timer 300 t 'xwl-run-when-idle-hook) ; when idle

  ;; EMMS
  (emms-add-directory-tree emms-source-file-default-directory)
  (let ((mounted? (zerop (shell-command "mount | grep fun"))))
    (if mounted?
	(emms-add-directory-tree "/mnt/fun/music")
      (setq connected?
	    (zerop (shell-command "sudo mount /dev/sda5 /mnt/fun -o umask=0")))
      (when connected?
	(emms-add-directory-tree "/mnt/fun/music"))))
  (emms-add-directory-tree "/home/william/download/music")
  ;; (emms-playlist-sort-by-score)
  ;; (emms-playlist-mode-open-buffer xwl-emms-playlist-file)

  (run-with-timer 0 86400 'xwl-running-daily) ; dialy stuffs

  ;; send mail by google
  ;; (xwl-sendmail-by-google)

  (xwl-erc-select)

  ;; end
  (find-file "~/.scratch")
  (message (substring (emacs-version) 0 16)))

(defun xwl-sendmail-by-google ()
  "Enable sendmail by google."
  (interactive)
  (require 'starttls)
  (setq smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	smtpmail-auth-credentials
	`(("smtp.gmail.com" 587 "william.xwl@gmail.com" ,pwgmail))
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
  (message "sendmail by google enabled.")

  ;; gcc to self
  (setq gnus-parameters
        `( ;; Seems it's only needed when fetching mail by Emacs herself.
          (".*" (gcc-self . t))		; always Gcc to oneself
          ,@(mapcar*
             (lambda (args)
               (let ((list (car args))
                     (group (cadr args)))
                 `(,group (to-address  . ,list)
                          (auto-expire . t)
                          (to-list     . ,list)
                          ;; (gcc-self    . t)
                          )))
             xwl-mailing-list-group-alist)
          (,(regexp-opt
             '("important"
               "savings"
               "nnfolder+archive:outgoing.important"
               "nnfolder+archive:outgoing.news"
               "nnfolder+archive:outgoing.work"
               "general"
               "hotmail"))
           (gcc-self . t)))))


;; main
;; ----

(xwl-emacs-main)

;; .xwl-emacs-main.el ends here
