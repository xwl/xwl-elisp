;; scripts to run for main Emacs

(defun xwl-auto-save-hook ()
;;  (xwl-auto-save-desktop)
  (emms-score-save-hash))

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

  (xwl-erc-select)

  ;; end
  (find-file "~/.scratch")
  (message (substring (emacs-version) 0 16)))

;; main
;; ----

(xwl-emacs-main)

;; .xwl-emacs-main.el ends here
