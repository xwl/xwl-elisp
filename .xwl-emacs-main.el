;; scripts to run for main Emacs

(defun xwl-auto-save-hook ()
;;  (xwl-auto-save-desktop)
  (emms-score-save-hash))

(defun xwl-check-holidays ()
  (calendar)
  (with-current-buffer "*Calendar*"
    (or (diary-view-entries)
        (when (check-calendar-holidays (calendar-current-date))
          (calendar-cursor-holidays)))))

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
  (let ((mounted? (zerop (shell-command "mount | grep sda5"))))
    (when mounted?
      (emms-add-directory-tree
       (catch 'return
         (mapc
          (lambda (path)
            (when (file-exists-p path)
              (throw 'return path)))
          '("/media/usb0/music"
            "/media/usb1/music"
            "/mnt/fun/music"))))))
  (emms-add-directory-tree "/home/william/download/music")
  ;; (emms-playlist-sort-by-score)
  ;; (emms-playlist-mode-open-buffer xwl-emms-playlist-file)

  ;; (xwl-erc-select)

  ;; end
  (unless (xwl-check-holidays)
    (find-file "~/.scratch")
    (delete-other-windows)
    (message (substring (emacs-version) 0 16)))

  ;; (run-with-timer 0 86400 'xwl-running-daily) ; dialy stuffs
)

;; main
;; ----

(xwl-emacs-main)

;; .xwl-emacs-main.el ends here
