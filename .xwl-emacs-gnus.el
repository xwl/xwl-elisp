;; scripts to run for Emacs with Gnus

(defun xwl-emacs-with-gnus ()
  ;; (setq display-time-mail-file nil)

  (gnus)

  ;; Eval this only if we are running Gnus!
  (run-with-idle-timer 300 t 'xwl-check-mails))

(defun xwl-check-mails ()
  "Check new mails/news every five minutes."
  (gnus-group-get-new-news))

(xwl-emacs-with-gnus)

;; .xwl-emacs-gnus.el
