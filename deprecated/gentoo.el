;;; gentoo.el --- gentoo administration within Emacs

;; Copyright (C) 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1
;; Url: http://xwl.appspot.com/ref/gentoo.el

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

;; This package tries to simply gentoo daily administration work(not all
;; but most frequent commands) by using Emacs' ido features for
;; completing package names. It's modelled after `wajig.el' which I
;; wrote for Debian GNU/Linux.

;; To make full use of this package, you should install emerge, eix,
;; gentoolkit, genlop, sudo, etc.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'gentoo "gentoo")

;;; Code:

(require 'ansi-color)

;;; Customizations

(defgroup gentoo nil
  "An interface for gentoo in debian."
  :group 'gentoo)

(defcustom gentoo-mode-hook nil
  "Normal hook run after entering gentoo mode."
  :type 'hook
  :group 'gentoo)

(defcustom gentoo-cache-filename "~/.gentoo-cache.el"
  "Gentoo cache file."
  :type 'string
  :group 'gentoo)


;;; Gentoo Mode

;;;###autoload
(defun gentoo ()
  "Create a *gentoo* buffer."
  (interactive)
  (let ((gentoo-exist-p (get-buffer "*gentoo*")))
    (switch-to-buffer "*gentoo*")
    (unless gentoo-exist-p
      (gentoo-mode))))

(defvar gentoo-mode-map
;;  (setq gentoo-mode-map
  (let ((map (make-sparse-keymap)))
    ;; help
    (define-key map (kbd "h e") 'gentoo-help-emerge)
    (define-key map (kbd "h i") 'gentoo-help-eix)
    (define-key map (kbd "h q") 'gentoo-help-equery)
    (define-key map (kbd "h g") 'gentoo-help-genlop)
    ;; mode related
    (define-key map (kbd "K") 'gentoo-kill)
    ;; essensial pkg operations
    (define-key map (kbd "U") 'gentoo-eix-sync)
    (define-key map (kbd "s") 'gentoo-eix-search-name)
    (define-key map (kbd "S") 'gentoo-eix-search-description)
    (define-key map (kbd "i") 'gentoo-emerge)
    (define-key map (kbd "u") 'gentoo-emerge-update)
    (define-key map (kbd "r") 'gentoo-emerge-unmerge)
    (define-key map (kbd "E") 'gentoo-edit-make-conf)
    ;; service control
    (define-key map (kbd "v x") 'gentoo-service-start)
    (define-key map (kbd "v v") 'gentoo-service-stop)
    (define-key map (kbd "v r") 'gentoo-service-restart)
    (define-key map (kbd "v a") 'gentoo-service-rc-add)
    (define-key map (kbd "v d") 'gentoo-service-rc-delete)
    (define-key map (kbd "v s") 'gentoo-service-rc-show)
    ;; query pkg info
    (define-key map (kbd "q f") 'gentoo-equery-files)
    (define-key map (kbd "q s") 'gentoo-equery-size)
    (define-key map (kbd "q b") 'gentoo-equery-belongs)
    map)
  ;; )
  "Keymap for `gentoo-mode'.")

(define-derived-mode gentoo-mode nil "Gentoo"
  "Major mode for gentoo.
\\{gentoo-mode-map}"
  (gentoo-mode-help)
  (set-syntax-table gentoo-mode-syntax-table)
  ;; (setq font-lock-defaults '(gentoo-font-lock-keywords))
  (unless gentoo-installed-pkgs
    (gentoo-update-cache))
  (run-hooks 'gentoo-mode-hook))

(defun gentoo-mode-help ()
  "Help page for gentoo-mode."
  (interactive)
  (message "For a list of available key bindings, press `F1 m' or `C-h m'."))

(defvar gentoo-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?/ "w" st)
    st)
  "Syntax table used while in `gentoo-mode'.")


;;; emerge

(defun gentoo-emerge (pkg)
  (interactive 
   (list
    (ido-completing-read "$ sudo emerge " gentoo-installed-pkgs)))
  (gentoo-command `("sudo" "emerge" ,pkg)))

(defun gentoo-emerge-unmerge (pkg)
  (interactive
   (list
    (ido-completing-read "$ sudo emerge --unmerge " gentoo-installed-pkgs)))
  (gentoo-command `("sudo" "emerge" "--unmerge" ,pkg)))

(defun gentoo-emerge-update (pkg)
  (interactive
   (list
    (ido-completing-read "$ sudo emerge --update " gentoo-installed-pkgs)))
  (gentoo-command `("sudo" "emerge" "--update" ,pkg)))


;;; eix

(defun gentoo-eix-update ()
  "$ sudo update-eix"
  (interactive)
  (gentoo-command '("sudo" "update-eix")))

(defun gentoo-eix-sync ()
  (interactive)
  (gentoo-command '("sudo" "eix-sync")))

(defun gentoo-eix-search-name (pkg)
  "$ eix -s pkg"
  (interactive
   (list
    (ido-completing-read "$ eix -s " gentoo-installed-pkgs)))
  (gentoo-command `("eix" "-s" ,pkg)))

(defun gentoo-eix-search-description (pkg)
  "$ eix -S pkg"
  (interactive
   (list
    (ido-completing-read "$ eix -S " gentoo-installed-pkgs)))
  (gentoo-command `("eix" "-S" ,pkg)))


;;; equery

(defun gentoo-equery-files (pkg)
  (interactive
   (list
    (ido-completing-read "$ equery files " gentoo-installed-pkgs)))
  (gentoo-command `("equery" "files" ,pkg)))

(defun gentoo-equery-sizes (pkg)
  (interactive
   (list
    (ido-completing-read "$ equery sizes " gentoo-installed-pkgs)))
  (gentoo-command `("equery" "sizes" ,pkg)))

(defun gentoo-equery-belongs (pkg)
  (interactive 
   (list (read-string "$ equery belongs ")))
  (gentoo-command `("equery" "belongs" ,pkg)))


;;; Service Control (/etc/init.d/*)

(defun gentoo-service-start (service)
  (interactive
   (list
    (ido-completing-read "$ sudo /etc/init.d/? start: " gentoo-services)))
  (gentoo-command `("sudo" ,(concat "/etc/init.d/" service) "start")))

(defun gentoo-service-stop (service)
  (interactive
   (list
    (ido-completing-read "$ sudo /etc/init.d/? stop: " gentoo-services)))
  (gentoo-command `("sudo" ,(concat "/etc/init.d/" service) "stop")))

(defun gentoo-service-restart (service)
  (interactive
   (list
    (ido-completing-read "$ sudo /etc/init.d/? restart: " gentoo-services)))
  (gentoo-command `("sudo" ,(concat "/etc/init.d/" service) "restart")))

(defun gentoo-service-rc-add (service)
  "Add SERVICE to default runlevel."
  (interactive
   (list
    (ido-completing-read "$ sudo rc-update add ? default: " gentoo-services)))
  (gentoo-command `("sudo" "rc-update" "add" ,service "default")))

(defun gentoo-service-rc-delete (service)
  "Delete SERVICE from default runlevel."
  (interactive
   (list
    (ido-completing-read "$ sudo rc-update del ? default: " gentoo-services)))
  (gentoo-command `("sudo" "rc-update" "del" ,service "default")))

(defun gentoo-service-rc-show ()
  "Show SERVICE enabled in default runlevel."
  (interactive)
  (gentoo-command '("sudo" "rc-update" "show" "default" "--verbose")))


;;; Other Commands

(defun gentoo-help-emerge ()
  (interactive)
  (gentoo-command (split-string "emerge -h")))

(defun gentoo-help-eix ()
  (interactive)
  (gentoo-command (split-string "eix -h")))

(defun gentoo-help-equery ()
  (interactive)
  (gentoo-command (split-string "equery -h")))

(defun gentoo-help-genlop ()
  (interactive)
  (gentoo-command (split-string "genlop -h")))

(defun gentoo-edit-make-conf ()
  (interactive)
  (find-file "/sudo::/etc/make.conf"))


;;; Low Level Functions

(defun gentoo-command (command-string)
  "Run COMMAND-STRING, e.g., '(\"cmd\" \"arg1\" ...) in *gentoo*
buffer."
  (let ((inhibit-read-only t))
    (gentoo)
    (erase-buffer)
    (if gentoo-running
	(error "Gentoo process already exists")
      (setq gentoo-running t)
      (setq gentoo-process
	    (apply 'start-process "gentoo" "*gentoo*" command-string))
      (set-process-filter gentoo-process 'gentoo-process-filter)
      (set-process-sentinel gentoo-process 'gentoo-process-sentinel))))

(defvar gentoo-process nil)
(defvar gentoo-running nil)

(defun gentoo-process-sentinel (process event)
  (setq gentoo-running nil)
  (save-excursion
    (with-current-buffer (get-buffer "*gentoo*")
      (let ((inhibit-read-only t))
	(cond
	 ((eq (process-status process) 'exit)
	  (insert "------------- done --------------\n")
	  (goto-char (point-max)))
	 ((eq (process-status process) 'signal)
	  (message "gentoo process killed")))))))

(defun gentoo-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process)))
	  (inhibit-read-only t))
      (save-excursion
	(goto-char (process-mark process))
        (insert (replace-regexp-in-string "" "" output))
	(set-marker (process-mark process) (point)))
      (and moving (goto-char (process-mark process)))
      (let ((ansi-color-for-comint-mode t))
        (ansi-color-process-output "")))))

(defun gentoo-kill ()
  "Kill running gentoo process."
  (interactive)
  (when gentoo-process
    (unless (eq (process-status gentoo-process) 'exit)
      (delete-process gentoo-process))
    (setq gentoo-running nil)))

(defvar gentoo-installed-pkgs nil
  "Installed packages on the system.
You can run `gentoo-update-cache' to keep update.")

(defvar gentoo-services nil
  "Existing services' list.
You can run `gentoo-update-cache' to keep update.")

(defun gentoo-update-cache ()
  "Update gentoo cache saved in `gentoo-cache-filename'."
  (interactive)
  (message "Updating gentoo cache...")
  (gentoo-update-installed-pkgs)
  (gentoo-update-services)
  ;; (gentoo-update-command-path-alist)
  (with-temp-buffer
    (insert ";; automatically generated by gentoo.el\n")
    (insert (format "
\(setq gentoo-installed-pkgs '%S
      gentoo-services '%S)
"
                    gentoo-installed-pkgs
                    gentoo-services))
    (write-region (point-min) (point-max) gentoo-cache-filename))
  (message "Updating gentoo cache...done"))

(defun gentoo-update-installed-pkgs ()
  "Update `gentoo-installed-pkgs'."
  (setq gentoo-installed-pkgs
	(split-string
         (shell-command-to-string 
          "eix -I | egrep '^\\[I\\]' | sed 's/^.*\\///'"))))

(defun gentoo-update-services ()
  (setq gentoo-services
	(split-string (shell-command-to-string "ls /etc/init.d/"))))

;; (add-hook 'gentoo-mode-hook 'less-minor-mode-on)

;; initial variables
(if (file-readable-p gentoo-cache-filename)
    (load-file gentoo-cache-filename)
  (gentoo-update-cache))

(provide 'gentoo)

;;; gentoo.el ends here
