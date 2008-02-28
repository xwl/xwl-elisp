;;; fink.el --- fink administration within Emacs

;; Copyright (C) 2007, 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.3
;; Url: http://williamxu.net9.org/ref/fink.el

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

;; This package tries to simply fink daily administration work(not all
;; but most frequent commands) by using Emacs' ido features for
;; completing package names. It's modelled after `wajig.el' which I
;; wrote for Debian GNU/Linux.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'fink)

;;; Code:

(require 'ansi-color)

;;; Customizations

(defgroup fink nil
  "An interface for fink in macosx."
  :group 'fink)

(defcustom fink-mode-hook nil
  "Normal hook run after entering fink mode."
  :type 'hook
  :group 'fink)

(defcustom fink-cache-filename "~/.fink-cache.el"
  "Fink cache file."
  :type 'string
  :group 'fink)


;;; Fink Mode

;;;###autoload
(defun fink ()
  "Create a *fink* buffer."
  (interactive)
  (let ((fink-exist-p (get-buffer "*fink*")))
    (switch-to-buffer "*fink*")
    (unless fink-exist-p
      (fink-mode))))

(defvar fink-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "E" 'fink-edit-sources)
    (define-key map "h" 'fink-mode-help)

    (define-key map "i" 'fink-install)
    (define-key map "I" 'fink-install-at-point)
    (define-key map "K" 'fink-kill)
    (define-key map "o" 'fink-describe)
    (define-key map "d" 'fink-fetch)

    (define-key map "R" 'fink-remove)
    (define-key map "S" 'fink-apropos)
    (define-key map "s" 'fink-describe)
    (define-key map "u" 'fink-update)
    (define-key map "U" 'fink-selfupdate)
    (define-key map "" 'fink-show-at-point)

    ;; services control
    (define-key map (kbd "V x") 'fink-service-start)
    (define-key map (kbd "V v") 'fink-service-stop)
    (define-key map (kbd "V r") 'fink-service-restart)
    ;; cursor movement
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)

    ;; dpkg commands
    (define-key map "L" 'fink-list-installed-files)
    map)
  "Keymap for `fink-mode'.")

(defvar fink-font-lock-keywords
  '(("^\n\\([a-zA-Z0-9].*: \\)\\(.*\\)"
     (1 font-lock-keyword-face nil t)
     (2 font-lock-function-name-face nil t))
    ("Web site:\\|Maintainer:"
     (0 font-lock-keyword-face t t)))
  "Keywords to highlight in fink mode.")

(define-derived-mode fink-mode nil "Fink"
  "Major mode for fink.
\\{fink-mode-map}"
  (fink-mode-help)
  (set-syntax-table fink-mode-syntax-table)
  (setq font-lock-defaults '(fink-font-lock-keywords))
  (unless fink-available-pkgs
    (if (file-readable-p fink-cache-filename)
        (load-file fink-cache-filename)
      (fink-update-cache)))
  (run-hooks 'fink-mode-hook))

(defun fink-mode-help ()
  "Help page for fink-mode."
  (interactive)
  (message "For a list of available key bindings, press `F1 m' or `C-h m'."))

(defvar fink-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?/ "w" st)
    st)
  "Syntax table used while in `fink-mode'.")


;;; define-fink-command

(defvar fink-process nil)
(defvar fink-running nil)

(defalias 'fink-completing-read
  (if (and (fboundp 'ido-completing-read)
	   ido-mode)
      'ido-completing-read		; added since Emacs 22
    'completing-read))

(defun fink-process-sentinel (process event)
  (setq fink-running nil)
  (save-excursion
    (with-current-buffer (get-buffer "*fink*")
      (let ((inhibit-read-only t))
	(case (process-status process)
          ((exit)
           (goto-char (point-max))
           (insert "------------- done --------------\n"))
          ((signal)
           (message "fink process killed")))))))

(defun fink-process-filter (process output)
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

(defun fink-kill ()
  "Kill running fink process."
  (interactive)
  (when fink-process
    (unless (eq (process-status fink-process) 'exit)
      (delete-process fink-process))
    (setq fink-running nil)))

(defvar fink-available-pkgs nil
  "Installed packages on the system.
You can run `fink-update-cache' to keep update.")

(defvar fink-services nil
  "Existing services' list.
You can run `fink-update-cache' to keep update.")

(defun fink-update-cache ()
  "Update fink cache saved in `fink-cache-filename'."
  (interactive)
  (message "Updating fink cache...")
  (fink-update-available-pkgs)
  (fink-update-services)
  ;; (fink-update-command-path-alist)
  (with-temp-buffer
    (insert ";; automatically generated by fink.el\n")
    (insert (format "
\(setq fink-available-pkgs '%S
      fink-services '%S)
"
                    fink-available-pkgs
                    fink-services))
    (write-region (point-min) (point-max) fink-cache-filename))
  (message "Updating fink cache...done"))

(defun fink-update-available-pkgs ()
  "Update `fink-available-pkgs'."
  (setq fink-available-pkgs
	(split-string
         (shell-command-to-string
          ;; FIXME: Why doesn't "sed 's/.\{4\}'"  work?
          "fink list | sed 's/....//' | awk '{print $1}'"))))

(defun fink-update-services ()
  (setq fink-services
	(split-string (shell-command-to-string "service --list"))))

(defmacro define-fink-command (command &optional arglist)
  "Define a new fink command. COMMAND is one of fink commands,
such as help, update. Optional ARGLIST is (pkg). e.g.,

    (define-fink-command help)
    (define-fink-command show (pkg))

pkg is the package name to operate on."
  (when (symbolp command)
    (setq command (symbol-name command)))
  (let* ((fink-command (intern (format "fink-%s" command)))
	 (docstring ;; show help from `fink --help'
	  (let ((help (shell-command-to-string "fink --help")))
	    (if (string-match (format "^  %s      - .*" command) help)
                (match-string 0 help)
              "")))
	 (interactive
	  (if arglist
	      (setq interactive
		    `(interactive
		      (list
		       (fink-completing-read
                        ;; Assume `yes' for all interactive questions.
			,(format "$ sudo fink --yes --quiet %s " command)
                        fink-available-pkgs))))
	    '(interactive))))
    `(defun ,fink-command ,arglist
       ,docstring
       ,interactive
       (let ((inhibit-read-only t))
	 (fink)
	 (if fink-running
	     (error "Fink process already exists")
	   (erase-buffer)
	   (setq fink-running t)
	   (setq fink-process
		 ,(if arglist
		      `(start-process "fink" "*fink*"
				      "sudo" "fink" "--yes" "--quiet" ,command  ,(car arglist))
		    `(start-process "fink" "*fink*"
				    "sudo" "fink" "--yes" "--quiet" ,command)))
	   (set-process-filter fink-process 'fink-process-filter)
	   (set-process-sentinel fink-process 'fink-process-sentinel))))))

(define-fink-command install (pkg))
(define-fink-command remove (pkg))
(define-fink-command purge (pkg))
(define-fink-command update (pkg))
(define-fink-command apropos (pkg))
(define-fink-command describe (pkg))
(define-fink-command fetch (pkg))

(define-fink-command selfupdate)
(define-fink-command update-all)
(define-fink-command list)
(define-fink-command cleanup)
(define-fink-command index)
(define-fink-command validate)
(define-fink-command configure)
(define-fink-command scanpackages)
(define-fink-command show-deps)

(defun fink-show-at-point ()
  "Run `fink describe' on current word(pkg name)."
  (interactive)
  (fink-describe (current-word)))

(defun fink-install-at-point ()
  "Run `fink install' on current word(pkg name)."
  (interactive)
  (fink-install (current-word)))


;;; fink-command, fink-dpkg-command

(defun fink-command (command-string)
  "Run COMMAND-STRING, e.g., '(\"cmd\" \"arg1\" ...) in *fink*
buffer."
  (let ((inhibit-read-only t))
    (fink)
    (erase-buffer)
    (if fink-running
	(error "Fink process already exists")
      (setq fink-running t)
      (setq fink-process
	    (apply 'start-process "fink" "*fink*" command-string))
      (set-process-filter fink-process 'fink-process-filter)
      (set-process-sentinel fink-process 'fink-process-sentinel))))

;; Service Control

(defun fink-service-start (service)
  (interactive
   (list
    (ido-completing-read "Start service: " fink-services)))
  (fink-command (list "sudo" "service" service "start")))

(defun fink-service-stop (service)
  (interactive
   (list
    (ido-completing-read "Stop service: " fink-services)))
  (fink-command (list "sudo" "service" service "stop")))

(defun fink-service-restart (service)
  (interactive
   (list
    (ido-completing-read "Restart service: " fink-services)))
  (fink-service-stop service)
  (fink-service-start service))

(defun fink-edit-sources ()
  (interactive)
  (find-file "/sudo::/sw/etc/fink.conf"))

;; dpkg interfaces, if dpkg commands grows here, i'll add a
;; define-dpkg-command, but not now.
(defun fink-list-installed-files (pkg)
  (interactive
   (list
    (ido-completing-read "List files installed by: " fink-available-pkgs)))
  (fink-command (list "sudo" "dpkg" "-L" pkg)))

(provide 'fink)

;;; fink.el ends here
