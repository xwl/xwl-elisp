;;; wajig.el --- an interface for wajig

;; Copyright (C) 2005 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; $Id: wajig.el,v 0.2 2006/01/20 22:52:31 xwl Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; "Wajig is an interface to many Debian administrative tasks. Written
;; in Python, wajig uses traditional Debian administration and user
;; tools including apt-get, dpkg, apt-cache, wget, and others. It is
;; intended to unify and simplify common administrative tasks." See
;; http://www.togaware.com/wajig/ for more infomation.

;; I write this package as a wrapper of wajig for Emacs. I love living
;; within Emacs. :-)

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'wajig)
;;
;; Then, simply run `M-x wajig'.

;; The Aim
;; -------

;; At first, i thought i would write an interface for wajig in
;; Emacs. Then i found wajig also supports some other system operations
;; besides deb control, such as start/stop system services. Hence, i
;; plan to add more system administrations, in the hope of reducing
;; typing in a shell as much as possible.

;; Important Notes
;; ---------------

;; 1. It's expected to run all the commands in a non-interactive
;; style. So do set APT::GET::Assume-Yes to "true".

;; 2. I only tested wajig commands that i tend to use often. Far from
;; all of the wajig commands! Hence, possibly some functions won't work
;; properly. Be careful and let me know if you find any!

;;; Change Log:

;; v 0.1 [2005/09/15 17:19:36] Initial version

;;; Todo:

;; 5. hightlight keywords in outputs?
;; 6. fix the output of `wajig udpate'.
;; 7. re-cache for update installed pkgs seems not working?

;;; Code:

;;; User Customization

(defgroup wajig nil
  "An interface for wajig in debian."
  :group 'wajig)

(defcustom wajig-mode-hook nil
  "Normal hook run after entering wajig mode."
  :type 'hook
  :group 'wajig)

(defcustom wajig-source-download-dir "~/download"
  "Directory for saving source downloads."
  :type 'string
  :group 'wajig)


;;; Wajig Commands

(defvar wajig-commands-string
  (shell-command-to-string "sudo wajig list-commands")
  "Output from `wajig commands'.")

(defvar wajig-process nil
  "Wajig command process.")

(defvar wajig-running nil
  "Is wajig process running?")

(defun wajig-update-installed-pkgs ()
  "Update `wajig-installed-pkgs'."
  (interactive)
  (setq wajig-installed-pkgs
	(split-string
	 (shell-command-to-string "sudo wajig list-installed"))))

(defvar wajig-installed-pkgs
  (wajig-update-installed-pkgs)
  "Installed packages on the system. You can run
`wajig-update-installed-pkgs' to keep update.")

(defun wajig-process-sentinel (process event)
  "Set buffer read-only after a wajig command finishes."
  (setq wajig-running nil)
  (save-excursion
    (with-current-buffer (get-buffer "*wajig*")
      (cond
       ((eq (process-status process) 'exit)
	(goto-char (point-max))
	(insert "------------- done --------------\n")
	(set (make-local-variable 'buffer-read-only) t))
       ((eq (process-status process) 'signal)
	(message "wajig process killed"))))))

(defun wajig-process-filter (process output)
  "Filter wajig command outputs."
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process))))
      (save-excursion
	(goto-char (process-mark process))
	(insert (replace-regexp-in-string "" "\n" output))
	;; (insert output)
	(set-marker (process-mark process) (point)))
      (if moving (goto-char (process-mark process))))))

(defun wajig-kill ()
  "Kill wajing process."
  (interactive)
  (when wajig-process
    (unless (eq (process-status wajig-process) 'exit)
      (kill-process wajig-process)
      (setq wajig-running nil))))

(defmacro define-wajig-command (command &optional arglist)
  "Define a new wajig command. COMMAND is one of wajig commands,
such as help, update. ARGLIST is (pkg). e.g.,

    (define-wajig-command help)
    (define-wajig-command show (pkg))

pkg is the package name to operate on."
  (when (symbolp command)
    (setq command (symbol-name command)))
  (let ((wajig-command (intern (format "wajig-%s" command)))
	(wajig-arglist) (docstring) (interactive))
    (if arglist
	(setq wajig-arglist (append arglist '(&optional re-cache))
	      docstring (progn
			  (string-match
			   (format "^ %s.*" command) wajig-commands-string)
			  (concat
			   (match-string 0 wajig-commands-string)
			   "\n\nNon-nil RE-CACHE forces re-caching `wajig-installed-pkgs'."))
	      interactive `(interactive
			    (list (wajig-completing-read ; require emacs22.
				   ,(format "$ sudo wajig %s " command)
				   wajig-installed-pkgs))))
      (setq wajig-arglist nil	       ; show help from `wajig commands'
	    docstring (progn
			(string-match
			 (format "^ %s.*" command) wajig-commands-string)
			(match-string 0 wajig-commands-string))
	    interactive '(interactive)))
    `(progn
       (defun ,wajig-command ,wajig-arglist
	 ,docstring
	 ,interactive
	 (wajig)
	 (set (make-local-variable 'buffer-read-only) nil)
	 (erase-buffer)
	 (if wajig-running
	     (error "Wajig process already exists")
	   (setq wajig-running t)
	   (setq wajig-process
		 ,(if arglist
		      `(progn
			 (when re-cache
			   (wajig-update-installed-pkgs))
			 (start-process "wajig" "*wajig*"
					"sudo" "wajig" ,command ,(car arglist)))
		    `(start-process "wajig" "*wajig*"
				    "sudo" "wajig" ,command)))
	   (set-process-filter wajig-process 'wajig-process-filter)
	   (set-process-sentinel wajig-process 'wajig-process-sentinel))))))

(defun wajig-do (command-string)
  "Run COMMAND-STRING in *wajig* buffer."
  (wajig)
  (set (make-local-variable 'buffer-read-only) nil)
  (erase-buffer)
  (if wajig-running
      (error "Wajig process already exists")
    (setq wajig-running t)
    (setq wajig-process
	  (apply 'start-process "wajig" "*wajig*" command-string))
    (set-process-filter wajig-process 'wajig-process-filter)
    (set-process-sentinel wajig-process 'wajig-process-sentinel)))

;; Compatibility
;; -------------

(defalias 'wajig-completing-read
  (if (fboundp 'ido-completing-read)
      'ido-completing-read		; added in Emacs 22
    'completing-read))

;; Wajig Command Without Arguments
;; -------------------------------

(mapc
 (lambda (command)
   ;; FIXME: Is this the correct way, using `eval'?
   (eval `(define-wajig-command ,command)))
 '(auto-clean
   auto-download
   clean
   commands
   daily-upgrade
   describe-new
   detail-new
   dist-upgrade
   docs
   file-download
   file-remove
   fix-configure
   fix-install
   fix-missing
   init
   integrity
   help
   large
   last-update
   list
   list-all
   list-cache
   list-commands
   list-daemons
   list-hold
   list-installed
   list-log
   list-orphans
   local-dist-upgrade
   local-upgrade
   non-free
   orphans
   policy
   reset
   search-apt
   setup
   showdistupgrade
   showinstall
   showremove
   showupgrade
   size
   sizes
   snapshot
   tasksel
   toupgrade
   update))

;; Wajig Command With One Argument
;; -------------------------------

(mapc
 (lambda (command)
   (eval `(define-wajig-command ,command (pkg))))
 '(auto-install
   available
   build
   build-depend
   changelog
   dependents
   describe
   detail
   download
   file-install
   find-file
   find-pkg
   force
   hold
   install/dist
   list-names
   list-files
   list-scripts
   list-section
   list-status
   list-wide
   move
   news
   package
   readme
   recommended
   reconfigure
   recursive
   reload
   repackage
   restart
   rpm2deb
   rpminstall
   rpmtodeb
   search
   show
   source
   start
   status
   status-match
   status-search
   stop
   suggested
   unhold
   unofficial
   update-alts
   update-pci-ids
   update-usb-ids
   versions
   whatis
   whichpkg

   install
   purge
   purge-depend
   purge-orphans
   reinstall
   remove
   remove-depend
   remove-orphans
   upgrade))

;; Addtional/Redefined Commands
;; ----------------------------

(defun wajig-mode-help ()
  "Help page for wajig-mode."
  (interactive)
  (message "For a list of available commands, press h or c."))

(defun wajig-edit-sources ()
  "Edit /etc/apt/sources.list using `tramp'."
  (interactive)
  (find-file "/su::/etc/apt/sources.list"))

(defun wajig-search-by-name (pkg)
  "Run `wajig search -n pkg' or `apt-cache search -n pkg'."
  (interactive
   (list
    (wajig-completing-read
     "$ apt-cache search -n "
     wajig-installed-pkgs)))
  (wajig-do `("apt-cache" "search" ,pkg "-n")))

(defun wajig-show-at-point ()
  "Run `wajig show' on current word(pkg name)."
  (interactive)
  (wajig-show (current-word)))

(defun wajig-manually (command-str)
  "Run special commands manually."
  (interactive "s$ ")
  (wajig-do (split-string command-str)))

(defun wajig-source (pkg)
  "Run \"cd `wajig-source-download-dir' && wajig source pkg\""
  (interactive
   (list
    (ido-completing-read
     (format "$ cd %s && wajig source " wajig-source-download-dir)
     wajig-installed-pkgs)))
  (let ((old-dir default-directory))
    (cd wajig-source-download-dir)
    (wajig-do `("wajig" "source" ,pkg))
    (cd old-dir)))

(defun wajig-source-download (pkg)
  "Run \"cd `wajig-source-download-dir' && wajig source -d pkg\""
  (interactive
   (list
    (ido-completing-read
     (format "$ cd %s && wajig source -d " wajig-source-download-dir)
     wajig-installed-pkgs)))
  (let ((old-dir default-directory))
    (cd wajig-source-download-dir)
    (wajig-do `("wajig" "source" ,pkg "-d"))
    (cd old-dir)))

(defun wajig-bug (pkg)
  "Run \"wajig bug pkg\". (Should run as non-root?)"
  (interactive
   (list
    (ido-completing-read
     "$ wajig bug "
     wajig-installed-pkgs)))
  (wajig-do `("wajig" "bug" ,pkg)))

(defun wajig-query-installed ()
  "$ dpkg -l	list packages concisely"
  (interactive)
  (wajig-do '("dpkg" "-l")))


;;; Wajig Mode

(defvar wajig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "E" 'wajig-edit-sources)
    (define-key map "u" 'wajig-update)
    (define-key map "s" 'wajig-search-by-name)
    (define-key map "S" 'wajig-search)
    (define-key map "c" 'wajig-commands)
    (define-key map "h" 'wajig-help)
    (define-key map "i" 'wajig-install)
    (define-key map "R" 'wajig-remove)
    (define-key map "L" 'wajig-list-files)
    (define-key map "t" 'wajig-toupgrade)
    (define-key map "U" 'wajig-upgrade)
    (define-key map "w" 'wajig-whichpkg)
    (define-key map "N" 'wajig-news)
    (define-key map "m" 'wajig-manually)
    (define-key map "o" 'wajig-show)
    (define-key map "C" 'wajig-auto-clean)
    (define-key map "d" 'wajig-source)
    (define-key map "D" 'wajig-source-download)
    (define-key map "" 'wajig-show-at-point)
    ;; query status
    (define-key map (kbd "Q a") 'wajig-available)
    (define-key map (kbd "Q c") 'wajig-changelog)
    (define-key map (kbd "Q S") 'wajig-list-scripts)
    (define-key map (kbd "Q r") 'wajig-readme)
    (define-key map (kbd "Q l") 'wajig-list-log)
    (define-key map (kbd "Q i") 'wajig-query-installed)
    (define-key map (kbd "Q s") 'wajig-status)
    (define-key map (kbd "Q n") 'wajig-non-free)
    (define-key map (kbd "Q b") 'wajig-bug)
    ;; file operation
    (define-key map (kbd "F i") 'wajig-file-install)
    ;; cursor movement
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map))

(defvar wajig-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st)
  "Syntax table used while in `wajig-mode'.")

(define-derived-mode wajig-mode nil "Wajig"
  "Major mode for wajig in debian.
\\{wajig-mode-map}"
  (wajig-mode-help)
  (set-syntax-table wajig-mode-syntax-table)
  ;; (set (make-local-variable 'buffer-read-only) t)
  (run-hooks 'wajig-mode-hook))

(defun wajig ()
  "Create a *wajig* buffer."
  (interactive)
  (let ((wajig-exist-p (get-buffer "*wajig*")))
    (switch-to-buffer "*wajig*")
    (unless wajig-exist-p
      (wajig-mode))))

(provide 'wajig)

;;; wajig.el ends here
