;;; generic-apt.el --- generic apt alike interfaces for various package management tools

;; Copyright (C) 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; This extenstion tries to provide a generic apt(as used in Debian
;; GNU/Linux) alike interface over various automatic package management
;; tools, such as: apt-get(Debian GNU/Linux), yum(redhat/fedora),
;; emerge(Gentoo GNU/Linux), fink(Mac OS X), pkg-get(Solaris), etc.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'generic-apt "generic-apt")

;;; TODO

;; - Support multiple sessions, i.e., use local variables.
;; - startup screen better print some more useful info on the screen.
;; - search by name,  search by description.

;;; Code:

(require 'tramp)
(eval-when-compile
  (require 'cl))

;;; User Customization

(defgroup generic-apt nil
  "Generic apt alike interfaces for various package management tools."
  :group 'generic-apt)

(defcustom generic-apt-mode-hook nil
  "Normal hook run after entering `generic-apt-mode'."
  :type 'hook
  :group 'generic-apt)

(defcustom generic-apt-source-download-dir "~/download"
  "Directory for saving source downloads."
  :type 'string
  :group 'generic-apt)

(defcustom generic-apt-cache-filename "~/.generic-apt-cache.el"
  "Generic-Apt cache file."
  :type 'string
  :group 'generic-apt)

(defcustom generic-apt-select-methods '((apt-get "sudo apt-get")
                                        (fink "sudo fink"))
  "Package management tool lists.
Each element is the essential command prefix string.  For
example, \"ssh foo sudo apt-get\".  Then the command will execute
as: \"$ ssh foo sudo apt-get ...\""
  :type 'list
  :group 'generic-apt)

;; Symbol tag for different tools.
(defvar generic-apt-protocol nil)
(make-local-variable 'generic-apt-tag)

(defvar generic-apt-command "")
(make-local-variable 'generic-apt-command)

(defvar generic-apt-buffer-name "")
(make-local-variable 'generic-apt-buffer-name)

(defvar generic-apt-available-pkgs '())
(make-local-variable 'generic-apt-buffer-name)


;;; Generic-Apt Mode

(defvar generic-apt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" 'generic-apt-auto-clean)
    (define-key map "c" 'generic-apt-commands)
    (define-key map "d" 'generic-apt-source)
    (define-key map "D" 'generic-apt-source-download)
    (define-key map "E" 'generic-apt-edit-sources)
    (define-key map "h" 'generic-apt-help)
    (define-key map "i" 'generic-apt-install)
    (define-key map "I" 'generic-apt-install-at-point)
    (define-key map "K" 'generic-apt-kill)
    (define-key map "l" 'generic-apt-list-files)
    (define-key map "m" 'generic-apt-manually)
    (define-key map "o" 'generic-apt-show)
    (define-key map "R" 'generic-apt-remove)
    (define-key map "S" 'generic-apt-search)
    (define-key map "r" 'generic-apt-run-frequent-commands)
    (define-key map "s" 'generic-apt-search-by-name)
    (define-key map "t" 'generic-apt-toupgrade)
    (define-key map "u" 'generic-apt-update)
    (define-key map "U" 'generic-apt-upgrade)
    (define-key map "" 'generic-apt-show-at-point)
    (define-key map "w" 'generic-apt-whichpkg)
    ;; query status
    (define-key map (kbd "Q a") 'generic-apt-available)
    (define-key map (kbd "Q b") 'generic-apt-bug)
    (define-key map (kbd "Q c") 'generic-apt-changelog)
    (define-key map (kbd "Q i") 'generic-apt-query-installed)
    (define-key map (kbd "Q l") 'generic-apt-list-log)
    (define-key map (kbd "Q N") 'generic-apt-non-free)
    (define-key map (kbd "Q n") 'generic-apt-news)
    (define-key map (kbd "Q p") 'generic-apt-policy)
    (define-key map (kbd "Q r") 'generic-apt-readme)
    (define-key map (kbd "Q S") 'generic-apt-list-scripts)
    (define-key map (kbd "Q s") 'generic-apt-status)
    ;; file operation
    (define-key map (kbd "F i") 'generic-apt-file-install)
    ;; services control
    (define-key map (kbd "V x") 'generic-apt-start)
    (define-key map (kbd "V v") 'generic-apt-stop)
    (define-key map (kbd "V r") 'generic-apt-restart)
    ;; cursor movement
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    ;; network admin
    (define-key map (kbd "N i") 'generic-apt-network-ifconfig)
    (define-key map (kbd "N n") 'generic-apt-network-netstat)
    (define-key map (kbd "N p") 'generic-apt-network-ping)
    (define-key map (kbd "N t") 'generic-apt-network-traceroute)
    (define-key map (kbd "N m") 'generic-apt-network-nmap)
    map)
    "Keymap for `generic-apt-mode'.")

(defvar generic-apt-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st)
  "Syntax table used while in `generic-apt-mode'.")

(defvar generic-apt-font-lock-keywords
  `(("^Package:\\(.*\\)"
     (1 font-lock-function-name-face nil t))
    ("^Conflicts:"
     (0 font-lock-warning-face nil t))
    ("^Description:\\(.*\n\\)"
     (1 font-lock-function-name-face nil t))
    (,(concat
       "^\\("
       (regexp-opt
	'("Package" "Priority" "Section" "Installed-Size" "Maintainer"
	  "Architecture" "Version" "Depends" "Suggests" "Filename"
	  "Size" "MD5sum" "Description" "Tag" "Status" "Replaces"
	  "Conffiles" "Source" "Provides" "Pre-Depends" "Recommends"
          "SHA1" "SHA256" "Enhances" "Config-Version" "Task"))
       "\\):")
     (0 font-lock-keyword-face t t)))
  "Keywords to highlight in generic-apt mode.")

(define-derived-mode generic-apt-mode nil "Generic-Apt"
  "Major mode for generic apt alike interfaces for various package management tools.
\\{generic-apt-mode-map}"
  (setq generic-apt-command
        (ido-completing-read "generic-apt: "
                             (mapcar (lambda (i) (cadr i))
                                     generic-apt-select-methods)))
  (setq generic-apt-protocol
        (let ((methods generic-apt-select-methods)
              (i nil)
              (ret nil))
          (while methods
            (setq i (car methods)
                  methods (cdr methods))
            (when (string= generic-apt-command (cadr i))
              (setq ret (car i)
                    methods nil)))
          ret))
  (setq generic-apt-buffer-name
        (format "*Generic-Apt/%s*" generic-apt-command))
  (set-syntax-table generic-apt-mode-syntax-table)
  (setq font-lock-defaults '(generic-apt-font-lock-keywords))
  (setq buffer-read-only t)
  ;; (generic-apt-update-cache)
  (run-hooks 'generic-apt-mode-hook)
  (generic-apt-help))

;;;###autoload
(defun generic-apt ()
  "Create or switch to a generic-apt buffer."
  (interactive)
  (let ((generic-apt-exist-p (get-buffer generic-apt-buffer-name)))
    (switch-to-buffer generic-apt-buffer-name)
    (unless generic-apt-exist-p
      (generic-apt-mode))))


;;; Root Command, Buffer, Process Management

(defvar generic-apt-process nil)
(make-local-variable 'generic-apt-process)

(defvar generic-apt-running nil)
(make-local-variable 'generic-apt-running)

(defun generic-apt-update-cache ()
  "Update generic-apt cache saved in `generic-apt-cache-filename'."
  (interactive)
  (message "Updating generic-apt cache...")
  (funcall
   (intern
    (format "generic-apt-%S-update-available-pkgs"
            generic-apt-protocol)))
  (with-temp-buffer
    (if (and (not (string= generic-apt-cache-filename ""))
             (file-readable-p generic-apt-cache-filename))
        (insert-file-contents generic-apt-cache-filename)
      (insert ";;; automatically generated by generic-apt, edit with care!!\n\n"))
    (goto-char (point-min))
    (let ((str-name (format "generic-apt-%S-available-pkgs"
                            generic-apt-protocol)))
      (if (re-search-forward (format "(setq %s" str-name) nil t 1)
          (progn
            (backward-up-list)
            (kill-sexp))
        (goto-char (point-max)))
      (insert (format "(setq %s '%S)\n\n" str-name generic-apt-available-pkgs)))
    (write-region (point-min) (point-max) generic-apt-cache-filename))
  (message "Updating generic-apt cache...done"))

;; TODO, test this.
;; ;; initial variables
;; (if (file-readable-p generic-apt-cache-filename)
;;     (load-file generic-apt-cache-filename)
;;   (generic-apt-update-cache))

(defun generic-apt-process-sentinel (process event)
  "Set buffer read-only after a generic-apt command finishes."
  (setq generic-apt-running nil)
  (save-excursion
    (with-current-buffer (get-buffer generic-apt-buffer-name)
      (let ((inhibit-read-only t))
	(cond
	 ((eq (process-status process) 'exit)
	  (goto-char (point-max))
	  (insert "------------- done --------------\n"))
	 ((eq (process-status process) 'signal)
	  (message "generic-apt process killed")))))))

(defun generic-apt-process-filter (process output)
  "Filter generic-apt command outputs."
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process)))
	  (inhibit-read-only t)
	  (percentage-match "[0-9]\\{1,3\\}%"))
      (save-excursion
	(goto-char (process-mark process))
	(setq output
              (replace-regexp-in-string "\r" "\n" output))
	;; make percentage output nicer
;;         (cond ((string-match percentage-match output)
;;                (message "generic-apt: %s" output))
;;               ((string-match "^\\ +$\\|^\n$" output)
;;                nil)
;;               (t
;;                (forward-line 0)
;; ;;                (insert output)))
        (insert output)
	(set-marker (process-mark process) (point)))
      (and moving (goto-char (process-mark process))))))

(defun generic-apt-kill ()
  "Kill generic-apt process."
  (interactive)
  (when generic-apt-process
    (unless (eq (process-status generic-apt-process) 'exit)
      (delete-process generic-apt-process))
    (setq generic-apt-running nil)))

(defun generic-apt-run-command (args)
  (generic-apt-run-1
   (append (split-string generic-apt-command " ") args)))

(defun generic-apt-run-command-to-string (args-or-string)
  (shell-command-to-string
   (concat generic-apt-command
           " "
           (if (stringp args-or-string)
               args-or-string
             (mapconcat 'identity args-or-string " ")))))

(defun generic-apt-run-other-command (other-command-and-args)
  (let ((prefix
         ;; FIXME: Is there a better remove the last element of a list?
         (nreverse
          (cdr
           (nreverse
            (remove-if
             (lambda (i) (string= i ""))
             (split-string generic-apt-command " ")))))))
    (generic-apt-run-1
     (append prefix other-command-and-args))))

(defun generic-apt-run-1 (full-command-and-args)
  (let ((inhibit-read-only t))
    (generic-apt)
    (erase-buffer)
    (if generic-apt-running
	(error "Generic-Apt process already exists")
      (setq generic-apt-running t)
      (setq generic-apt-process
	    (apply 'start-process "generic-apt" generic-apt-buffer-name
                   full-command-and-args))
      (set-process-filter generic-apt-process 'generic-apt-process-filter)
      (set-process-sentinel generic-apt-process 'generic-apt-process-sentinel))))


;;; Commands

(defun generic-apt-help ()
  "Help page for `generic-apt-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     "Welcome to generic apt!

Here is a brief list of the most used commamnds:

    u - Selfupdate package database cache
    s - Search packages
    o - Describe a package
    i - Install a package
    u - Upgrade a package
    r - Remove a package
    E - Edit config file
")
    (message "For a list of all available commands, press `F1 m'.")))

(defun generic-apt-install-at-point ()
  "Install package at point."
  (interactive)
  (funcall
   (intern
    (format "generic-apt-%s-install" generic-apt-protocol))
   (current-word)))

(defun generic-apt-show-at-point ()
  "Run `generic-apt show' on current word(pkg name)."
  (interactive)
  (funcall
   (intern
    (format "generic-apt-%s-show" generic-apt-protocol))
   (current-word)))


;;; Required Backend Interface

(defun generic-apt-edit-sources ()
  "Edit /etc/apt/sources.list using `tramp'."
  (interactive)
  (let ((f (funcall
            (intern
             (format "generic-apt-%S-edit-sources" generic-apt-protocol)))))
    (when (string-match "^ssh" generic-apt-command)
      (let ((hostname "")
            (proxies tramp-default-proxies-alist)
            (i '()))
        (while proxies
          (setq i (car proxies)
                proxies (cdr proxies))
          (when (string-match (regexp-opt (list (car i)))
                              generic-apt-command)
            (setq hostname (car i)
                  (setq f (format "/ssh:%s:%s" hostname f)))
            (setq proxies nil)))))
    (find-file f)))

(defun generic-apt-search (pkg)
  "Search PKG by package name."
  (interactive "sSearch: ")
  (funcall
   (intern
    (format "generic-apt-%S-search" generic-apt-protocol))
   pkg))

(defun generic-apt-update ()
  "Update package database cache."
  (interactive)
  (funcall
   (intern
    (format "generic-apt-%S-update" generic-apt-protocol))))

(defun generic-apt-install (pkg)
  "Install PKG."
  (interactive
   (list
    (ido-completing-read "Install: " generic-apt-available-pkgs)))
  (funcall
   (intern
    (format "generic-apt-%S-install" generic-apt-protocol))))

(defun generic-apt-upgrade (pkg)
  "Upgrade PKG."
  (interactive
   (list
    (ido-completing-read "Upgrade: " generic-apt-available-pkgs)))
  (funcall
   (intern (format "generic-apt-%S-upgrade" generic-apt-protocol))
   pkg))

(defun generic-apt-remove (pkg)
  "Remove PKG."
  (interactive
   (list
    (ido-completing-read "Remove: " generic-apt-available-pkgs)))
  (funcall
   (intern (format "generic-apt-%S-remove" generic-apt-protocol))
   pkg))

(defun generic-apt-show (pkg)
  "Describe PKG."
  (interactive
   (list
    (ido-completing-read "Show: " generic-apt-available-pkgs)))
  (funcall
   (intern (format "generic-apt-%S-show" generic-apt-protocol))
   pkg))

;; generic-apt-%s-update-available-pkgs


;;; Utilities


;; ;; Compatibility

;; (defalias 'generic-apt-completing-read
;;   (if (and (fboundp 'ido-completing-read)
;; 	   ido-mode)
;;       'ido-completing-read		; added in Emacs 22
;;     'completing-read))


(provide 'generic-apt)

;;; generic-apt.el ends here
