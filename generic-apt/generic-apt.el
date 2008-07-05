;;; generic-apt.el --- Generic apt alike interfaces for various package management tools

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
;; GNU/Linux) alike interface over various package management tools,
;; such as: apt-get(Debian GNU/Linux), yum(redhat/fedora), emerge(Gentoo
;; GNU/Linux), fink(Mac OS X), pkg-get(Solaris), etc.

;; Put generic-apt files into your load-path first.  Then add something similar
;; to the following example to your .emacs.  192.168.1.20 is a remote debian
;; machine, while localhost is a Mac OS X with fink installed.
;;
;;     ;; Add this so that we can edit file on remote machine as root.  Also
;;     ;; note that you should config your ssh agent not to prompt password
;;     ;; while logining the remote host.
;;
;;     (eval-after-load 'tramp
;;       '(progn
;;          (add-to-list 'tramp-default-proxies-alist
;;     		  '("192.168.1.20" "\\`root\\'" "/ssh:%h:"))
;;          ))
;;
;;     (require 'generic-apt-install)
;;     (add-hook 'generic-apt-mode-hook 'less-minor-mode-on)
;;
;;     (setq generic-apt-select-methods
;;           '((apt-get "ssh 192.168.1.20 sudo apt-get")
;;             (fink "sudo fink")))
;;
;; Then type: `M-x generic-apt'.

;;; Code:

(require 'tramp)
(eval-when-compile
  (require 'cl))

;;; Customizations

(defgroup generic-apt nil
  "Generic apt alike interfaces for various package management tools."
  :group 'generic-apt)

(defcustom generic-apt-mode-hook nil
  "Normal hook run after entering `generic-apt-mode'."
  :type 'hook
  :group 'generic-apt)

;; (defcustom generic-apt-source-download-dir "~/download"
;;   "Directory for saving source downloads."
;;   :type 'string
;;   :group 'generic-apt)

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

(defvar generic-apt-protocol nil)
(make-variable-buffer-local 'generic-apt-protocol)

(defvar generic-apt-command "")
(make-variable-buffer-local 'generic-apt-command)

(defvar generic-apt-buffer-name "")
(make-variable-buffer-local 'generic-apt-buffer-name)

(defvar generic-apt-available-pkgs '())
(make-variable-buffer-local 'generic-apt-available-pkgs)

(defvar generic-apt-font-lock-keywords nil
  "Keywords to highlight in generic-apt mode.")
(make-variable-buffer-local 'generic-apt-font-lock-keywords)

(defvar generic-apt-sources-file ""
  "Config file for the package management tool.")
(make-variable-buffer-local 'generic-apt-sources-file)


;;; Generic-Apt Mode

(defvar generic-apt-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Already defined for all protocols.
    (define-key map "h" 'generic-apt-help)
    (define-key map "I" 'generic-apt-install-at-point)
    (define-key map "" 'generic-apt-show-at-point)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "K" 'generic-apt-kill)
    (define-key map "E" 'generic-apt-edit-sources)

    ;; RFC for each protocol.
    (define-key map "u" 'generic-apt-update)
    (define-key map "s" 'generic-apt-search-by-name)
    (define-key map "S" 'generic-apt-search)
    (define-key map "o" 'generic-apt-show)
    (define-key map "i" 'generic-apt-install)
    (define-key map "l" 'generic-apt-listfiles)
    (define-key map "U" 'generic-apt-upgrade)
    (define-key map "C" 'generic-apt-clean)
    (define-key map "R" 'generic-apt-remove)
    map)
  "Keymap for `generic-apt-mode'.")

(defvar generic-apt-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st)
  "Syntax table used while in `generic-apt-mode'.")

(define-derived-mode generic-apt-mode nil "Generic-Apt"
  "Major mode for generic apt alike interfaces for various package management tools.
\\{generic-apt-mode-map}"
  (set-syntax-table generic-apt-mode-syntax-table)
  (setq font-lock-defaults '(generic-apt-font-lock-keywords))
  (setq buffer-read-only t)

  ;; Take special care with these two!
  (setq generic-apt-command generic-apt-command)
  (setq generic-apt-buffer-name generic-apt-buffer-name)

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

  (setq generic-apt-font-lock-keywords
        (intern (format "generic-apt-%S-font-lock-keywords"
                        generic-apt-protocol)))

  (setq generic-apt-sources-file
        (eval
         (intern (format "generic-apt-%S-sources-file"
                         generic-apt-protocol))))

  (setq generic-apt-available-pkgs
        (eval
         (intern (format "generic-apt-%S-available-pkgs"
                         generic-apt-protocol))))

  ;; initial variables
  (if (file-readable-p generic-apt-cache-filename)
      (load-file generic-apt-cache-filename)
    (generic-apt-update-cache))

  (run-hooks 'generic-apt-mode-hook)
  (generic-apt-help))

;;;###autoload
(defun generic-apt (&optional method)
  "Create or switch to a generic-apt buffer."
  (interactive)
  ;; Wrap around them so that even when current buffer is another
  ;; generic-apt buffer, we won't mess with its local variables.
  (let* ((generic-apt-command
          (or method
              (ido-completing-read "generic-apt: "
                                   (mapcar (lambda (i) (cadr i))
                                           generic-apt-select-methods))))
         (generic-apt-buffer-name
          (format "*Generic-Apt/%s*" generic-apt-command)))
    (switch-to-buffer generic-apt-buffer-name)
    (unless (eq major-mode 'generic-apt-mode)
      (generic-apt-mode))))


;;; Interfaces

(defun generic-apt-help ()
  "Help page for `generic-apt-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     "Welcome to generic apt -- The apt-get with *super* power!

Here is a brief list of the most useful commamnds:

    u - Selfupdate package database cache
    s - Search packages by name
    S - Search packages by content
    o - Describe a package
    i - Install a package
    l - List installed files by a package
    U - Upgrade a package
    R - Remove a package
    C - Cleanup
    E - Edit config file

------------- done --------------
")
    (message "For a list of all available commands, press `F1 m'.")))

(defun generic-apt-edit-sources ()
  "Edit /etc/apt/sources.list using sudo, with `tramp' when necessary."
  (interactive)
  (let ((f generic-apt-sources-file))
    (if (string-match "^ssh" generic-apt-command)
        (let ((hostname "")
              (proxies tramp-default-proxies-alist)
              (i '()))
          (while proxies
            (setq i (car proxies)
                  proxies (cdr proxies))
            (when (string-match (regexp-opt (list (car i)))
                                generic-apt-command)
              (setq hostname (car i)
                    f (format "/ssh:%s:%s" hostname f))
              (setq proxies nil)))
          (find-file f))
      (find-file (concat "/sudo::" f)))))

(defun generic-apt-search (pkg)
  "Search PKG by package name."
  (interactive "sSearch: ")
  (funcall (intern (format "generic-apt-%S-search" generic-apt-protocol))
           pkg))

(defun generic-apt-search-by-name (pkg)
  "Search PKG by package name, `-n'."
  (interactive "sSearch(by name): ")
  (funcall (intern (format "generic-apt-%S-search-by-name" generic-apt-protocol))
           pkg))

(defun generic-apt-update ()
  "Update package database cache."
  (interactive)
  (funcall (intern (format "generic-apt-%S-update" generic-apt-protocol))))

(defun generic-apt-install (pkg)
  "Install PKG."
  (interactive
   (list
    (ido-completing-read "Install: " generic-apt-available-pkgs)))
  (funcall (intern (format "generic-apt-%S-install" generic-apt-protocol))
           pkg))

(defun generic-apt-install-at-point ()
  "Install package at point."
  (interactive)
  (funcall (intern (format "generic-apt-%s-install" generic-apt-protocol))
           (current-word)))

(defun generic-apt-upgrade (pkg)
  "Upgrade PKG."
  (interactive
   (list
    (ido-completing-read "Upgrade: " generic-apt-available-pkgs)))
  (funcall (intern (format "generic-apt-%S-upgrade" generic-apt-protocol)) pkg))

(defun generic-apt-remove (pkg)
  "Remove PKG."
  (interactive
   (list
    (ido-completing-read "Remove: " generic-apt-available-pkgs)))
  (funcall (intern (format "generic-apt-%S-remove" generic-apt-protocol)) pkg))

(defun generic-apt-show (pkg)
  "Describe PKG."
  (interactive
   (list
    (ido-completing-read "Show: " generic-apt-available-pkgs)))
  (funcall (intern (format "generic-apt-%S-show" generic-apt-protocol)) pkg))

(defun generic-apt-show-at-point ()
  "Run `generic-apt show' on current word(pkg name)."
  (interactive)
  (funcall (intern (format "generic-apt-%s-show" generic-apt-protocol))
           (current-word)))

(defun generic-apt-upgrade-all ()
  "Upgrade all installed packages."
  (interactive)
  (funcall  (intern (format "generic-apt-%S-upgrade-all" generic-apt-protocol))))

(defun generic-apt-listfiles (pkg)
  "List files installed by PKG."
  (interactive
   (list
    (ido-completing-read "Listfiles: " generic-apt-available-pkgs)))
  (funcall (intern (format "generic-apt-%S-listfiles" generic-apt-protocol))
           pkg))

(defun generic-apt-clean ()
  "Clean cache."
  (interactive)
  (funcall (intern (format "generic-apt-%S-clean" generic-apt-protocol))))


;;; Root Command, Buffer, Process Management

(defvar generic-apt-process nil)
(make-variable-buffer-local 'generic-apt-process)

(defvar generic-apt-running nil)
(make-variable-buffer-local 'generic-apt-running)

(defun generic-apt-update-cache ()
  "Update generic-apt cache saved in `generic-apt-cache-filename'."
  (interactive)
  (message "Updating generic-apt cache...")
  (funcall (intern (format "generic-apt-%S-update-available-pkgs"
                           generic-apt-protocol)))
  (let ((protocol generic-apt-protocol)
        (pkgs generic-apt-available-pkgs))
    (with-temp-buffer
      (if (and (not (string= generic-apt-cache-filename ""))
               (file-readable-p generic-apt-cache-filename))
          (insert-file-contents generic-apt-cache-filename)
        (insert ";;; automatically generated by generic-apt, edit with care!!\n\n"))
      (goto-char (point-min))
      (let ((str-name (format "generic-apt-%S-available-pkgs" protocol)))
        (if (re-search-forward (format "(setq %s" str-name) nil t 1)
            (progn
              (backward-up-list)
              (kill-sexp))
          (goto-char (point-max)))
        (insert (format "(setq %s '%S)\n\n" str-name pkgs)))
      (write-region (point-min) (point-max) generic-apt-cache-filename))
    (message "Updating generic-apt cache...done")))

(defun generic-apt-process-sentinel (process event)
  "Set buffer read-only after a generic-apt command finishes."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (setq generic-apt-running nil)
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
  (generic-apt-run-1 (append (split-string generic-apt-command " ") args)))

(defun generic-apt-run-command-to-string (args-string)
  (shell-command-to-string (concat generic-apt-command " " args-string)))

(defun generic-apt-run-other-command (other-cmd-and-args)
  (generic-apt-run-1
   (append (split-string (generic-apt-extract-prefix)) other-cmd-and-args)))

(defun generic-apt-run-other-command-to-string (other-cmd-and-args-string)
  (shell-command-to-string
   (concat (generic-apt-extract-prefix) " " other-cmd-and-args-string)))

(defun generic-apt-extract-prefix ()
  "Extract prefix from `generic-apt-command'.

For instance, \"sudo fink\" => \"sudo\""
  (replace-regexp-in-string " ?[^ ]+$" "" generic-apt-command))

(defun generic-apt-run-1 (full-command-and-args)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if generic-apt-running
        (error "Generic-Apt process already exists")
      (setq generic-apt-running t)
      (setq generic-apt-process
            (apply 'start-process "generic-apt" generic-apt-buffer-name
                   full-command-and-args))
      (set-process-filter generic-apt-process 'generic-apt-process-filter)
      (set-process-sentinel generic-apt-process 'generic-apt-process-sentinel))))


;;; Utilities

(defun generic-apt-info-unsupported ()
  (message "Requested operation is not yet supported for `%S' backend"
           generic-apt-protocol))


;;; Compatibility

;; (defalias 'generic-apt-completing-read
;;   (if (and (fboundp 'ido-completing-read)
;; 	   ido-mode)
;;       'ido-completing-read		; added in Emacs 22
;;     'completing-read))


(provide 'generic-apt)

;;; generic-apt.el ends here
