;;; wajig.el --- an interface for wajig

;; Copyright (C) 2005 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; $Id: wajig.el,v 0.1 2005/09/13 16:10:47 xwl Exp $

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

;; I write this package as a wrapper for wajig for Emacs. I love living
;; within Emacs. :-)

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'wajig)
;;
;; Then, simply run `M-x wajig'.

;;; Change Log:

;; v 0.1 [2005/09/15 17:19:36] Initial version

;;; Todo:

;; 1. auto complete package name.
;; 2. how to kill prcocess
;; 3. better run shell command asynchronically.

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

(defmacro define-wajig-command (command &optional arglist)
  "Define a new wajig command. COMMAND is one of wajig commands,
such as help, update. ARGLIST is (pkg type). e.g.,

    (define-wajig-command help)
    (define-wajig-command show (pkg))
    (define-wajig-command install (pkg require-input))

PKG is the package name to operate on.

If REQUIRE-INPUT non-nil, then this command may need interactions with
user."
  (when (symbolp command)
    (setq command (symbol-name command)))
  (let* ((wajig-command (intern (format "wajig-%s" command)))
	 (docstring		      ; show help from `wajig commands'.
	  (progn
	    (string-match (format "^ %s.*" command)
			  wajig-commands-string)
	    (match-string 0 wajig-commands-string)))
	 (interactive
	  (if arglist
	      `(interactive ,(format "s$ wajig %s " command))
	    '(interactive)))
	 ;; arguments for wajig-command
	 (pkg (and arglist (car arglist)))
	 ;; require-input depends on pkg be t now.
	 (require-input (and pkg (cadr arglist))))
    `(progn
       (defun ,wajig-command ,(if pkg '(pkg) nil)
	 ,docstring
	 ,interactive
	 (wajig)
	 (set (make-local-variable 'buffer-read-only) nil)
	 (kill-region (point-min) (point-max))
	 (cond
	  (,require-input		; require interactions
	   (make-comint "wajig" "sudo" nil
			"wajig" ,command ,pkg))
	  (,pkg				; need one argument
	   ;; FIXME: Better `start-process' asynchronically, but i
	   ;; don't how to handle the outputs.
	   (call-process "sudo" nil "*wajig*" t
			 "wajig" ,command ,pkg))
	  (t				; need zero argument
	   (call-process "sudo" nil "*wajig*" t
			 "wajig" ,command)))
	 (unless ,require-input
	   (insert "------------- done --------------\n")
	   (set (make-local-variable 'buffer-read-only) t))))))

;; define wajig commands
(define-wajig-command help)
(define-wajig-command update)
(define-wajig-command commands)
(define-wajig-command list-commands)

(define-wajig-command show (pkg))
(define-wajig-command search (pkg))
(define-wajig-command list-names (pkg))
(define-wajig-command list-files (pkg))
(define-wajig-command find-file (pkg))

(define-wajig-command install (pkg t))
(define-wajig-command remove (pkg t))

;; more commands
(defun wajig-mode-help ()
  "Help page for wajig-mode."
  (interactive)
  (message "For a list of available commands, run `M-x describe-mode'."))

(defun wajig-search-by-name (pkg)
  "Run `wajig search -n pkg' or `apt-cache search -n pkg'."
  (interactive "s$ wajig search -n ")
  (wajig)
  (set (make-local-variable 'buffer-read-only) nil)
  (erase-buffer)
  (call-process "sudo" nil "*wajig*" t
		"wajig" "search" pkg "-n")
  (insert "------------- done --------------\n")
  (set (make-local-variable 'buffer-read-only) t))

;; (defun wajig-edit-sources ()
;;   "Edit `/etc/apt/sources.list'."
;;   (interactive)
;;   (find-file "/etc/apt/sources.list")
;;   )

;; install software at point.

(defun wajig-show-at-point ()
  "Run `wajig show' on current word(pkg name)."
  (interactive)
  (wajig-show (current-word)))

(defun wajig-source (pkg)
  "Run `cd ~/download && wajig source pkg'."
  (interactive "s$ cd ~/download && wajig source ")
  (wajig)
  (set (make-local-variable 'buffer-read-only) nil)
  (erase-buffer)
  (cd wajig-source-download-dir)
  (call-process "sudo" nil "*wajig*" t
		"wajig" "source" pkg)
  (insert "------------- done --------------\n")
  (set (make-local-variable 'buffer-read-only) t))

(defun wajig-source-download (pkg)
  "Run `cd ~/download && wajig source -d pkg'."
  (interactive "s$ cd ~/download && wajig source -d ")
  (wajig)
  (set (make-local-variable 'buffer-read-only) nil)
  (erase-buffer)
  (cd wajig-source-download-dir)
  (call-process "sudo" nil "*wajig*" t
		"wajig" "source" pkg "-d")
  (insert "------------- done --------------\n")
  (set (make-local-variable 'buffer-read-only) t))


;;; Wajig Mode

(defvar wajig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'wajig-mode-help)
    (define-key map "H" 'wajig-help)
    (define-key map "u" 'wajig-update)
    (define-key map "c" 'wajig-commands)
    ;; (define-key map "e" 'wajig-edit-sources)

    (define-key map "o" 'wajig-show)
    (define-key map "" 'wajig-show-at-point)
    (define-key map "s" 'wajig-search-by-name)
    (define-key map "S" 'wajig-search)
    (define-key map "l" 'wajig-list-names)
    (define-key map "L" 'wajig-list-files)
    (define-key map "f" 'wajig-find-files)

    (define-key map "i" 'wajig-install)
    (define-key map "R" 'wajig-remove)

    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "d" 'wajig-source)
    (define-key map "D" 'wajig-source-download)
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
  (switch-to-buffer "*wajig*")
  (unless (eq major-mode 'wajig-mode)
    (wajig-mode)))

(provide 'wajig)

;;; wajig.el ends here
