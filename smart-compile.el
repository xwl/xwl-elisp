;;; smart-compile.el --- an interface to `compile'

;; Copyright (C) 2005, 2007 William Xu <william.xwl@gmail.com>
;; Copyright (C) 1998-2003  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: 3.1
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 20, 21
;; URL: http://home.att.ne.jp/alpha/z123/elisp-e.html

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associates a particular file with a paticular compile functions,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 20 or later.

;;; Code:

;;   List of compile commands. In argument of `compile', some keywords
;; beginning with '%' will be replaced by:

;;   %F  absolute pathname            ( /usr/local/bin/netscape.bin )
;;   %f  file name without directory  ( netscape.bin )
;;   %n  file name without extention  ( netscape )
;;   %e  extention of file name       ( bin )
(defvar smart-compile-alist
  '(("\\.c$"          . "gcc -O2 %f -lm -o %n")
    ("\\.[Cc]+[Pp]*$" . "g++ -O2 %f -lm -o %n")
    ("\\.java$"       . "javac %f")
    ("\\.f90$"        . "f90 %f -o %n")
    ("\\.[Ff]$"       . "f77 %f -o %n")
    ("\\.pl$"         . "perl -cw %f")
    ("\\.mp$"	      . "mptopdf %f")
    ("\\.php$"        . "php %f")
    ("\\.tex$"        . "latex %f")
    ("\\.texi$"       . "makeinfo %f")
    (emacs-lisp-mode  . (emacs-lisp-byte-compile)))
  "Each element is of the form: (matcher . handler).
Matcher can be either string(matching `buffer-file-name') or
symbol(matching `major-mode').
Hanlder can also be either string(a shell command) or symbol(a lisp
expression)." )

(defvar smart-compile-replace-alist
  '(("%F" . (buffer-file-name))
    ("%f" . (file-name-nondirectory (buffer-file-name)))
    ("%n" . (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    ("%e" . (file-name-extension (buffer-file-name)))))

(defvar smart-compile-check-makefile t)
(make-variable-buffer-local 'smart-compile-check-makefile)

(defvar smart-run-alist
  '(("\\.c$"          . "./%n")
    ("\\.[Cc]+[Pp]*$" . "./%n")
    ("\\.java$"       . "java %n")
    ("\\.php$"	      . "php %f")
    ("\\.m$"	      . "./%f")
    ("\\.scm"         . "./%f")
    ("\\.tex$"        . "dvisvga %n.dvi")
    ;;    ("\\.texi$"       . "info %n.info")))
    (texinfo-mode     . (info (smart-compile-replace "%n.info")))))

(defvar smart-executable-alist
  '("%n.class"
    "%n"
    "%n.m"
    "%n.php"
    "%n.scm"
    "%n.dvi"
    "%n.info"))

(defun smart-compile-replace (str)
  "Replace the smart-compile-replace-alist."
  (let ((rlist smart-compile-replace-alist))
    (while rlist
      (while (string-match (caar rlist) str)
	(setq str (replace-match (eval (cdar rlist)) t nil str)))
      (setq rlist (cdr rlist))))
  str)

(defun smart-compile ()
  "Run `compile' by checking project builder(like make, ant, etc) and
`smart-compile-alist'."
  (interactive)
  (catch 'return
    (unless (buffer-file-name)
      (error "cannot get filename."))
    ;; project builders
    (when smart-compile-check-makefile
      (cond
       ((or (file-readable-p "Makefile") ; make
            (file-readable-p "makefile"))
        (if (y-or-n-p "Makefile is found. Try 'make'? ")
            (progn (set (make-local-variable 'compile-command) "make ")
                   (throw 'return))
          (setq smart-compile-check-makefile nil)))
       ((file-readable-p "build.xml")   ; ant
        (if (y-or-n-p "build.xml is found. Try 'ant'? ")
            (progn (set (make-local-variable 'compile-command) "ant ")
                   (throw 'return))
          (setq smart-compile-check-makefile nil)))))
    ;; smart-compile-alist
    (mapc '(lambda (el)
             (let ((matcher (car el))
                   (handler (cdr el)))
               (cond ((and (stringp matcher) (stringp handler))
                      (when (string-match matcher (buffer-file-name))
                        (set (make-local-variable 'compile-command)
                             (smart-compile-replace handler))
                        (call-interactively 'compile)
                        (throw 'return)))
                     ((and (stringp matcher) (symbolp handler))
                      (when (string-match matcher (buffer-file-name))
                        (eval handler)
                        (throw 'return)))
                     ((and (symbolp matcher) (stringp handler))
                      (when (eq matcher major-mode)
                        (set (make-local-variable 'compile-command)
                             (smart-compile-replace handler))
                        (call-interactively 'compile)
                        (throw 'return)))
                     ((and (symbolp matcher) (symbolp handler))
                      (when (eq matcher major-mode)
                        (eval handler)
                        (throw 'return))))))
          smart-compile-alist)
    (call-interactively 'compile)))

(defun smart-run ()
  "Run the executable program according to the file type.
You can set `smart-run-alist' and `smart-executable-alist' to add new
commands for new file types."
  (interactive)
  (let ((name (buffer-file-name))
	(alist smart-run-alist)
	(rlist smart-compile-replace-alist)
	(elist smart-executable-alist)
	(executable nil)
	(update t)
	(case-fold-search nil))

    (if (not name) (error "cannot get filename."))

    ;; dose the executable file exist and update?
    (let ((exfile (car elist)))
      (while (and elist (not executable))
	;; r is a local rlist
	(let ((r smart-compile-replace-alist))
	  (while r
	    (while (string-match (caar r) exfile)
	      (setq exfile
		    (replace-match
		     (eval (cdar r)) t nil exfile)))
	    (setq r (cdr r))))
	(let ((file (concat (file-name-directory (buffer-file-name))
			    exfile)))
	  (if (file-readable-p file)
	      (progn
		(if (file-newer-than-file-p name file)
		    (setq update nil))
		(setq executable t))
	    (setq exfile (cadr elist))))
	(setq elist (cdr elist))))

    (if (and executable update)
	(while alist
	  (let ((run-type (caar alist))
		(run-cmd (cdar alist)))
	    (cond ((and (symbolp run-type)
			(eq run-type major-mode))
		   (eval run-cmd))
		  ((and (stringp run-type)
			(string-match run-type name))
		   (progn (shell-command (smart-compile-replace run-cmd))
			  (setq alist nil)))
		  (t (setq alist (cdr alist))))))

      (if (and (not update) (y-or-n-p "File out of date, recompile? "))
	  (smart-compile)
	(if (y-or-n-p "Compile first? ")
	    (smart-compile))))))

(provide 'smart-compile)

;;; smart-compile.el ends here
