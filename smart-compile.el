;;; smart-compile.el --- `compile' bases on major-mode or filename

;; Copyright (C) 2005, 2007 William Xu <william.xwl@gmail.com>

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 1.0

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

;; This is a mostly rewritten based on ideas from Seiji Zenitani
;; <zenitani@mac.com>'s `smart-compile.el'.
;;
;; Besides the original `smart-compile' function, i've add a `smart-run'
;; function. This two functions may be the most useful from this
;; extension.
;;
;; Let me illustrate it by an example. Suppose you are editing a file
;; `foo.c'. To compile it, try `M-x smart-compile', which will run a
;; shell command similar to `gcc -o foo foo.c -O2'. To run the
;; executable, say `foo', try `M-x smart-run', which will run a shell
;; command similar to `./foo'.
;;
;; To use, add the following to your .emacs:
;;
;;     (autoload 'smart-compile "smart-compile"
;;       "Run `compile' by checking project builder(like make, ant, etc) and
;;     `smart-compile-table'." t nil)
;;
;;     (autoload 'smart-run "smart-compile"
;;       "Run the executable program according to the file type.
;;     You can set `smart-run-table' and `smart-executable-table' to add new
;;     commands for new file types." t nil)
;;
;; And you may want to customzie these "triggers":
;; `smart-compile-table', `smart-run-table', `smart-executable-table'.

;;; Code:

(defgroup smart-compile nil
  "smart-compile extension."
  :prefix "smart-"
  :group 'smart-compile)

(defcustom smart-compile-table
  '((c-mode "gcc -O2 %f -lm -o %n")
    (c++-mode "g++ -O2 %f -lm -o %n")
    (java-mode "javac %f")
    ("\\.f90$" "f90 %f -o %n")
    ("\\.[Ff]$" "f77 %f -o %n")
    ("\\.pl$" "perl -cw %f")
    ("\\.mp$" "mptopdf %f")
    ("\\.php$" "php %f")
    ("\\.tex$" "latex %f")
    ("\\.l$" "lex -o %n.yy.c %f && gcc -O2 %n.yy.c -lm -o %n")
    ("\\.y$" "yacc -o %n.tab.c %f && gcc -O2 %n.tab.c -lm -o %n")
    ("\\.dot$" "dot -Tjpg %f > %n.jpg")
    (emacs-lisp-mode  (emacs-lisp-byte-compile)))
  "`smart-compile' running scheme.
Each element is of the form: (matcher . handler).  Matcher can be
either string(matching `buffer-file-name') or symbol(matching
`major-mode').  Hanlder can also be either string(a shell
command) or a lisp expression.

See also `smart-compile-replace-table'."
  :type 'symbol
  :group 'smart-compile)

(defcustom smart-compile-replace-table
  '(("%F" (buffer-file-name))
    ("%f" (file-name-nondirectory (buffer-file-name)))
    ("%n" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    ("%e" (file-name-extension (buffer-file-name))))
  "File name shortcut format.
Some special strings(like %f, %F) in `smart-compile-table',
`smart-run-table' and `smart-executable-table' will be replaced
according the following map(with an example in the end).

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )"
  :type 'symbol
  :group 'smart-compile)

(defcustom smart-run-table
  '(("\\.c$"          "./%n")
    ("\\.[Cc]+[Pp]*$" "./%n")
    ("\\.java$"       "java %n")
    ("\\.php$"        "php  %f")
    ("\\.m$"          "./%f")
    ("\\.scm"         "./%f")
    (latex-mode       "xdvi %n.dvi" t)
    ("\\.pl$"         "./%f"))
  "`smart-run' running scheme.
Each element is of the form: (matcher . handler).  Matcher can be
either string(matching `buffer-file-name') or symbol(matching
`major-mode').  Hanlder can also be either string(a shell
command) or a lisp expression.

See also `smart-compile-replace-table', `smart-executable-table'."
  :type 'symbol
  :group 'smart-compile)

(defcustom smart-executable-table
  '("%n.class"
    "%n"
    "%n.m"
    "%n.php"
    "%n.scm"
    "%n.dvi"
    "%n.info")
  "File formats that `smart-run' knows how to execute.

See also `smart-compile-replace-table'."
  :type 'symbol
  :group 'smart-compile)


(defvar smart-compile-check-makefile t)
(make-variable-buffer-local 'smart-compile-check-makefile)

(defun smart-compile-replace (str)
  "Replace in STR by `smart-compile-replace-table'."
  (dolist (el smart-compile-replace-table str)
    (setq str (replace-regexp-in-string (car el) (eval (cadr el)) str))))

;;;###autoload
(defun smart-compile ()
  "Run `compile' by checking project builder(like make, ant, etc) and
`smart-compile-table'."
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
                   (throw 'return t))
          (setq smart-compile-check-makefile nil)))
       ((file-readable-p "build.xml")   ; ant
        (if (y-or-n-p "build.xml is found. Try 'ant'? ")
            (progn (set (make-local-variable 'compile-command) "ant ")
                   (throw 'return t))
          (setq smart-compile-check-makefile nil)))))
    ;; smart-compile-table
    (mapc '(lambda (el)
             (let ((matcher (car el))
                   (handler (cadr el)))
               (when (or (and (stringp matcher)
                              (string-match matcher (buffer-file-name)))
                         (and (not (stringp matcher))
                              (eq matcher major-mode)))
                 (if (stringp handler)
                     (progn (set (make-local-variable 'compile-command)
                                 (smart-compile-replace handler))
                            (call-interactively 'compile))
                   (eval handler))
                 (throw 'return t))))
          smart-compile-table)
    (call-interactively 'compile)))

(defun smart-shell-command-asynchronously (cmd)
  (start-process-shell-command cmd nil cmd))

;;;###autoload
(defun smart-run ()
  "Run the executable program according to the file type.
You can set `smart-run-table' and `smart-executable-table' to add new
commands for new file types."
  (interactive)
  (let ((up-to-date nil)
        (bin-exist nil))
    (unless (buffer-file-name)
      (error "cannot get filename."))
    ;; Dose the executable file exist and up-to-date?
    (catch 'return
      (mapc '(lambda (el)
               (let ((bin (smart-compile-replace el)))
                 (when (file-exists-p bin)
                   (setq bin-exist t)
                   (unless (file-newer-than-file-p (buffer-file-name) bin)
                     (setq up-to-date t))
                   (throw 'return t))))
            smart-executable-table))
    (if (and (not up-to-date)
             (y-or-n-p "File out of date, recompile? "))
        (smart-compile)
      ;; smart-run-table
      (catch 'return
        (mapc '(lambda (el)
                 (let ((matcher (car el))
                       (handler (cadr el))
                       (async-run (caddr el)))
                   (when (or (and (stringp matcher)
                                  (string-match matcher (buffer-file-name)))
                             (and (not (stringp matcher))
                                  (eq matcher major-mode)))
                     (if (stringp handler)
                         (if async-run
                             (smart-shell-command-asynchronously
                              (smart-compile-replace handler))
                           (shell-command (smart-compile-replace handler)))
                       (eval handler))
                     (throw 'return t))))
              smart-run-table)))))

(provide 'smart-compile)

;;; smart-compile.el ends here
