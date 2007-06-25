;;; smart-compile.el --- `compile' and run based on major-mode or filename

;; Copyright (C) 2005, 2007 William Xu <william.xwl@gmail.com>

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 2.1

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
;; Besides the original `smart-compile' function, i've add a
;; `smart-compile-and-run' function. This two functions may be the most
;; useful from this extension.
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
;;     `smart-compile-table'." t)
;;
;;     (autoload 'smart-compile-and-run "smart-compile"
;;       "Run the executable program according to the file type.
;;     See `smart-compile-table'." t)
;;
;;     (autoload 'smart-compile-replace "smart-compile"
;;       "Replace in STR by `smart-compile-replace-table'." t)
;;
;; And you may want to customzie the "trigger" - `smart-compile-table'.

;;; Code:

(defgroup smart-compile nil
  "smart-compile extension."
  :prefix "smart-compile-"
  :group 'smart-compile)

(defcustom smart-compile-replace-table
  '(("%F" (buffer-file-name))
    ("%f" (file-name-nondirectory (buffer-file-name)))
    ("%n" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    ("%e" (file-name-extension (buffer-file-name))))
  "File name shortcut format.
Some special strings(like %f, %F) in `smart-compile-table', will
be replaced according the following map(with an example in the
end).

  %F  absolute pathname            (/usr/local/bin/netscape.bin)
  %f  file name without directory  (netscape.bin)
  %n  file name without extention  (netscape)
  %e  extention of file name       (bin)"
  :type 'symbol
  :group 'smart-compile)

(defcustom smart-compile-table
  '((c-mode "gcc -O2 %f -lm -o %n" "%n" "./%n")
    (c++-mode "g++ -O2 %f -lm -o %n" "%n" "./%n")
    ("\\.pl$" "perl -cw %f" nil "perl -s %f")
    ("\\.php$" nil nil "php %f")
    ("\\.tex$" "latex %f" "%n.dvi" "xdvi %n.dvi" t))
  "Each element in the table has the form:

    '(MATCHER COMPILE-HANDLER BIN RUN-HANDLER &optional ASYNC-RUN-P)

MATCHER, COMPILE-HANDLER, BIN and RUN-HANDLER could be either a
string or lisp expression.

MATCHER could either match against filename or major mode.

COMPILE-HANDLER is the command for compiling.

BIN is the object file created after compilation.

RUN-HANDLER is the command for running BIN.

Non-nil ASYNC-RUN-P will make RUN-HANDLER run asynchronously.

See also `smart-compile-replace-table'."
  :type 'symbol
  :group 'smart-compile)


;; Just run `smart-compile' for the first time, then fall back to
;; normal `compile' for future request. The reason is that user may edit
;; compile command in minibuffer manually.
(defvar smart-compile-first-time-p t)
(make-variable-buffer-local 'smart-compile-first-time-p)

;;;###autoload
(defun smart-compile-replace (str)
  "Replace in STR by `smart-compile-replace-table'."
  (dolist (el smart-compile-replace-table str)
    (setq str (replace-regexp-in-string (car el) (eval (cadr el)) str))))

;;;###autoload
(defun smart-compile ()
  "Run `compile' by checking project builder(like make, ant, etc) and
`smart-compile-table'."
  (interactive)
  ;; obj up-to-date ?
  (let ((up-to-date nil)
        (bin nil))
    (catch 'return
      (mapc (lambda (el)
              (let ((matcher (nth 0 el))
                    (b (nth 2 el)))
                (when (and b
                           (or (and (stringp matcher)
                                    (string-match matcher (buffer-file-name)))
                               (and (not (stringp matcher))
                                    (eq matcher major-mode))))
                  (setq bin (smart-compile-replace b))
                  (when (and (file-exists-p bin)
                             (file-newer-than-file-p bin (buffer-file-name)))
                    (setq up-to-date t)
                    (throw 'return t)))))
            smart-compile-table))
    (if up-to-date
        (message "`%s' is already up-to-date" (or bin "Object"))
      (if smart-compile-first-time-p
        (let ((compile-handler-string-p t))
          (cond ((and (or (file-exists-p "Makefile") ; make
                          (file-exists-p "makefile"))
                      (y-or-n-p "Found Makefile, try 'make'? "))
                 (setq compile-command "make "))
                ((and (file-exists-p "build.xml") ; ant
                      (y-or-n-p "Found build.xml, try 'ant'? "))
                 (setq compile-command "ant "))
                ((let ((pro (car (directory-files "." nil "\\.pro$")))) ; qmake
                   (and pro (y-or-n-p (format "Found %s, try 'qmake'? " pro))))
                 (setq compile-command "qmake "))
                (t
                 (catch 'return
                   (mapc (lambda (el)
                           (let ((matcher (nth 0 el))
                                 (compile-handler (nth 1 el)))
                             (when (or (and (stringp matcher)
                                            (string-match matcher (buffer-file-name)))
                                       (and (not (stringp matcher))
                                            (eq matcher major-mode)))
                               (if (stringp compile-handler)
                                   (setq compile-command (smart-compile-replace compile-handler))
                                 (setq compile-handler-string-p nil))
                               (throw 'return t))))
                         smart-compile-table))))
          (if compile-handler-string-p
              (call-interactively 'compile)
            (eval compile-handler))
          (setq smart-compile-first-time-p nil))
        (call-interactively 'compile)))))

(defun smart-compile-shell-command-asynchronously (cmd)
  (start-process-shell-command cmd nil cmd))

;;;###autoload
(defun smart-compile-run ()
  "Run the executable program according to the file type.
See `smart-compile-table'."
  (interactive)
  (let ((what-to-do nil))
    (catch 'return
      (mapc (lambda (el)
              (let ((matcher (nth 0 el))
                    (run-handler (nth 3 el))
                    (async-run-p (nth 4 el)))
                (when (or (and (stringp matcher)
                               (string-match matcher (buffer-file-name)))
                          (and (not (stringp matcher))
                               (eq matcher major-mode)))
                  (if (stringp run-handler)
                      (progn
                        (setq run-handler (smart-compile-replace run-handler))
                        (if async-run-p
                            (progn
                              (message "%s..." run-handler)
                              (smart-compile-shell-command-asynchronously run-handler))
                          (shell-command run-handler)))
                    (eval run-handler))
                  (setq what-to-do t)
                  (throw 'return t))))
            smart-compile-table))
    (unless what-to-do
      (call-interactively 'shell-command))))

(provide 'smart-compile)

;;; smart-compile.el ends here
