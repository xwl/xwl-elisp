;;; buffer-action.el --- Perform actions(compile/run, etc) in buffer based on mode/filename

;; Copyright (C) 2005, 2007, 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 3.1
;; Url: http://williamxu.net9.org/ref/buffer-action.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a mostly rewritten based on ideas from Seiji Zenitani
;; <zenitani@mac.com>'s `smart-compile.el'. Besides compile action, i've
;; add a run action, and maybe more in the future.

;; Let me illustrate it by an example. Suppose you are editing a file
;; named `foo.c'. To compile it, `M-x buffer-action-compile', it will run
;; a shell command similar to `gcc -o foo foo.c -O2'; to run the
;; executable binary `foo' , `M-x buffer-action-run', it will run a shell
;; command similar to `./foo'. Sounds neat, right?

;; What kind of shell commands or lisp expressions to call for each
;; action(compile/run) is configurable through `buffer-action-table'.

;; To use, add this file to your load-path and the following to your
;; .emacs:
;;       (autoload 'buffer-action-compile "buffer-action")
;;       (autoload 'buffer-action-run "buffer-action")

;; TODO

;; - Recongize Makefile in a directory, so don't bother me when calling
;;   the compile command from different file buffers, which all belongs
;;   to the same directory.
;; - Solve FIXME.

;;; Code:

;;; Customizations

(defgroup buffer-action nil
  "buffer-action extension."
  :prefix "buffer-action-"
  :group 'buffer-action)

(defcustom buffer-action-replace-table
  '(("%F" buffer-file-name)
    ("%f" (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%n" (lambda () (file-name-sans-extension
                      (file-name-nondirectory (buffer-file-name)))))
    ("%e" (lambda () (file-name-extension (buffer-file-name)))))
  "File name shortcut format.
Some special strings(like %f, %F) in `buffer-action-table', will
be replaced according the following map(with an example in the
end).

  %F  absolute pathname            (/usr/local/bin/netscape.bin)
  %f  file name without directory  (netscape.bin)
  %n  file name without extention  (netscape)
  %e  extention of file name       (bin)"
  :type 'symbol
  :group 'buffer-action)

(defcustom buffer-action-table
  '((c-mode "gcc -O2 %f -lm -o %n" "%n" "./%n")
    (c++-mode "g++ -O2 %f -lm -o %n" "%n" "./%n")
    (java-mode "javac %n" "%n.class" "java %n")
    (makefile-mode "make" nil nil)
    ("\\.pl$" "perl -cw %f" nil "perl -s %f")
    ("\\.php$" nil nil "php %f")
    ("\\.tex$" "latex %f" "%n.dvi" "xdvi %n.dvi &")
    (texinfo-mode
     (lambda ()
       (save-excursion
         ;; (texinfo-make-menu)
         (texinfo-all-menus-update)
         (texinfo-every-node-update)
         (save-buffer))
       (makeinfo-buffer))
     "%n.info"
     (lambda ()
       (Info-revert-find-node
        (replace-regexp-in-string "\\.texinfo*$" ".info" (buffer-action-replace "%F"))
        (makeinfo-current-node))))
    (emacs-lisp-mode
     (lambda ()
       (byte-compile-file (buffer-action-replace "%f")))
     "%n.elc"
     eval-buffer)
    ("\\.info$" nil nil (lambda () (info (buffer-file-name))))
    ("\\.dot$" "dot -Tjpg %f -o %n.jpg" "%n.png" "qiv %f &")
    )
  "Each element in the table has the form:

    '(MATCHER COMPILER-ACTION BIN RUN-ACTION)

MATCHER is either a filename or major mode.

BIN is usually a filename(string) or nil, it should be created by
COMPILER-ACTION when necessary, and will be executed by
RUN-ACTION.

COMPILER-ACTION, RUN-ACTION is either a shell command or lisp
expression.

See also `buffer-action-replace-table'."
  :type 'symbol
  :group 'buffer-action)


;;; Interface functions

(defvar buffer-action-compile-action nil)
(make-variable-buffer-local 'buffer-action-compile-action)

(defvar buffer-action-run-action nil)
(make-variable-buffer-local 'buffer-action-run-action)

;;;###autoload
(defun buffer-action-compile ()
  "Run `compile' by checking project builder(like make, ant, etc) and
`buffer-action-table'.

When running for the first time, you can edit the command in
minibuffer, else use last command without bothering you any
more. If you want to edit it again, please add C-u prefix."
  (interactive)
  (let* ((row (buffer-action-match))
         (bin (buffer-action-replace (nth 2 row)))
         (up-to-date                    ; Is BIN up-to-date ?
          (and (stringp bin)
               (file-exists-p bin)
               (file-newer-than-file-p bin (buffer-file-name)))))
    (cond
     ;; No need to recompile.
     ((and up-to-date (not current-prefix-arg))
      (message "`%s' is already up-to-date" (or bin "Object")))
     ;; Reset or Setup compile command and compile with new command.
     ((or current-prefix-arg (not buffer-action-compile-action))
      (cond
       ((and (or (file-exists-p "Makefile") ; make
                 (file-exists-p "makefile"))
             (y-or-n-p "Found Makefile, try 'make'? "))
        (setq buffer-action-compile-action "make "))
       ((and (file-exists-p "build.xml") ; ant
             (y-or-n-p "Found build.xml, try 'ant'? "))
        (setq buffer-action-compile-action "ant "))
       ((let ((pro (car (directory-files "." nil "\\.pro$")))) ; qmake
          (and pro (y-or-n-p (format "Found %s, try 'qmake'? " pro))))
        (setq buffer-action-compile-action "qmake "))
       (t
        (setq buffer-action-compile-action
              (buffer-action-replace (nth 1 row)))))
      (if (stringp buffer-action-compile-action)
          (progn
            ;; First time run will be interactive.
            (setq compile-command buffer-action-compile-action)
            (call-interactively 'compile)
            (setq buffer-action-compile-action compile-command))
        (funcall buffer-action-compile-action)))
     ;; Compile using previous compile command.
     ((stringp buffer-action-compile-action)
      (compile buffer-action-compile-action))
     (t
      (funcall buffer-action-compile-action)))))

;;;###autoload
(defun buffer-action-run ()
  "Run the binary file according to `buffer-action-table'.

When running for the first time, you can edit the command in
minibuffer, else use last command without bothering you any
more. If you want to edit it again, please add C-u prefix."
  (interactive)
  (cond
   ((or current-prefix-arg (not buffer-action-run-action))
    (let ((run (buffer-action-replace (nth 3 (buffer-action-match)))))
      (if (stringp run)
          ;; FIXME: I'm unable to avoid using the deprecated
          ;; INITIAL-CONTENTS parameter.
          (progn
            (setq buffer-action-run-action
                  (read-from-minibuffer
                   "Run-action $ " (concat run " ")))
            (buffer-action-shell-command))
        (setq buffer-action-run-action run)
        (funcall buffer-action-run-action))))
   ((stringp buffer-action-run-action)
    (buffer-action-shell-command))
   (t
    (funcall buffer-action-run-action))))


;;; Utilities

(defun buffer-action-replace (any)
  "If ANY is a string, update it by `buffer-action-replace-table', else
return ANY unchanged."
  (let ((ret any))
    (when (stringp any)
      (dolist (el buffer-action-replace-table ret)
        (setq ret (replace-regexp-in-string
                   (car el) (funcall (cadr el)) ret))))
    ret))

(defun buffer-action-match ()
  "Retrieve the row matching against current buffer in `buffer-action-table'."
  (let ((table buffer-action-table)
        (row '())
        (ret nil))
    (while table
      (setq row (car table)
            table (cdr table))
      (let ((matcher (nth 0 row)))
        (when (or (and (stringp matcher)
                       (string-match matcher (buffer-file-name)))
                  (eq matcher major-mode))
          (setq table nil)
          (setq ret row))))
    (condition-case nil                 ; abort
        (if ret
            ret
          (error))
      (error "%s" "Match nothing in `buffer-action-table'"))))

(defun buffer-action-shell-command ()
  "Run shell command either synchronously or asynchronously(when
with `&') with a unique output buffer, whose window will be
deleted automatically."
  (let ((cmd buffer-action-run-action))
    (if (string-match "&\\ *$" cmd)
        (let ((buf (generate-new-buffer-name (concat "*" cmd "*"))))
          (message cmd)
          (shell-command cmd buf)
          (delete-window (get-buffer-window buf)))
      (shell-command cmd))))

(provide 'buffer-action)

;;; buffer-action.el ends here
