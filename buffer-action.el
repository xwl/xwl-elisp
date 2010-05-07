;;; buffer-action.el --- Perform actions(compile/run, etc) in buffer based on mode/filename

;; Copyright (C) 2005, 2007, 2008, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 3.4a
;; Url: http://xwl.appspot.com/ref/buffer-action.el

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

;; Perform actions(compile, run) based on buffer properties, such as major
;; mode, filename or any lisp expressions.  e.g.,

;;   foo.c: M-x buffer-action-compile => "gcc -o foo foo.c -O2"
;;          M-x buffer-action-run => "./foo"

;; What kind of shell commands or lisp expressions to call for each
;; action(compile/run) is configurable through `buffer-action-table'.

;; To use, add this file to your load-path and the following to your
;; .emacs:
;;       (autoload 'buffer-action-compile "buffer-action")
;;       (autoload 'buffer-action-run "buffer-action")

;;; History

;; This is a mostly rewritten based on ideas from Seiji Zenitani
;; <zenitani@mac.com>'s `smart-compile.el'. Besides compile action, i've add a
;; run action, and also extend buffer-action-table to accept more formats when
;; configuring a match.

;;; TODO

;; - Recognize Makefile in a directory, so don't bother me when calling
;;   the compile command from different file buffers, which all belongs
;;   to the same directory.

;;; Code:

(eval-when-compile (require 'cl))

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
  '((c-mode    "gcc -O2 %f -lm -o %n" "%n" "./%n")
    (c++-mode  "g++ -O2 %f -lm -o %n" "%n" "./%n")
    (java-mode "javac %n" "%n.class" "java %n")
    (makefile-mode "make" nil nil)
    ("\\.pl$" "perl -cw %f" nil "perl -s %f")
    ("\\.php$" nil t "php %f")
    ("\\.py$" nil t "python %f")
    ("\\.tex$" "latex %f" "%n.dvi" "xdvi %n.dvi &")
    (texinfo-mode (lambda ()
                    (save-excursion
                      ;; (texinfo-make-menu)
                      (texinfo-all-menus-update)
                      (texinfo-every-node-update)
                      (save-buffer))
                    (makeinfo-buffer))
                  "%n.info"
                  (lambda ()
                    (Info-revert-find-node
                     (replace-regexp-in-string
                      "\\.texinfo*$" ".info" (buffer-action-replace "%F"))
                     (makeinfo-current-node))))
    (emacs-lisp-mode (lambda ()
                       (byte-compile-file (buffer-action-replace "%f")))
                     "%n.elc"
                     eval-buffer)
    ("\\.info$" nil t (lambda () (info (buffer-file-name))))
    ("\\.dot$" "dot -Tjpg %f -o %n.jpg" "%n.png" "qiv %f &"))
  "Each element in the table has the form:

    '(MATCHER COMPILER-ACTION BIN RUN-ACTION)

MATCHER could be a filename, major mode, or predicative
thunk(functions with zero arguments).

BIN could be a filename, predicative thunk, t, nil.  This is used
for deciding whether a recompilation is necessary.  When it is a
filename, it will be used to compare with MATCHER, where
appropriate.

COMPILER-ACTION, RUN-ACTION is either a shell command or thunk.

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
  "Run `compile' by checking project builders and `buffer-action-table'.

Project builders are like make, ant, etc.  When running for the
first time, you can edit the command in minibuffer, then it would
use last command without bothering you any more.  If you want to
edit it again, please add C-u prefix."
  (interactive)
  (let* ((row (buffer-action-match))
         ;; Check whether BIN is latest.
         (latest (buffer-action-latest-p (nth 2 row))))
    (cond
     ;; 1. No need to recompile
     ((and (not current-prefix-arg) latest)
      (if (stringp latest)
          (message "`%s' is already latest" latest)
        (message "Everything up-to-date")))
     ;; 2. Compile for the first time, will be interactive
     ((or current-prefix-arg (not buffer-action-compile-action))
      (buffer-action-compile-setup row)
      (if (stringp buffer-action-compile-action)
          (progn
            (setq buffer-action-compile-action
                  (buffer-action-replace buffer-action-compile-action))
            (setq compile-command buffer-action-compile-action)
            (call-interactively 'compile)
            (setq buffer-action-compile-action compile-command))
        (funcall buffer-action-compile-action)))
     ;; 3. Just compile
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
    (setq buffer-action-run-action (nth 3 (buffer-action-match)))
    (if (stringp buffer-action-run-action)
        ;; FIXME: I'm unable to avoid using the obsolete INITIAL-CONTENTS
        ;; parameter, since I'd like the default line inserted and editable at
        ;; the same time.
        (progn
          (setq buffer-action-run-action
                (read-from-minibuffer
                 ;; "Run-action $ " nil nil t nil (concat run " ")))
                 "Run-action $ "
                 (concat (buffer-action-replace buffer-action-run-action) " ")))
          (buffer-action-shell-command buffer-action-run-action))
      (funcall buffer-action-run-action)))
   ((stringp buffer-action-run-action)
    (buffer-action-shell-command buffer-action-run-action))
   (t
    (funcall buffer-action-run-action))))


;;; Utilities

(defun buffer-action-replace (fmt)
  "Format FMT by looking at `buffer-action-replace-table'."
  (let ((ret fmt))
    (dolist (el buffer-action-replace-table)
      (let ((case-fold-search nil))
        (setq ret (replace-regexp-in-string
                   (car el) (funcall (cadr el)) ret))))
    ret))

(defun buffer-action-match ()
  "Retrieve the row matching against current buffer in `buffer-action-table'."
  (let ((table buffer-action-table)
        (row '())
        (ret nil))
    (condition-case nil
        (progn
          (while table
            (setq row (car table)
                  table (cdr table))
            (let ((matcher (nth 0 row)))
              (when (or (and (stringp matcher)
                             (string-match matcher (buffer-file-name)))
                        (and (symbolp matcher)
                             (eq matcher major-mode))
                        ;; Most major-mode are both a symbol and function...
                        (and (functionp matcher)
                             (not (symbolp matcher))
                             (funcall matcher)))
                (setq table nil)
                (setq ret row))))
          ret)
      (error "Action not found for current buffer"))))

(defun buffer-action-shell-command (cmd)
  "Run shell CMD.
When CMD ends with a `&', run it asynchronously using a unique output buffer,
whose window will be deleted automatically."
  (setq cmd (buffer-action-replace cmd))
  (when (fboundp 'convert-standard-filename)
    (setq cmd (convert-standard-filename cmd)))
  (if (string-match "&\\ *$" cmd)
      (let ((buf (generate-new-buffer-name (concat "*" cmd "*"))))
        (message cmd)
        (shell-command cmd buf)
        (delete-window (get-buffer-window buf)))
    (shell-command cmd)))

(defun buffer-action-latest-p (bin)
  "BIN is the element in `buffer-action-table'.

Check whether BIN is up to date.  Return filename for BIN when up
to date."
  (cond ((functionp bin)
         (let ((s (funcall bin)))
           (if (stringp s)
               (buffer-action-file-newer-than-file-p s)
             s)))
        ((stringp bin)
         (buffer-action-file-newer-than-file-p bin))
        (t
         bin)))

(defun buffer-action-file-newer-than-file-p (target-file)
  (when (and (file-exists-p target-file)
             (file-newer-than-file-p target-file (buffer-file-name)))
    target-file))

(defun buffer-action-compile-setup (row)
  "Setup correct compiler action.
ROW is matched one in `buffer-action-table'."
  (let ((cmd
         (some
          (lambda (el)
            (let ((f (find-if 'file-exists-p (car el))))
              (when f (concat (cadr el) " " f))))
          `((("Makefile" "makefile" "../Makefile" "../makefile") "make -C")
            (("build.xml") "ant")
            (,(directory-files "." nil "\\.pro$") "qmake")
            (("bld.inf" "../group/bld.inf") "sbs -c winscw_udeb -b")))))
    (if (and cmd (y-or-n-p (format "Run like this? `%s' " cmd)))
        (setq buffer-action-compile-action (concat cmd " "))
      (setq buffer-action-compile-action (nth 1 row)))))

(provide 'buffer-action)

;;; buffer-action.el ends here
