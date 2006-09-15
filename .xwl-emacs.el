;;; .xwl-emacs.el --- love life, love zly!

;; Copyright (C) 2003, 2004, 2005, 2006 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 2.12
;; Last updated: 2006/09/15 23:53:49
;;; History

;; 2004/10/23 21:58:09
;;
;; Emacs, My Sweetheart
;;
;; Emacs, ooh, my sweetheart ~
;; ever since i got to know you,
;; life's become worthless without you,
;; you know how much i love you,
;; more than anything else,
;;
;; Emacs, ooh, my sweetheart ~
;; who brings me real happiness,
;; who teaches me to create and share,
;; who tells me to respect and honor,
;; who leads me to love life,
;;
;; Emacs, ooh, my sweetheart ~
;; who brings me into a brand new world,
;; who makes my life more convenient than ever,
;; who saves my soul as well as words,
;; who leads to enjoy life,
;;
;; Emacs, ooh, my sweetheart ~
;; my solely,lovely, honey,
;; i'll always be there,
;; loving you, forever...
;;
;;                                -- William Xu
;;
;; See `xwl-private.el' for more.

;;; Change Log

;; See `darcs changelog'.

;;; Code

;; a special version of emacs. (default is cvs version)
(setq xwl-emacs-special-p
      (string= (substring emacs-version 0 2) "21"))

(setq xwl-emacs-unicode-branch-p
      (string= (substring emacs-version 0 2) "23"))


;;; Pre

(defun xwl-emacs-reloaded-begin ()
  (interactive)
  ;; (shell-command "mplayer ~/share/sounds/default/emacs.ogg &")
  (delete-other-windows))

(xwl-emacs-reloaded-begin)

;; load path
;; ---------

(setq xwl-load-path
      '("~/studio/darcs/emms/"
	"~/studio/darcs/quail"
	"~/studio/darcs/elisp"
	;; "~/studio/arch/muse"
        ))

(setq xwl-recursive-load-path		; add paths recursively.
      '("~/.emacs.d/site-lisp"
	"~/.emacs.d/site-lisp-stable"	; use stable version for some
					; pkgs when bad things happen
        ;;
      ))

(setq load-path
      (append xwl-load-path
	      xwl-recursive-load-path
	      load-path))

(dolist (l xwl-recursive-load-path)
  (cd l)
  (normal-top-level-add-subdirs-to-load-path))

;; emacs unicode branch
(when xwl-emacs-unicode-branch-p
  (setq load-path
	(append load-path
		'("/usr/share/emacs/site-lisp/erc"
		  "/usr/share/emacs-snapshot/site-lisp/emacs-goodies-el"
		  "/usr/share/emacs-snapshot/site-lisp/muse-el"
		  "/usr/share/emacs-snapshot/site-lisp/dictionary-el"
                  "~/share/emacs/site-lisp"
                  "~/share/emacs/site-lisp/auctex"
                  "/usr/share/emacs/site-lisp/planner-el"
		  "/home/william/studio/cvs/emacs-w3m")))
  (cd "/home/william/studio/cvs/emacs-w3m")
  (normal-top-level-add-subdirs-to-load-path))

;;; STANDARD & CRUCIAL FEATURES

(load ".xwl-emacs-standard.el")
(load ".xwl-emacs-crucial.el")


;;; CONVENIENCE

(setq default-directory "~/")

(setq canlock-password "ac9f2efab48b539dd67c289f5eb50f1721edbd74")

;; general
;; -------

(require 'xwl-lib)

(defun xwl-chmod-file-executable ()
  "Make scripts executable if necessary."
  (let ((Filename (buffer-file-name))
	(cmd (shell-command-to-string
	      "which -a sh bash expect perl octave guile scsh python"))
	(exec-signal nil))

    (setq exec-signal
	  (concat "^#!\\ ?\\("
		  (replace-regexp-in-string "\n" "\\|"
					    (substring cmd 0 (1- (length cmd))) nil t)
		  "\\)"))

    ;; shell-command returns 0 if succeeds, or positive digit if fails.
    (save-excursion
      (goto-char (point-min))
      (if (and (looking-at exec-signal)
	       ;; remote test -x takes long time.
	       (null (eq major-mode 'emacs-wiki-mode)))
	  ;; is executable already ?
	  (if (zerop (shell-command (concat "test -x " Filename)))
	      (message (concat "Wrote " Filename))
	    (progn
	      (shell-command (concat "chmod u+x " Filename))
	      (message (concat "Wrote and made executable " Filename))))
	(message (concat "Wrote " Filename))))))


(defun his-scheme-eval-last-sexp (&optional insert)
  (interactive "P")
  (let ((standard-output (if insert (current-buffer) t))
        (cmd (buffer-substring (save-excursion (backward-sexp) (point)) (point))))
    (with-temp-buffer
      (comint-redirect-send-command-to-process
       cmd (current-buffer) (get-process "scheme") t nil)
      (while (string= (buffer-string) "") (sleep-for 0.01))
     (princ (buffer-string)))))

(defun xwl-run-scsh (&optional scheme?)
  "`run-scsh', rename it to scsh.

If SCHEME?, `run-scheme'."
  (interactive "P")
  (if scheme?
      (run-scheme "mzscheme")
    (progn
      (run-scsh nil)
      (rename-buffer "*scsh*"))))

;; info
(setq Info-directory-list
      (append '("~/info/")
	      Info-default-directory-list))

;; todo
(setq todo-file-do   "~/.todo/do"
      todo-file-done "~/.todo/done"
      todo-file-top  "~/.todo/top")

(defun xwl-todo-mode-hook ()
  (local-set-key (kbd "a") 'todo-add-category)
  (local-set-key (kbd "i") 'todo-insert-item-here)
  (local-set-key (kbd "I") 'todo-insert-item)
  (local-set-key (kbd "e") 'todo-edit-item)
  (local-set-key (kbd "D") 'todo-delete-item))

(add-hook 'todo-mode-hook 'xwl-todo-mode-hook)

;; abbreviations
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(abbrev-mode t)

;; template
(require 'template)
(setq template-default-directories
      (append '("~/.templates")
	      template-default-directories)
      template-date-format "%02m-%02d-%Y %02H:%02M:%02S"
      template-auto-insert t)
(setq template-expansion-alist
      '(("nick_name" (insert "xwl"))))
(template-initialize)

(defadvice template-new-file (around inhibit-less)
  "Inhibit `xwl-toggle-find-file-less' temporarily."
  (let ((less-on? (memq 'less-minor-mode-on find-file-hook)))
    (when less-on?
      (xwl-toggle-find-file-less))
    ad-do-it
    (when less-on?
      (xwl-toggle-find-file-less))))

(ad-activate 'template-new-file)

;; key solutions: C-up, C-down, etc.
(unless window-system
  (load "linux")
  (load "kn-prefix")
  (load "kn-prefix-autoloads"))

;; back up
(setq version-control t
      kept-old-versions 2
      kept-new-versions 5
      delete-old-versions t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t)

(setq backup-directory-alist
      '(("." . "~/src/backup/.emacs.backups" )))

;; screen-lines
(require 'screen-lines)
(screen-lines-mode 1)
(setq screen-lines-minor-mode-string nil)

;; compilation-mode, grep-find
;; (defun xwl-compilation-mode-hook ()
;;     (define-key compilation-mode-map (kbd "p") 'previous-line)
;;     (define-key compilation-mode-map (kbd "n") 'next-line)
;;     (define-key compilation-mode-map (kbd "q") 'xwl-hide-buffer))

;; (add-hook 'compilation-mode-hook 'xwl-compilation-mode-hook)

;; which week at school
;; --------------------

;; date of Monday in the first week.
(setq xwl-week-at-school-start '(2 20 2006))

(defun xwl-week-at-school (start)
  "Return which week at school. START is the date of Monday in
the first week. e.g, '(9 12 2005) sets \"Sep 12, 2005\"  as the
first Monday."
  (1+ (/ (- (calendar-absolute-from-gregorian (calendar-current-date))
	    (calendar-absolute-from-gregorian start)) 7)))

(setq xwl-week-at-school-format "第%d周"
      xwl-week-at-school-string
      (format xwl-week-at-school-format
	      (xwl-week-at-school xwl-week-at-school-start)))

(defun xwl-week-at-school-update ()
  "Update `xwl-week-at-school-string' on mode line."
  (setq xwl-week-at-school-string
	(format xwl-week-at-school-format
		(xwl-week-at-school xwl-week-at-school-start)))
  (force-mode-line-update))

;; (run-with-timer 0 3600 'xwl-week-at-school-update)

;; tramp
;; tramp (setq file-name-handler-alist nil)
; /telnet:william@localhost:~/

;; /ftp:movie9@denniszz.net9.org#9021:/

;; (setq tramp-default-method-alist
;;       '(("" "%" "smb")
;; 	("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")
;; 	("^ftp" "" "ftp")
;; 	("\\`localhost\\'" "\\`root\\'" "sudo")))

;; (setq tramp-default-method "sudo"
;;       password-cache-expiry 300)

(setq tramp-auto-save-directory "~/.tramp-auto-save-directory")

;; /sudo:root@localhost:/etc/X11/

;; flyspell
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode 1)
;; (add-hook 'cperl-mode-hook    'flyspell-prog-mode 1)
;; (add-hook 'makefile-mode-hook 'flyspell-prog-mode 1)
;; (add-hook 'python-mode-hook   'flyspell-prog-mode 1)
;; (add-hook 'sh-mode-hook       'flyspell-prog-mode 1)

(setq ange-ftp-ftp-program-name "ftp"
      ange-ftp-gateway-ftp-program-name "ftp"
      ftp-program "ftp")

;; dired
(defun xwl-dired-w3m-find-file ()
  (interactive)
  (let ((file (file-name-nondirectory (dired-get-filename))))
    (if (y-or-n-p (format "Use emacs-w3m to browse %s? " file))
	(w3m-find-file file))))

(defun xwl-dired-get-filename-no-extention ()
  "In dired, return the filename without extentions. eg.
\"abc.txt\"  --> \"abc\". Right now the filename should only
be with length 3 extentions !"
  (interactive)
  (let ((filename (file-name-nondirectory (dired-get-filename))))
    (message (substring filename 0 (- (length filename) 4)))))

;;   (set-process-sentinel (get-buffer-process (current-buffer))
;; 			#'xwl-shell-mode-kill-buffer-on-exit))
;; (defun xwl-shell-mode-kill-buffer-on-exit (process state)
;;   (kill-buffer (current-buffer)))

;; xwl-metafont-mode-hook
(defun xwl-metapost-mode-hook ()

  ;;   (defun xwl-metapost-mptopdf ()
  ;;     (interactive)
  ;;     (shell-command (concat "mptopdf " (buffer-file-name))))

  (defun xwl-metapost-preview (STR)
    (interactive "sPreview fig-")
    (let* ((file (file-name-sans-extension
		  (file-name-nondirectory (buffer-file-name))))
	   (file-pdf (concat file "-" STR ".pdf")))
      (shell-command (concat "xpdf " file-pdf))))

  (defun xwl-metapost-pdftopng ()
    (interactive)
    (let* ((file (file-name-sans-extension
		  (file-name-nondirectory (buffer-file-name))))
	   (i 1)
	   (file-pdf (concat file "-" (int-to-string i) ".pdf"))
	   (file-png (concat file "-" (int-to-string i) ".png")))

      (while (file-exists-p file-pdf)
	(if (file-newer-than-file-p file-pdf file-png)
	    (shell-command (concat "pdftoppm " file-pdf " tmp;\
                  convert tmp-000001.ppm " file-png ";\
                  rm -f tmp-000001.ppm")))
	(setq i (1+ i)
	      file-pdf (concat file "-" (int-to-string i) ".pdf")
	      file-png (concat file "-" (int-to-string i) ".png")))

      (message (concat file "-*.pdf have been converted to " file "-*.png."))))

  (auto-revert-mode -1)

  (xwl-text-mode-hook)
  (local-set-key (kbd ":") (lambda () (interactive) (smart-insert-operator ":")))
  ;;   (local-set-key (kbd "<C-up>") 'xwl-metapost-mptopdf)
  (local-set-key (kbd "<C-down>") 'xwl-metapost-preview)
  (local-set-key (kbd "<C-left>") 'xwl-metapost-pdftopng))

;; (autoload 'typing-of-emacs "The Typing Of Emacs, a game." t)

;; (require 'faith)

;; fvwm-mode
(autoload 'fvwm-mode "fvwm-mode" "Mode for editing fvwm files" t)

;; visual blank, tab, end-of-line ?
;(require 'blank-mode)

;; TeX
(require 'tex-site)
(require 'preview)

(defun xwl-LaTex-mode-hook ()
  (set (make-local-variable 'outline-regexp) "\%\%\%+ ")
  (outline-minor-mode 1)
  (smart-insert-operator-hook)

  (define-key LaTeX-mode-map (kbd "<C-up>") 'TeX-command-master))

(add-hook 'LaTeX-mode-hook 'xwl-LaTex-mode-hook)

(setq TeX-command-list
      '(("TeX" "%(PDF)%(tex) %S%(PDFout) \"%(mode)\\input %t\"" TeX-run-TeX nil
	 (plain-tex-mode ams-tex-mode texinfo-mode)
	 :help "Run plain TeX")
	("LaTeX" "%l \"%(mode)\\input{%t}\"" TeX-run-TeX nil
	 (latex-mode doctex-mode)
	 :help "Run LaTeX")
	("Makeinfo" "makeinfo %t" TeX-run-compile nil
	 (texinfo-mode)
	 :help "Run Makeinfo with Info output")
	("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
	 (texinfo-mode)
	 :help "Run Makeinfo with HTML output")
	("AmSTeX" "%(PDF)amstex %S%(PDFout) \"%(mode)\\input %t\"" TeX-run-TeX nil
	 (ams-tex-mode)
	 :help "Run AMSTeX")
	("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
	 (context-mode)
	 :help "Run ConTeXt once")
	("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
	 (context-mode)
	 :help "Run ConTeXt until completion")
	("ConTeXt Clean" "texutil --purgeall" TeX-run-interactive nil
	 (context-mode)
	 :help "Clean temporary ConTeXt files")
	("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
	("View" "%V" TeX-run-discard t t :help "Run Viewer")
	("Print" "%p" TeX-run-command t t :help "Print the file")
	("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
	("File" "%(o?)dvipdf %d" TeX-run-command t t :help "Generate PostScript file")
	("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
	("Check" "lacheck %s" TeX-run-compile nil
	 (latex-mode)
	 :help "Check LaTeX file for correctness")
	("Spell" "<ignored>" TeX-run-ispell-on-document nil t :help "Spell-check the document")
	("Other" "" TeX-run-command t t :help "Run an arbitrary command")))

;; metafont
(add-hook 'metapost-mode-hook 'xwl-metapost-mode-hook)

;; mmm-mode
;; (set-face-background 'mmm-code-submode-face nil)
;; (set-face-background 'mmm-default-submode-face nil)

;; emacs uptime
;;(require 'uptimes)
;;(global-set-key (kbd "C-c m u") 'uptimes-this)

;; misc

(defun xwl-browse-url-firefox-tab-only (url &optional new-window)
  "NEW-WINDOW is always nil."
  (xwl-start-process-shell-command
   (concat "firefox -new-tab " url)))

(setq browse-url-browser-function
      (if window-system
          'xwl-browse-url-firefox-tab-only
        'w3m-browse-url))

(global-set-key (kbd "C-c n b") 'browse-url)

;; enable tab completion for `shell-command'.
;; (setq shell-command-enable-completions t)
;; (shell-command-activate-advices)
;; (setq shell-command-prompt "%w%$ ")

;; fortune
(defun xwl-fortune-of-the-day ()
  "$ fortune-zh"
  (interactive)
  (message
   (ansi-color-filter-apply
    (shell-command-to-string "fortune-zh"))))

(global-set-key (kbd "C-c m f") 'xwl-fortune-of-the-day)

;; bookmark
;; --------

(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional second arg DEFAULT is a string to return if the user enters
the empty string."
  (bookmark-maybe-load-default-file)	; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt (bookmark-all-names))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
	   (default default)
	   (prompt (if default
		       (concat prompt (format " (%s): " default))
		     (concat prompt ": ")))
	   (str
	    (ido-completing-read prompt
				 bookmark-alist
				 nil
				 0
				 nil
				 'bookmark-history)))
      (if (string-equal "" str) default str))))

;; named-let, Thanks to Riastradh@#emacs
(defmacro his-named-let (name parameters &rest body)
  `(labels
       ((,name ,(mapcar 'car parameters) ,@body))
     (,name ,@(mapcar 'cadr parameters))))

(defun xwl-dired-wvHtml ()
  (concat "wvHtml --charset=gb2312 * "
	  (xwl-dired-get-filename-no-extention) ".html"))

(defun xwl-start-process-shell-command (cmd)
  "Don't create a separate output buffer.
This is run asynchronously, compared to `shell-command'."
  (start-process-shell-command cmd nil cmd))

;; redefine this function to disable output buffer.
(defun dired-run-shell-command (command)
  (let ((handler
	 (find-file-name-handler
	  (directory-file-name default-directory)
	  'shell-command)))
    (if handler
	(apply handler 'shell-command (list command))
      (xwl-start-process-shell-command command)))
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)

(setq dired-guess-shell-alist-user
      `((,(regexp-opt
	   '(".mp3" ".ogg" ".wav" ".avi" ".mpg" ".dat" ".wma" ".asf"
	     ".rmvb" ".rm" ".mkv"))

	 (progn (emms-add-file (dired-get-filename))
		(keyboard-quit)))

	(,(regexp-opt
	   '(".gif" ".png" ".bmp" ".jpg" ".tif" ".jpeg"))
	 "xzgv")

	(".htm[l]?" "firefox")
	(".dvi" "xdvi")
	(".rar" "unrar x")
	(".pdf" "xpdf")
	(".doc" (xwl-dired-wvHtml))
	(".ppt" "openoffice")
	(".chm" "xchm")))

;; last dir
(setq xwl-dired-visited-dirs-max 10	; max number of visited dirs
      xwl-dired-visited-dirs
      (make-vector xwl-dired-visited-dirs-max nil))

(setq xwl-dired-visited-dirs-pos 0)	; current position

(defun xwl-dired-save-visited-dirs ()
  "Save at most `xwl-dired-visited-dirs-max' visited dirs."
  (interactive)
  (aset xwl-dired-visited-dirs xwl-dired-visited-dirs-pos dired-directory)
  (setq xwl-dired-visited-dirs-pos (1+ xwl-dired-visited-dirs-pos))
  (when (= xwl-dired-visited-dirs-pos xwl-dired-visited-dirs-max)
    (setq xwl-dired-visited-dirs-pos 0)))

;(add-hook 'dired-after-readin-hook 'xwl-dired-save-visited-dirs)

(defun xwl-dired-last-dir ()
  "Dired on last visited dir."
  (interactive)
  (if (zerop xwl-dired-visited-dirs-pos)
      (setq xwl-dired-visited-dirs-pos (1- xwl-dired-visited-dirs-max))
    (setq xwl-dired-visited-dirs-pos (1- xwl-dired-visited-dirs-pos)))
  (dired (aref xwl-dired-visited-dirs xwl-dired-visited-dirs-pos)))

;; desktop
;; (require 'desktop)
;; (desktop-save-mode 1)
;; (setq desktop-files-not-to-save ;; "^/[^/:]*:"
;;       (regexp-opt
;;        '("do" ".bbdb" "*scratch*" "*Bookmark List*")))
;; (setq desktop-modes-not-to-save
;;       '(dired-mode
;; 	fundamental-mode))
;; (setq desktop-path '("~"))
;; (global-set-key (kbd "C-c n d") 'desktop-read)
;; (defun xwl-auto-save-desktop ()
;;   (interactive)
;;   (desktop-save "~/"))



(defun xwl-term (&optional create)
  (interactive)
  (cond
   (create
    (term "/bin/bash")
    (rename-buffer "*terminal*" t))
   ((not (get-buffer "xwl-term"))
    (term "/bin/bash")
    (rename-buffer "xwl-term" t))
   (t (switch-to-buffer "xwl-term"))))

;; repair some missed keys on console
(require 'wx-key)

;; Chinese
;; -------

;; (define-coding-system-alias 'gbk 'gb2312)

(require 'chinese-gbk)
(when window-system
  (progn
    (require 'characters-gbk)
    (require 'fontset-gbk)
    ;; use Chinese instead of Japanese charsets to decode utf-8
    ;;   (require 'gbk-utf-mode)
    ;;   (utf-translate-cjk-mode 1))
    ))

;; (when (string= (substring emacs-version 0 2) "21")
;;   (set-terminal-coding-system 'chinese-gbk)
;;   (set-keyboard-coding-system 'chinese-gbk))

;; chinese-wubi
(require 'wubi)
(register-input-method
 "chinese-wubi" "Chinese-GB" 'quail-use-package "wubi" "wubi")
(wubi-load-local-phrases)
(setq default-input-method "chinese-wubi")

(setq xwl-input-methods
      '("chinese-wubi"
	"japanese")
      xwl-current-input-methods xwl-input-methods)

(defun xwl-cycle-input-method ()
  "Cycle `xwl-input-method-alist'."
  (interactive)
  (if (null (cdr xwl-current-input-methods))
      (setq xwl-current-input-methods xwl-input-methods)
    (setq xwl-current-input-methods (cdr xwl-current-input-methods)))
  (set-input-method (car xwl-current-input-methods)))

;; (global-set-key (kbd "C-SPC") 'toggle-input-method)
(global-set-key (kbd "C-/") 'toggle-input-method)
(global-set-key (kbd "C-?") 'xwl-cycle-input-method)
(global-set-key (kbd "C-,") 'wubi-toggle-quanjiao-banjiao)

;; fonts
;; (when window-system
;;   (unless (fboundp 'xwl-setup-font)
;;     (create-fontset-from-fontset-spec
;;      "-*-bitstream vera sans mono-medium-r-normal--0-110-*-*-*-*-fontset-bvsmono110,
;; chinese-gb2312:-*-SimSun-medium-r-normal-*-16-*-*-*-*-*-gb2312.1980-0,
;; chinese-gbk:-*-SimSun-medium-r-normal-*-16-*-*-*-*-*-gbk-0,
;; chinese-cns11643-5:-*-SimSun-medium-r-normal-*-16-*-*-*-*-*-gbk-0,
;; chinese-cns11643-6:-*-SimSun-medium-r-normal-*-16-*-*-*-*-*-gbk-0,
;; chinese-cns11643-7:-*-SimSun-medium-r-normal-*-16-*-*-*-*-*-gbk-0")

;;     (unless xwl-emacs-unicode-branch-p
;;      (set-default-font "fontset-bvsmono110"))
;;     (defun xwl-setup-font() 'font-setup-done)))


;;; PROGRAMMING

;; darcs
(setq darcs-command-prefix (kbd "C-c d"))
(require 'darcs)

(defadvice darcs-changes (after enter-changlog-mode)
  "$ darcs changes, then enter `changelog-mode' and enable
`less-minor-mode'."
  (change-log-mode)
  (less-minor-mode-on))

(ad-activate 'darcs-changes)

; (require 'darcsum)

(require 'vc-darcs)
(add-to-list 'vc-handled-backends 'DARCS)

;; shell-script/conf mode
(defun xwl-sh-mode-hook()
  (defun xwl-sh-run ()
    (interactive)
    (shell-command
     (concat "sh "
	     (xwl-resolve-file-name (buffer-file-name) "f"))))

  (set (make-local-variable 'outline-regexp) "###+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "<C-down>") 'xwl-sh-run))

(add-hook 'sh-mode-hook 'xwl-sh-mode-hook)

;;TODO, add same to conf mode

;; pascal
(require 'pascal)
(defun xwl-pascal-mode-hook ()
  (define-key pascal-mode-map (kbd "=") (lambda () (interactive) (smart-insert-operator "=")))
  (define-key pascal-mode-map (kbd "+") (lambda () (interactive) (smart-insert-operator "+")))
  (define-key pascal-mode-map (kbd "-") (lambda () (interactive) (smart-insert-operator "-")))
  (define-key pascal-mode-map (kbd "/") (lambda () (interactive) (smart-insert-operator "/")))
  (define-key pascal-mode-map (kbd "%") (lambda () (interactive) (smart-insert-operator "%")))
  (define-key pascal-mode-map (kbd "&") (lambda () (interactive) (smart-insert-operator "&")))
  (define-key pascal-mode-map (kbd "*") (lambda () (interactive) (smart-insert-operator "*")))
  (define-key pascal-mode-map (kbd "!") (lambda () (interactive) (smart-insert-operator "!")))
  (define-key pascal-mode-map (kbd "|") (lambda () (interactive) (smart-insert-operator "|")))
  (define-key pascal-mode-map (kbd "<") (lambda () (interactive) (smart-insert-operator "<")))
  (define-key pascal-mode-map (kbd ">") (lambda () (interactive) (smart-insert-operator ">")))
  (define-key pascal-mode-map (kbd ",") (lambda () (interactive) (smart-insert-operator "," t)))
  (define-key pascal-mode-map (kbd ":") (lambda () (interactive) (smart-insert-operator ":"))))

(xwl-pascal-mode-hook)

;; sql
(require 'xwl-private)
;(load-file "~/.emacs.d/site-lisp/william/wx-passwd.el")

(require 'sql)

(setq sql-mysql-program "mysql"
      sql-user          "root"
      sql-password      pwsql
      sql-database      ""
      sql-server        "localhost")

(add-hook 'sql-mode-hook 'smart-insert-operator-hook)
(add-hook 'sql-interactive-mode-hook 'smart-insert-operator-hook)

(global-set-key (kbd "C-c c m") 'sql-mysql)

(define-key sql-interactive-mode-map (kbd "RET")
  (lambda () (interactive) (insert ";") (comint-send-input)))

;; gdb
(setq gdb-many-windows t)
(global-set-key (kbd "<C-end>") 'gdb-restore-windows)

;;;; c, c++, java

;; doxymacs
(require 'doxymacs)

;; cc-mode
(require 'ctypes)
(ctypes-auto-parse-mode 1)

(autoload 'expand-member-functions
  "member-functions" "Expand C++ member function declarations" t)

(require 'lisp-mnt)
(require 'srfi)


;;; INTERFACES

;; wget
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(load "w3m-wget")
(autoload 'w3-wget "w3-wget" "wget interface for Emacs/W3." t)
;(setq wget-basic-options (cons "-equiet=off" wget-basic-options))
;(setq wget-basic-options (cons "-P." wget-basic-options))
(setq wget-download-directory "~/download/"
      wget-default-options '("-nd" "-nc" "-c" "-r" "--proxy=off"))

;; octave
(autoload 'octave-help "octave-hlp" nil t)

(defun xwl-octave-mode-hook ()
  (smart-insert-operator-hook))

(add-hook 'octave-mode-hook 'xwl-octave-mode-hook)

;; sawfish
; (load-file "/usr/share/emacs/site-lisp/sawfish/sawfish.el") ; oops
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(defun xwl-sawfish-mode-hook ()
  (local-set-key (kbd "C-c <f1>  i")   'sawfish-info)
  (local-set-key (kbd "C-c <f1>  C-v") 'sawfish-info-variable)
  (local-set-key (kbd "C-c <f1>  v")   'sawfish-describe-variable)
  (local-set-key (kbd "C-c <f1>  C-f") 'sawfish-info-function)
  (local-set-key (kbd "C-c <f1>  f")   'sawfish-describe-function)
  (local-set-key (kbd "C-c <f1>  a")   'sawfish-apropos)

  (local-set-key (kbd "ESC TAB") 'sawfish-complete-symbol))

(add-hook 'sawfish-mode-hook 'xwl-sawfish-mode-hook)

;; ispell
(setq ispell-alternate-dictionary "/usr/share/dict/words")
;;      ispell-personal-dictionary "/home/william/.ispell_william")

;; (mapcar* '(lambda (hook) (add-hook hook 'ispell-message))
;; 	 '(
;; 	   message-send-hook
;; 	   mail-send-hook
;; 	   mh-before-send-letter-hook
;; 	   ))

;; RSS: newsticker

;; (setq newsticker-url-list-defaults
;;       '(("douban-tokyo-love-story" "http://www.douban.com/feed/group/11197/discussion")
;; 	("newsmth-blog" "http://www.newsmth.com/pc/rssrec.php")))

;; (global-set-key (kbd "C-c m n") 'newsticker-show-news)

;; (add-hook 'newsticker-mode-hook 'less-minor-mode-on)

;;;; dictionary
;; ------------

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)

;; search online
;; (setq dictionary-proxy-port 31280
;;       dictionary-proxy-server "sc.net9.org"
;;       dictionary-use-http-proxy t)

;; search at home
(setq dictionary-server "localhost"
      dictionary-tooltip-dictionary "wn"
      dictionary-default-dictionary "*")

(setq dictionary-coding-systems-for-dictionaries
      '(("cdict" . gb2312)
	("xdict" . gb2312)
	("stardict" . gb2312)))

(setq global-dictionary-tooltip-mode nil
      dictionary-tooltip-mode nil)

(defun xwl-dictionary-next-dictionary ()
  (interactive)
  (end-of-line)
  (search-forward-regexp "^From" nil t)
  (beginning-of-line))

(defun xwl-dictionary-prev-dictionary ()
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^From" nil t)
  (beginning-of-line))

(defun xwl-dictionary-mode-hook ()
  (define-key dictionary-mode-map (kbd "<backtab>") 'dictionary-prev-link)
  (define-key dictionary-mode-map (kbd "n") 'xwl-dictionary-next-dictionary)
  (define-key dictionary-mode-map (kbd "p") 'xwl-dictionary-prev-dictionary))

(add-hook 'dictionary-mode-hook 'xwl-dictionary-mode-hook)

;; wordnet
(require 'xwl-wordnet)

;;;; EMMS
;; ------

(require 'emms-setup)
(if xwl-emacs-special-p
    (emms-minimalistic)
  (emms-devel))

;; players
(setq emms-player-mpg321-command-name "mpg123"
      emms-player-mplayer-command-name "sudo"
      emms-player-mplayer-parameters
      '("nice" "-n" "-2" "mplayer" "-slave"
        ; "-font" "Simsun") ;"/usr/share/fonts/truetype/microsoft/simsun.ttf")
        )
      emms-player-list '(emms-player-mplayer
			 emms-player-mplayer-playlist
			 emms-player-ogg123
			 emms-player-mpg321))

;; coding
(setq emms-info-mp3info-coding-system 'gbk
      emms-lyrics-coding-system 'gbk
      emms-cache-file-coding-system 'utf-8)

;; files
(setq emms-source-file-default-directory "~/music/songs"
      emms-lyrics-dir "~/music/lyrics")

;; mode line format
(setq emms-mode-line-format "[ %s ]"
      emms-lyrics-display-format "%s"
      emms-playing-time-display-format "%s")

(setq xwl-memory-usage-string "")

(defun xwl-memory-usage-update-handler ()
  "Update memory usage report on mode line."
  (setq xwl-memory-usage-string
        (concat
         (car
         (split-string
          (shell-command-to-string
           "free -m | grep cache: | awk '{ print $3}'")))
         "M"))
  (force-mode-line-update))

(run-with-timer 0 60 'xwl-memory-usage-update-handler)

(setq global-mode-string
      '("" appt-mode-string
	display-time-string " "
        xwl-memory-usage-string " "
	;; xwl-week-at-school-string " "
	battery-mode-line-string " "
	erc-modified-channels-object
	emms-mode-line-string " "
	emms-playing-time-string " "
	emms-lyrics-mode-line-string " "))

;; faces
(unless xwl-emacs-special-p
  (set-face-foreground 'emms-playlist-selected-face "magenta")
  (set-face-foreground 'emms-playlist-track-face  "green"))

(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)

(add-hook 'emms-player-started-hook 'emms-show)

;; TODO: autoload feature
(setq emms-playlist-sort-prefix "S")

(define-key emms-playlist-mode-map (kbd "S s") 'emms-playlist-sort-by-score)

(global-set-key (kbd "<f3>") 'emms-playlist-mode-go-popup)

(defun xwl-emms-google-track ()
  (interactive)
  (let* ((file
	  (file-name-sans-extension
	   (file-name-nondirectory
	    (emms-track-get
	     (emms-playlist-current-selected-track)
	     'name))))
	 (url
	  (if (string-match "\\cc" file)
	      ;; baidu couldn't handle chinese correctly?

      ;; "http://mp3.baidu.com/m?f=ms&rn=10&tn=baidump3lyric&ct=150994944&word=hello
       ;; &submit=%B0%D9%B6%C8%CB%D1%CB%F7&lm=-1"


	      "http://mp3.baidu.com/"
	    (concat "http://search.lyrics.astraweb.com/?word="
             ;;"http://www.lyrics007.com/cgi-bin/s.cgi?q="
             (replace-regexp-in-string " " "+" file)
             ;; "&kw=on&submit=go"
             ))))
    (w3m-browse-url url)))

;; finish current, then stop.
(setq emms-no-next-p nil)

(defun emms-no-next ()
  "Finish current song, then stop."
  (interactive)
  (setq emms-no-next-p t)
  (message "Will finish current song, then stop."))

(defun xwl-emms-next-noerror ()
  "Wrap `emms-score-next-noerror' with `emms-no-next-p' check."
  (interactive)
  (cond (emms-no-next-p
	 (emms-stop)
	 (setq emms-no-next-p nil))
	(t
         (emms-score-next-noerror))))

(setq emms-player-next-function 'xwl-emms-next-noerror)

;; %a is artist.
;; %t is title.
;; %a is album.
;; %T is track number.
;; %y is year.
;; %n is free-form note.
;; %g is genre.
;; %p is playtime.

;; %s is score.

(setq my-emms-playlist-mode-line-format "%%-4s%a\t%t")

(defun my-emms-track-description-function (track)
  "Return a description of the current track."
  (let* ((name (emms-track-name track))
        (type (emms-track-type track))
        (score (emms-score-get-score name)))
  (condition-case nil
      (let* ((artist (emms-track-get track 'info-artist))
             (title (emms-track-get track 'info-title))
             (year (emms-track-get track 'info-year))
             (playing-time (emms-track-get track 'info-playing-time))
             (min (/ playing-time 60))
             (sec (% playing-time 60))
             (album (emms-track-get track 'info-album)))
;;         (format "%-4d%-6s%02d:%02d    %-20s《%s》 - %s"
;;                 score
;;                 (or year "")
;;                 (or min "")
;;                 (or sec "")
;;                 (or artist (error "Bad artist"))
;;                 (or album "")
;;                 (or title (error "Bad title")))

        (format "%-3d%s - %s"
                score
                (or artist (error "Bad artist"))
                (or title (error "Bad title")))
        )
    (error
     (let ((basic
            (if (eq 'file type)
                (file-name-sans-extension
                 (file-name-nondirectory name))
              (concat (symbol-name type) ":" name))))
       (format "%-3d%s" score basic))))))

(setq emms-track-description-function
      'my-emms-track-description-function)

;; Redefine this to use name only
(defun emms-mode-line-playlist-current ()
  "Format the currently playing song"
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (name (emms-track-name track))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond (title
                   title)
                  ((eq 'file type)
                   (file-name-sans-extension
                    (file-name-nondirectory name)))
                  (t
                   (concat (symbol-name type) ":" name))))))

(defun emms-playlist-mode-jump ()
  "Jump to the directory of track at point in `emms-playlist-buffer'."
  (interactive)
  (dired
   (file-name-directory
    (emms-track-get (emms-playlist-track-at) 'name))))

;; dired support
(defface emms-playlist-mark-face
  '((((class color) (background dark))
     :foreground "red")
    (((type tty) (class mono))
     :inverse-video t))
  "Face for the tracks marked in a playlist buffer."
  :group 'emms-playlist-mode)

(defface emms-playlist-flag-face
  '((((class color) (background dark))
     :foreground "magenta")
    (((type tty) (class mono))
     :inverse-video t))
  "Face for the flag before tracks in a playlist buffer."
  :group 'emms-playlist-mode)

(defun emms-playlist-mode-mark ()
  "Mark track."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (emms-with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (unless (looking-at "\\* ")
     (emms-playlist-mode-overlay-at-point 'emms-playlist-mark-face 1)
     (insert "* ")
     (emms-playlist-mode-overlay-track
      (point-at-bol) (1+ (point-at-bol)) 'emms-playlist-flag-face 1))
   (goto-char (point-at-bol))
   (forward-line 1)))

(defun emms-playlist-mode-unmark ()
  "Unmark track."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (widen)
  (emms-with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (let ((beg (point-at-bol))
         (end (point-at-eol)))
     (when (memq 'emms-playlist-mark-face
                 (emms-playlist-mode-faces-in beg end))
       (goto-char (point-at-bol))
       (delete-char 2)
       (emms-playlist-mode-remove-face
        'emms-playlist-mark-face beg end)
       (emms-playlist-mode-remove-face
        'emms-playlist-flag-face beg end)))
   (forward-line 1)))

(defun emms-playlist-mode-delete ()
  "Deleted marked tracks."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (widen)
  (when (y-or-n-p "Delete marked tracks ")
    (emms-with-inhibit-read-only-t
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
	 (if (memq 'emms-playlist-mark-face
		   (emms-playlist-mode-faces-in (point-at-bol)
						(point-at-eol)))
	     (progn
	       (emms-remove-all-overlays (point-at-bol) (point-at-eol))
	       (let ((track (car (emms-playlist-tracks-in-region
				  (point-at-bol)
				  (point-at-eol))))
		     (kill-whole-line t))
		 (goto-char (point-at-bol))
		 (kill-line)
		 (delete-file (emms-track-get track 'name))))
	   (forward-line 1)))))))

(defun emms-playlist-mode-delete-selected-track ()
  "Delete selected(playing) track."
  (interactive)
  (with-current-buffer emms-playlist-buffer
    (emms-playlist-ensure-playlist-buffer)
    (widen)
    (when (y-or-n-p "Delete selected(playing) track ")
      (emms-with-inhibit-read-only-t
       (save-excursion
         (emms-playlist-mode-center-current)
         (let ((track (emms-playlist-current-selected-track))
               (kill-whole-line t))
           (goto-char (point-at-bol))
           (kill-line)
           (delete-file (emms-track-get track 'name)))
         (emms-playlist-mode-play-smart))))))

;; ------------------- separate --------------------

(defun emms-playlist-mode-faces-in (beg end)
  "Return overlay faces between BEG and END in playlist buffer."
  (emms-playlist-ensure-playlist-buffer)
  (widen)
  (let ((overlays (overlays-in beg end)))
    (mapcar
     (lambda (overlay)
       (overlay-get overlay 'face))
     overlays)))

(defun emms-playlist-mode-remove-face (face beg end)
  "Remove overlay with FACE between BEG and END in playlist buffer."
  (emms-playlist-ensure-playlist-buffer)
  (let ((overlays (overlays-in beg end)))
    (catch 'exit
      (mapc
       (lambda (overlay)
	 (when (eq face (overlay-get overlay 'face))
	   (delete-overlay overlay)
	   (throw 'exit nil)))
       overlays))))

(defun emms-playlist-mode-remove-overlay-selected ()
  "Remove the overlay from the currently selected track"
  (when (not (null emms-playlist-mode-selected-overlay-marker))
    (save-excursion
      (goto-char emms-playlist-mode-selected-overlay-marker)
;;       (emms-remove-all-overlays (point-at-bol)
;; 				(point-at-eol))
;;       (emms-playlist-mode-overlay-at-point
;;        'emms-playlist-track-face 1))))
      (emms-playlist-mode-remove-face 'emms-playlist-selected-face
				      (point-at-bol)
				      (point-at-eol)))))

;; keys
(defun xwl-emms-playlist-mode-hook ()
  (toggle-truncate-lines 1)
  (define-key emms-playlist-mode-map (kbd "9") 'amixer-decrement-volume)
  (define-key emms-playlist-mode-map (kbd "0") 'amixer-increment-volume)
  (define-key emms-playlist-mode-map (kbd "x") 'emms-start)
  (define-key emms-playlist-mode-map (kbd "v") 'emms-stop)
  (define-key emms-playlist-mode-map (kbd "h") 'emms-shuffle)
  (define-key emms-playlist-mode-map (kbd "o") 'emms-show)
  (define-key emms-playlist-mode-map (kbd "F") 'emms-playlist-show-current-line)
  (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
  (define-key emms-playlist-mode-map (kbd "r") 'emms-toggle-repeat-track)
  (define-key emms-playlist-mode-map (kbd "R") 'emms-toggle-repeat-playlist)
  (define-key emms-playlist-mode-map (kbd "q") 'delete-window)
  (define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
  (define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
  (define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60)))
  (define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))

  (define-key emms-playlist-mode-map (kbd "C-x C-j") 'emms-playlist-mode-jump)

  (define-key emms-playlist-mode-map (kbd "N") 'emms-next)
  (define-key emms-playlist-mode-map (kbd "P") 'emms-previous)

  ;; emms dired
  (define-key emms-playlist-mode-map (kbd "n") 'next-line)
  (define-key emms-playlist-mode-map (kbd "p") 'previous-line)
  (define-key emms-playlist-mode-map (kbd "m") 'emms-playlist-mode-mark)
  (define-key emms-playlist-mode-map (kbd "u") 'emms-playlist-mode-unmark)
  (define-key emms-playlist-mode-map (kbd "D") 'emms-playlist-mode-delete)
  (define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-delete-selected-track)

  ;;   (define-key emms-playlist-mode-map (kbd "u") 'emms-score-up-playing)
  ;;   (define-key emms-playlist-mode-map (kbd "d") 'emms-score-down-playing)
  ;;   (define-key emms-playlist-mode-map (kbd "U") 'emms-score-up-file-on-line)
  ;;   (define-key emms-playlist-mode-map (kbd "D") 'emms-score-down-file-on-line)

  (define-key emms-playlist-mode-map (kbd "l") 'emms-playlist-mode-center-current)

  (local-unset-key (kbd "s"))
  (define-key emms-playlist-mode-map (kbd "s o") 'emms-score-show-playing)
  (define-key emms-playlist-mode-map (kbd "s u") 'emms-score-up-playing)
  (define-key emms-playlist-mode-map (kbd "s d") 'emms-score-down-playing)
  (define-key emms-playlist-mode-map (kbd "s O") 'emms-score-show-file-on-line)
  (define-key emms-playlist-mode-map (kbd "s U") 'emms-score-up-file-on-line)
  (define-key emms-playlist-mode-map (kbd "s D") 'emms-score-down-file-on-line))

;; TODO, weird?
(add-hook 'emms-playlist-mode-hooks 'xwl-emms-playlist-mode-hook)

;; (global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
(global-set-key (kbd "C-c e x") 'emms-start)
(global-set-key (kbd "C-c e v") 'emms-stop)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e o") 'emms-show)
(global-set-key (kbd "C-c e h") 'emms-shuffle)
;; (global-set-key (kbd "C-c e e") 'emms-play-file)
(global-set-key (kbd "C-c e SPC") 'emms-pause)
(global-set-key (kbd "C-c e f") 'emms-no-next)
(global-set-key (kbd "C-c e a") 'emms-add-directory-tree)

(global-set-key (kbd "C-c e d") 'emms-playlist-mode-delete-selected-track)

(global-set-key (kbd "C-c e r")   'emms-toggle-repeat-track)
(global-set-key (kbd "C-c e R")   'emms-toggle-repeat-playlist)
(global-set-key (kbd "C-c e m")   'emms-lyrics-toggle-display-on-minibuffer)
(global-set-key (kbd "C-c e M")   'emms-lyrics-toggle-display-on-modeline)
(global-set-key (kbd "C-c e g")   'xwl-emms-google-track)

(global-set-key (kbd "C-c e <left>")  (lambda () (interactive) (emms-seek -10)))
(global-set-key (kbd "C-c e <right>") (lambda () (interactive) (emms-seek +10)))
(global-set-key (kbd "C-c e <down>")  (lambda () (interactive) (emms-seek -60)))
(global-set-key (kbd "C-c e <up>")    (lambda () (interactive) (emms-seek +60)))

(global-set-key (kbd "C-c e s u") 'emms-score-up-playing)
(global-set-key (kbd "C-c e s d") 'emms-score-down-playing)
(global-set-key (kbd "C-c e s o") 'emms-score-show-playing)

;; amixer
(require 'amixer)
(setq amixer-mixer-program "aumix"
      amixer-volume-increment 1
      amixer-master-volume 10)

(if window-system
    (progn
;;       (global-set-key (kbd "C-M-9") 'amixer-decrement-volume)
;;       (global-set-key (kbd "C-M-0") 'amixer-increment-volume))
      )
  (global-set-key (kbd "C-M-_") 'amixer-decrement-volume)
  (global-set-key (kbd "C-M-+") 'amixer-increment-volume))

;; score
;; (require 'emms-score)
;; (emms-score 1)

(setq my-emms-last-track nil)

(defun my-emms-player-started-hook ()
  (setq my-emms-last-track (emms-playlist-current-selected-track)))

(add-hook 'emms-player-started-hook 'my-emms-player-started-hook)

(defun my-emms-player-finished-hook ()
  (emms-score-change-score 1 (emms-track-name my-emms-last-track)))

(add-hook 'emms-player-finished-hook 'my-emms-player-finished-hook)

(setq emms-info-asynchronously nil)

;;;; w3m

;; w3m

(require 'w3m)
(setq w3m-default-display-inline-images t
      w3m-default-save-directory "~/download/"
      w3m-home-page "file:///home/william/.emacs.d/muse/default/html/index.html"
      w3m-init-file "~/.emacs.d/.emacs-w3m"
      w3m-command-arguments
      (nconc w3m-command-arguments
	     ;; '("-o" "http_proxy=http://webcache.prc.sun.com:8080/"))
	     ;; '("-o" "http_proxy=http://222.43.34.94:3128/"))
	     '("-o" "http_proxy="))
      w3m-no-proxy-domains '(".edu.cn,166.111.,162.105.,net9.org"))

(setq w3m-process-modeline-format " loaded: %s")

(defun xwl-w3m-mode-hook ()
  (define-key w3m-mode-map (kbd "t") 'w3m-view-this-url-new-session)
  (define-key w3m-mode-map (kbd "b") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "f") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "B") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "F") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "o") 'w3m-goto-url)
  (define-key w3m-mode-map (kbd "O") 'w3m-goto-url-new-session))

(add-hook 'w3m-mode-hook 'xwl-w3m-mode-hook)

(setq w3m-content-type-alist
      '(("text/plain" "\\.\\(txt\\|tex\\|el\\)\\'" nil nil)
	("text/html" "\\.s?html?\\'" browse-url-default-browser nil)

	("image/jpeg" "\\.jpe?g\\'" ("/usr/bin/zgv" file) nil)
	("image/png" "\\.png\\'" ("/usr/bin/zgv" file) nil)
	("image/gif" "\\.gif\\'" ("/usr/bin/zgv" file) nil)
	("image/tiff" "\\.tif?f\\'" ("/usr/bin/zgv" file) nil)
	("image/x-xwd" "\\.xwd\\'" ("/usr/bin/zgv" file) nil)
	("image/x-xbm" "\\.xbm\\'" ("/usr/bin/zgv" file) nil)
	("image/x-xpm" "\\.xpm\\'" ("/usr/bin/zgv" file) nil)
	("image/x-bmp" "\\.bmp\\'" ("/usr/bin/zgv" file) nil)

	("video/mpeg" "\\.mpe?g\\'" nil nil)
	("video/quicktime" "\\.mov\\'" nil nil)

	("application/postscript" "\\.e?ps\\'" ("gv" file) nil)
	("application/pdf" "\\.pdf\\'" ("xpdf" file) nil)
	("application/xhtml+xml" nil nil "text/html")))

(setq mm-text-html-renderer 'w3m)


;;; HYPERMEDIA

;; folding, org

;;;; muse

(require 'muse)
(require 'muse-mode)
(require 'muse-colors)
(require 'muse-project)

(require 'muse-html)
(require 'muse-texinfo)
(require 'muse-latex)
;; (require 'muse-journal)
;; (require 'muse-latexcjk)

(setq muse-project-alist
      '(("default"
	 ("~/studio/muse/default" :default "index")
;;	 (:base "html" :path "/home/web")
	 (:base "html" :path "/williamxu@ftp.net9.org:/")
;; 	 (:base "texi" :path "~/info")
;; 	 (:base "info" :path "~/info"))
	 )
	("doc"
	 ("~/studio/muse/doc" :default "index")
	 (:base "html" :path "/home/web/doc")
;; 	 (:base "texi" :path "~/info")
;; 	 (:base "info" :path "~/info"))))
	 )
        ("planner"
         ("~/studio/muse/planner"
          :default "TaskPool"
          :major-mode planner-mode
          :visit-link planner-visit-link)
         (:base "planner-xhtml" :path "/home/web/planner"))))

(global-set-key (kbd "C-c m 9")
                (lambda () (interactive)
                  (find-file "/ftp:williamxu@ftp.net9.org:/")))

(setq muse-mode-auto-p t)

(setq muse-html-header "/home/william/studio/muse/style/header.html"
      muse-html-footer "/home/william/studio/muse/style/footer.html")

(defun xwl-muse-before-publish-hook ()
  (replace-regexp "" "")
  )

(add-hook 'muse-before-publish-hook 'xwl-muse-before-publish-hook)

(defun xwl-muse-resolve-path (base)
  "Return full path dir by looking at muse-current-project."
  (let ((path nil))
    (dolist (ls muse-current-project path)
      (if (and (listp ls) (member base ls))
	  (setq path (file-name-as-directory
		     (caddr (member base ls))))))))

(defun xwl-muse-preview (base &optional force)
  (interactive)
  (let* ((path (expand-file-name (xwl-muse-resolve-path base)))
	 (file (concat path (smart-compile-replace "%n.") base)))
;;     (when (or (file-newer-than-file-p buffer-file-name file)
;; 	      force)
      (muse-publish-this-file base path t);;)
    (cond ((equal base "html")
	   ;; (browse-url file)
	   (message "done"))
	  ((equal base "texi") 		; stupid here..<FIX ME>
	   (shell-command (concat "makeinfo " file))
	   (info (concat path (smart-compile-replace "%n.info")))
	   (shell-command (concat "makeinfo " file))
	   (Info-directory)
	   (Info-last)
	   (Info-last)))))

(defun xwl-muse-preview-html (&optional force)
  (interactive)
  (xwl-muse-preview "html" force))

(defun xwl-muse-preview-texi ()
  (interactive)
  (xwl-muse-preview "texi"))

(defun xwl-muse-mode-hook ()
  (local-unset-key (kbd "TAB"))
  (local-unset-key (kbd "*"))
  (define-key muse-mode-map (kbd "C-c DEL") 'xwl-muse-preview-html)
  (define-key muse-mode-map (kbd "C-c TAB") 'xwl-muse-preview-texi)

  (outline-minor-mode 1))

(add-hook 'muse-mode-hook 'xwl-muse-mode-hook)

(setq xwl-muse-tag-value
      '(("xwl-quotation"
	 "<div class=\"xwl-quotation\"><example>"
	 "</example></div>")
	("xwl-time"
	 "<div class=\"xwl-time\">"
	 "</div>")
	("xwl-code"
	 "<div class=\"xwl-code\"><example>"
	 "</example></div>")
	("xwl-note"
	 "<div class=\"xwl-note\"><example>"
	 "</example></div>")))

(defun xwl-muse-insert-tag (tag)
  "Support inserting user defined tags in a more flexiable style, by
looking at `xwl-muse-tag-value'. e.g.,

        (xwl-muse-insert-tag xwl-quotation)

Will result in,

<div class = \"xwl-quotation\"><pre>

</pre></div>"
  (interactive
   (list
    (ido-completing-read
     "Tag: "
     (mapcar 'car `(,@muse-publish-markup-tags
		    ,@xwl-muse-tag-value)))))
  (let ((tag-value (assoc tag xwl-muse-tag-value)))
    (if (not tag-value)
	(muse-insert-tag tag)
      (let ((begin (cadr tag-value))
	    (end (caddr tag-value)))
	(insert (concat begin "\n" "\n" end "\n"))
	(forward-line -2)))))

(define-key muse-mode-map (kbd "C-c <tab>") 'xwl-muse-insert-tag)

;; planner
;; -------

(require 'planner)
(setq planner-project "planner")
;; (planner-insinuate-calendar)
;; (planner-install-extra-task-keybindings)

(setq planner-add-task-at-end-flag t)
(setq planner-reverse-chronological-notes nil)

(global-set-key (kbd "C-c p t") 'planner-create-task)
(global-set-key (kbd "C-c p T") 'planner-create-task-from-buffer)
(global-set-key (kbd "C-c p n") 'planner-create-note)
(global-set-key (kbd "C-c p N") 'planner-create-note-from-context)

(global-set-key (kbd "<f7>") 'plan)

(require 'planner-deadline)

(require 'planner-trunk)
(setq planner-trunk-rule-list
      '(("\\`[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]\\'" nil
         ("重要" "每天" "长期" "读书" "杂项" "工作" "TaskPool" "购物"))))
(add-hook 'planner-mode-hook 'planner-trunk-tasks)

(defadvice plan (around writable-and-fill)
  "Make buffer writable and fill its frame."
  (let ((inhibit-read-only t))
    ad-do-it
    (planner-trunk-tasks)
    (delete-other-windows)))

(defadvice planner-create-task (around planner-writable)
  "Turn off buffer read-only."
  (let ((inhibit-read-only t))
    ad-do-it))

(defadvice planner-create-note (around planner-writable)
  "Turn off buffer read-only."
  (let ((inhibit-read-only t))
    ad-do-it))

(defadvice planner-task-done (around planner-writable)
  "Turn off buffer read-only."
  (let ((inhibit-read-only t))
    ad-do-it))

(ad-activate 'plan)
(ad-activate 'planner-create-task)
(ad-activate 'planner-create-note)
(ad-activate 'planner-task-done)

;; remind & diary
;; (require 'remind)
(setq diary-file "~/.diary")
;;       remind-diary-file "~/.diary_planner"
;;       remind-planner-file "~/.reminder_planner")
;; (add-hook 'diary-hook 'appt-make-list)
;; (diary)
;; (add-hook 'planner-mode-hook
;;    (lambda () (interactive)
;;      (make-local-hook 'after-save-hook)
;;      (add-hook 'after-save-hook 'remind-parse-planner t t)
;;      (add-hook 'after-save-hook 'remind-export-to-diary t t)))

;; (require 'planner-diary)
;; (add-hook 'diary-display-hook 'fancy-diary-display)

;; (setq planner-diary-use-diary t)
;; (planner-diary-insinuate)


;;; GNUS

(require 'gnus)

(setq gnus-init-file "~/src/backup/gnus/xwl-gnus.el"
      gnus-directory "~/src/backup/gnus")	; don't set it in `.gnus'

(setq gnus-startup-file           "~/src/backup/gnus/.newsrc"
      gnus-default-directory      "~/src/backup/gnus"
      gnus-home-directory         "~/src/backup/gnus"
      gnus-dribble-directory      "~/src/backup/gnus/dribble"
      message-directory           "~/src/backup/gnus/message"
      mail-source-directory       "~/src/backup/gnus/Mail"
      gnus-cache-directory        "~/src/backup/gnus/News/cache"
      gnus-article-save-directory "~/src/backup/gnus/News")

(setq gnus-interactive-exit nil)

;; scan new mail every 5 minutes, it seems not work.
;; (require 'gnus-demon)
;; (gnus-demon-add-handler 'gnus-demon-add-scanmail 5 nil)

;;;; Newsgroup Servers
;; -------------------

(setq gnus-check-new-newsgroups nil
      gnus-read-active-file 'some
      gnus-nov-is-evil nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-asynchronous t
      gnus-fetch-old-headers nil
      gnus-use-cross-reference nil)

(setq gnus-select-method '(nnfolder ""))
(setq gnus-secondary-select-methods
      '((nntp "128.230.129.221")
        (nntp "news.gmane.org")
        (nntp "news.mozilla.org")
        (nntp "news.cn99.com")
        (nntp "news.yaako.com")
        (nntp "webking.online.jn.sd.cn")
        ;; (nntp "news.newsfan.net") ; due to gb2312 issue
        ;; (nnslashdot "")
        ))

(defun xwl-gnus-add-newsgroups-maybe ()
  "If at midnight(or, between 23:00-07:00), at newsgroups, since
speed is usually fast at this time."
  (let ((hour (string-to-number
	       (format-time-string "%H" (current-time)))))
;;     (if (or (>= hour 23)
;; 	    (and (>= hour 0) (<= hour 7)))
	(setq gnus-secondary-select-methods
	      '(;; (nntp "128.230.129.221") ; fast reading only...
		;; (nntp "news.individual.net")
		;; (nntp "localhost")
		))
;;       (setq gnus-secondary-select-methods nil))
	))

;;;; Send/Fetch Mails/Messages
;; ---------------------------

(setq mail-archive-file-name "~/.emacs.d/outgoing")

(setq mail-user-agent 'gnus-user-agent)

;; (setq send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-default-smtp-server "localhost")

(defun xwl-sendmail-by-localhost ()
  "Send mail using localhost."
  (interactive)
  (setq message-send-mail-function 'message-send-mail-with-sendmail))

;; starttls.el, pop3.el, starttls, gnutls-bin
(defun xwl-sendmail-by-google ()
  "Enable sendmail by google."
  (interactive)
  (setq message-send-mail-function 'smtpmail-send-it)
  (require 'starttls)
  (setq smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	smtpmail-auth-credentials
	`(("smtp.gmail.com" 587 "william.xwl@gmail.com" ,pwgmail))
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
  (message "sendmail by google enabled."))

(defun xwl-sendmail-select ()
  "Select sendmail methods. You know, some ML doesn't allow
sendmail directly from localhost without a valid domain name."
  (save-excursion
    (let ((to (save-excursion
                (message-narrow-to-headers)
                (or (message-fetch-field "to")
                    ""))))
      (cond ((string-match "lists.sourceforge.net" to)
             (message "Will sendmail by google.")
             (xwl-sendmail-by-google))
            (t
             (xwl-sendmail-by-localhost))))))

(add-hook 'message-setup-hook 'xwl-sendmail-select)

(setq mail-sources '((file)))

;;;; Posting Styles
;; ----------------

(setq user-full-name "William Xu"
      user-mail-address "william.xwl@gmail.com"	; don't use `<mail>'!
      mail-host-address "williamxwl.com"
      mail-signature t)

;; make mails sent by myself display my name instead of "=>blahblah" in
;; the summary buffer
(setq gnus-ignored-from-addresses nil)

(defun xwl-fortune-signature (&optional cn-p)
  "Return fortune result as string.
Optional CN-P non-nil will use Chinese name."
  (let ((name "William"))
    (and cn-p (setq name "火柴"))
    (concat name "\n\n"
            (ansi-color-filter-apply
             (shell-command-to-string "fortune")))))

(defun xwl-fortune-signature-cn ()
  "Just a chinese version wrapper around `xwl-fortune-signature'.

FIMXE: does `gnus-posting-styles' accept functions with
arguments?"
  (xwl-fortune-signature t))

;; The entire alist will be iterated over!
(setq gnus-posting-styles
      `((".*"
	 (name user-full-name)
	 (address user-mail-address)
	 (organization "the Church of Emacs")
	 ;; (signature "William"))
         (signature xwl-fortune-signature))

        ;; 	(,(regexp-opt			; mailing lists
        ;; 	    (mapcar (lambda (list-group)
        ;; 		      (cadr list-group))
        ;; 		    xwl-mailing-list-group-alist))
        ;; 	  (signature "William"))

	("hotmail"
	 (address "william.xwl@hotmail.com"))

        (,(concat ".*"
                  (regexp-opt
                   '("webking.online.jn.sd.cn"
                     "news.newsfan.net"
                     "cn."))
                  ".*")
         (name "火柴")
         (signature xwl-fortune-signature-cn))

        ("cn.comp.os.linux"
         (name user-full-name)
         (signature (format
                     "William

\((\"email\" . \"william.xwl@gmail.com\")
 (\"www\"   . \"http://williamxu.net9.org\"))

%s"
                     (ansi-color-filter-apply
                      (shell-command-to-string "fortune")))))))

;;;; Mailing-lists & Mail Groups
;; -----------------------------

;; These groups will expire automatically.
(setq xwl-mailing-list-group-alist
      `(,@(mapcar
	   (lambda (list)
	     (let* ((list-str (symbol-name list))
		    (@-pos (string-match "@" list-str)))
	       `(,list-str ,(substring list-str 0 @-pos))))
	   '(
	     gtk-list@gnome.org
	     sawfish-list@gnome.org
	     conkeror@mozdev.org

	     gnu-emacs-sources@gnu.org
	     emms-help@gnu.org
	     emms-patches@gnu.org

	     zhcon-users@lists.sourceforge.net
	     ;; TODO, fix this
	     zhcon-devel@lists.sourceforge.net
	     zhcon-devel@lists.sf.net
	     zhcon-announce@lists.sourceforge.net

	     guile-user@gnu.org
	     guile-devel@gnu.org
	     bug-guile@gnu.org
	     scsh-users@scsh.net

	     ctrenzaibj@googlegroups.com
	     emacs-cn@googlegroups.com
             firefoxchina@googlegroups.com

             pdesc@ddtp.debian.net
	     ))

	;; local mails
	(".*Cron Daemon.*\\|.*root\\|Mailer-Daemon@williamxwl" "local")))

;; gnus parameters
(setq gnus-parameters
      `( ;; Seems it's only needed when fetching mail by Emacs herself.
	;; (".*" (gcc-self . t))		; always Gcc to oneself
	,@(mapcar*
	   (lambda (args)
	     (let ((list (car args))
		   (group (cadr args)))
	       `(,group (to-address  . ,list)
			(auto-expire . t)
			(to-list     . ,list)
			;; (gcc-self    . t)
			)))
	   xwl-mailing-list-group-alist)
	(,(regexp-opt
	   '("important"
	     "savings"
	     "nnfolder+archive:outgoing.important"
	     "nnfolder+archive:outgoing.news"
	     "nnfolder+archive:outgoing.work"
	     "general"
	     "hotmail"))
	 (gcc-self . t))))

(setq gnus-gcc-mark-as-read t)		; mark Gcc mail as read

;; visibility
(setq gnus-permanently-visible-groups
      (concat
       (regexp-opt
	'("savings"
	  "nnfolder+archive:outgoing.important"
	  ;; "sun"
	  ;; "nnfolder+archive:outgoing.work"
	  ;; "nnfolder+archive:outgoing.news"
	  "emms-help"
	  "emms-patches"
	  "newsmth"
	  ;; "hotmail"
	  ;; "rss"
          "important"
	  ))

       "\\|^general$"))

;;;; Split Incoming Mails
;; ----------------------

(defun xwl-notify-important ()
  "Notify me when important mails incoming."
  (xwl-start-process-shell-command
    "zenity --info --text \"You've Got Mail \!\" --title \"Gnus\"")
  "important")

(setq nnmail-split-fancy-match-partial-words t)

(setq nnmail-split-fancy
      `(| ,@(mapcar
	     (lambda (arg)
	       `(any ,(car arg) ,(cadr arg)))
	     xwl-mailing-list-group-alist)

	  ("subject"
           ,(concat ".*"
                    (regexp-opt
                     '("orkut"
                       "confirm"
                       "unsubscribed"))
                    ".*")
           "general")

	  (from
           ,(concat ".*"
                    (regexp-opt
                     '("douban.com"
                       "webmaster@linuxfans.org"
                       "mailman-owner@mozdev.org"
                       "xiaonei@exun.com"
                       "noreply@googlegroups.com"
                       "eweekly@ew.joyo.com"
                       "sender@maillist.csdn.net"
                       "newsletter@mysql.com"
                       "mailman-owner@python.org"
                       "Gmane Autoauthorizer"
                       "pandonny@linuxsir.org.cn"))
                    ".*")
           "general")


	  (to "william@localhost" "rss")

	  (to ,(regexp-opt
                '("william.xwl@gmail.com"
                  "xwl02@mails.tsinghua.edu.cn"
                  "xuweilin@mail.tsinghua.org.cn"))
              (: xwl-notify-important))

	  (from ".*@mails.thu.edu.cn" (: xwl-notify-important))

	  (to "william.xwl@hotmail.com" "hotmail")

	  (to ".*@newsmth.*" "newsmth")
	  (to ,(regexp-opt xwl-mailbox-lists) "general")

          (from ".*@localhost" "local")

	  "trash"))

(setq nnmail-split-methods 'nnmail-split-fancy)

;;;; Sent Mail & Sent News
;; -----------------------

(setq gnus-message-archive-method
      '(nnfolder "archive"
		 (nnfolder-directory   "~/src/backup/gnus/Mail/archive")
		 (nnfolder-active-file "~/src/backup/gnus/Mail/archive/active")
		 (nnfolder-get-new-mail nil)
		 (nnfolder-inhibit-expiry t)))

(setq gnus-message-archive-group   ; nnfolder+archive:outgoing.important
      `(("^important$" "outgoing.important")
;;         (,(regexp-opt
;; 	   (mapcar (lambda (list-group)
;; 		     (cadr list-group))
;; 		   xwl-mailing-list-group-alist))
;; 	 "outgoing.news")
;;         (,(concat ".*"
;;                   (regexp-opt
;;                    '("webking.online.jn.sd.cn"
;;                      "news.cn99.com"
;;                      "news.newsfan.net"))
;;                   ".*")
;;          "outgoing.news")
	(".*" "outgoing.news")))

;; set some default email and news headers
(setq message-default-mail-headers nil	;"Fcc: ~/.emacs.d/outgoing"
      message-default-news-headers nil)	;"Fcc: ~/.emacs.d/outgoing-news"

;;;; Expiration
;; ------------

(setq nnmail-expiry-wait-function
      (lambda (group)
	(cond
	 ((string= "trash" group) 7)
;; 	 ((string-match "sun.*" group) 60)
	 ((string-match
	   (regexp-opt
	    (mapcar (lambda (list-group)
		      (cadr list-group))
		    xwl-mailing-list-group-alist))
	   group)
	  30)
	 ((string-match "rss" group)
	  30)
	 (t 'never))))

;;;; Chinese Stuffs
;; ----------------

;; A workaround for unsupported charsets
(define-coding-system-alias 'gb18030 'gb2312)
(define-coding-system-alias 'x-gbk 'gb2312)
(define-coding-system-alias 'gbk 'gb2312)

;; This controls outgoing mails' charset.
(setq mm-coding-system-priorities '(utf-8 gb2312))

;; (sort-coding-systems '(utf-8 gbk chinese-iso-8bit gb2312))

;;    (setq gnus-default-charset 'cn-gb
;; 	gnus-newsgroup-ignored-charsets
;; 	'(unknown-8bit x-unknown iso-8859-1 ISO-8859-15 GB18030)))

(setq gnus-group-name-charset-group-alist
      '(("nnrss.*" . utf-8)             ; `G R'
        (".*" . gb2312)))

(setq gnus-summary-show-article-charset-alist
      '((1 . utf-8)
        (2 . cn-gb2312)
	(3 . big5)))

(setq gnus-group-name-charset-method-alist
      '(((nntp "news.newsfan.net") . gb2312)))

(add-to-list 'gnus-group-charset-alist '("nnrss.*" utf-8))

;;;; Group
;; -------

;;;; Summary
;; ---------

;; date
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%a %H:%M")
	(604800               . "%a %H:%M") ; this week
	((gnus-seconds-month) . "%d")
	((gnus-seconds-year)  . "%m/%d")
	(t                    . "%Y/%m/%d")))

;; Note!  Do `^, g' to update changes by `nnmail-extra-headers'! See
;; info for more.
(setq gnus-extra-headers '(Content-Type To Newsgroups))
(setq nnmail-extra-headers gnus-extra-headers)

(require 'rs-gnus-summary)

(defalias 'gnus-user-format-function-ct
  'rs-gnus-summary-line-content-type)

;;(setq gnus-summary-line-format "%U%R%z%-6d  %5k  %-20f%B%s\n")
(setq gnus-summary-line-format
      "%U%R%z%10&user-date; %u&ct; %5k  %-20f%B(%t) %s\n")

(defun xwl-gnus-summary-tree-plain ()
  "My old plain summary tree."
  (interactive)
  (setq gnus-sum-thread-tree-root            "" ; "* "
        gnus-sum-thread-tree-false-root      "" ; "* "
        gnus-sum-thread-tree-single-leaf     "\\"
        gnus-sum-thread-tree-single-indent   ""
        gnus-sum-thread-tree-indent          "  "
        gnus-sum-thread-tree-leaf-with-other "| "
        gnus-sum-thread-tree-vertical        ""))

(rs-gnus-summary-tree-arrows-wide)


;; vi
(defun xwl-vi-like-hook ()
    (local-set-key (kbd "k") 'previous-line)
    (local-set-key (kbd "j") 'next-line)
    (local-set-key (kbd "l") 'forward-char)
    (local-set-key (kbd "h") 'backward-char))

(defun xwl-gnus-summary-mode-hook ()
  (xwl-vi-like-hook)

  (local-set-key (kbd "p") 'gnus-summary-prev-same-subject)
  (local-set-key (kbd "n") 'gnus-summary-next-same-subject)
  (local-set-key (kbd "q") 'delete-other-windows)
  (local-set-key (kbd "Q") 'gnus-summary-exit)

  (local-set-key (kbd ",") 'gnus-summary-prev-thread)
  (local-set-key (kbd ".") 'gnus-summary-next-thread)

  (local-set-key (kbd "P") 'xwl-scroll-other-window-down-one-line)
  (local-set-key (kbd "N") 'xwl-scroll-other-window-up-one-line)
  (local-set-key (kbd "<") 'scroll-other-window-down)
  (local-set-key (kbd ">") 'scroll-other-window)
  (local-set-key (kbd "/ n") 'gnus-summary-insert-new-articles)

  (local-set-key (kbd "r") (lambda () (interactive)
			       (gnus-summary-show-article)
			       (other-window 1)))

  (local-set-key (kbd "RET") (lambda () (interactive)
			       (gnus-summary-show-article)
			       (other-window 1)))
  )

(add-hook 'gnus-summary-mode-hook 'xwl-gnus-summary-mode-hook)

(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

;;;; Article
;; ---------

(setq gnus-visible-headers
      (concat "^\\("
	      (regexp-opt
	       '("From" "To" "CC" "Subject" "Date"
		 "User-Agent" "X-Mailer" "X-Newsreader"
		 "NNTP-Posting-Host"
		 "Organization"
		 "Content-Type"
                 "Newsgroups"))
	      "\\):"))

;; (remove-hook 'gnus-article-prepare-hook 'gnus-article-fill-long-lines)
(add-hook 'gnus-article-prepare-hook 'less-minor-mode-on)

(defun xwl-gnus-article-mode-hook ()
  (local-set-key (kbd "q") 'delete-window)
  (xwl-gnus-article-show-ip)
)

(defun xwl-gnus-article-show-ip ()
  "Show author's ip info in newsgroups."
  (save-excursion
    (message-narrow-to-headers)
    (when (search-forward-regexp
           "NNTP-Posting-Host: \\([0-9.]+\\)" nil t) ; a-zA-Z
      (end-of-line)
      (insert-and-inherit " (")
      (insert-and-inherit
       (car
        (split-string
         (shell-command-to-string
          (concat "~/bin/ip.scm " (match-string-no-properties 1)))
         "\n")))
      (insert-and-inherit ")"))))

(add-hook 'gnus-article-prepare-hook 'xwl-gnus-article-mode-hook)

;;;; Scoring
;; ---------

(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; (add-hook 'message-send-hook 'ispell-message)

(defun xwl-summary-exit-hook ()
  (gnus-summary-bubble-group)
  (gnus-group-sort-groups-by-rank))

(add-hook 'gnus-summary-exit-hook 'xwl-summary-exit-hook)

;;;; Keys
;; ------

(defadvice gnus (around switch-or-start)
  "Start `gnus' or switch to started *Group* buffer."
  (if (gnus-alive-p)
      (switch-to-buffer "*Group*")
    ad-do-it)
  (xwl-gnus-add-newsgroups-maybe))

(ad-activate 'gnus)

(defun xwl-gnus-group-mode-hook ()
  (xwl-vi-like-hook)
  (local-set-key (kbd "Q") 'gnus-group-exit)
  (local-unset-key (kbd "q"))
  (gnus-topic-mode))

(add-hook 'gnus-group-mode-hook 'xwl-gnus-group-mode-hook)

;;;; MIME
;; ------

(setq mm-default-directory "~/download")

;; See `~/.mailcap' about actions based on MIME.

;;;; RSS
;; -----

(defun xwl-gnus-group-make-rss-group-noninteractively (url)
  "Given a URL, discover if there is an RSS feed.
If there is, use Gnus to create an nnrss group"
  (require 'nnrss)
  (if (not url)
      (setq url (read-from-minibuffer "URL to Search for RSS: ")))
  (let ((feedinfo (nnrss-discover-feed url)))
    (if feedinfo
	(let ((title (gnus-newsgroup-savable-name
                      (gnus-newsgroup-savable-name
                       (or (cdr (assoc 'title feedinfo)) ""))))
	      (desc (cdr (assoc 'description feedinfo)))
	      (href (cdr (assoc 'href feedinfo)))
	      (encodable (mm-coding-system-p 'utf-8)))
	  (when encodable
	    ;; Unify non-ASCII text.
	    (setq title (mm-decode-coding-string
			 (mm-encode-coding-string title 'utf-8) 'utf-8)))
	  (gnus-group-make-group (if encodable
				     (mm-encode-coding-string title 'utf-8)
				   title)
				 '(nnrss ""))
	  (push (list title href desc) nnrss-group-alist)
	  (nnrss-save-server-data nil))
      (error "No feeds found for %s" url))))

(setq xwl-gnus-rss-list
      '( ;; personal
        "http://www.newsmth.net/pc/rss.php?userid=xiaowei"
        "http://blog.sina.com.cn/myblog/index_rss.php?uid=1190363061"
        "http://blog.sina.com.cn/myblog/index_rss.php?uid=1198922365"
        "http://blog.sina.com.cn/myblog/index_rss.php?uid=1173538795"
        "http://www.newsmth.net/pc/rss.php?userid=xiaowei"
        "http://blog.sina.com.cn/myblog/index_rss.php?uid=1415686044"
        "http://shredderyin.spaces.msn.com/feed.rss"
        "http://blog.kanru.info/feed/"
        "http://kirbyzhou.spaces.msn.com/feed.rss"
        "http://blog.sina.com.cn/myblog/index_rss.php?uid=1215626582"
        "http://supermmx.org/blog/supermmx/feed"
        "http://blueapril.bokee.com/rss2.xml"
        ;; tech
        "http://rssnewsapps.ziffdavis.com/tech.xml"
        "http://www.LinuxDevices.com/backend/headlines10.rdf"
        "http://www.linuxjournal.com/node/feed"
        "http://rss.slashdot.org/Slashdot/slashdot"
        "http://www.debian.org/security/dsa"
        "http://googlechinablog.com/atom.xml"
        "http://blog.csdn.net/dancefire/rss.aspx"
        "http://feeds.feedburner.com/solidot"
        "http://2blogs.net/blog/feed.asp"
        "http://www.infolets.com/infolets.rss"
        "http://news.com.com/2547-1_3-0-20.xml"))

(defun xwl-gnus-group-update-rss-group ()
  "Add rss groups in `xwl-gnus-rss-list'."
  (interactive)
  (with-current-buffer "*Group*"
    (mapc
     (lambda (url)
       (condition-case nil
           (xwl-gnus-group-make-rss-group-noninteractively url)
         (error nil)))
     xwl-gnus-rss-list)
    (message "done")))


;;;; Misc
;; ------

;; message-mode
(define-key message-mode-map (kbd "ESC TAB") 'bbdb-complete-name)
(define-key message-mode-map (kbd "<backtab>") 'bbdb-complete-name)

;; fetch all/important mails at the right time.
(setq xwl-fetchmail-all-p nil)

(defun xwl-fetchmail-all ()
  "Fetch all mails."
  (interactive)
  (setq xwl-fetchmail-all-p t)
  (copy-file "~/.fetchmailrc.all" "~/.fetchmailrc" t)
  (message "Will fetchmail all mails."))

(defun xwl-fetchmail-important ()
  "Fetch important mails only."
  (interactive)
  (setq xwl-fetchmail-all-p nil)
  (copy-file "~/.fetchmailrc.important" "~/.fetchmailrc" t)
  (message "Will fetchmail important mails only."))

(defun xwl-fetchmail-toggle ()
  "Toggle fetching all mails or only important mails."
  (interactive)
  (setq xwl-fetchmail-all-p (not xwl-fetchmail-all-p))
  (if xwl-fetchmail-all-p
      (xwl-fetchmail-all)
    (xwl-fetchmail-important)))

(global-set-key (kbd "C-c n f") 'xwl-fetchmail-toggle)

;; (xwl-fetchmail-important)

(defun xwl-fetchmail-all-at-night ()
  "Fetch all mails after 21:00."
  (let ((hour (string-to-number
	       (format-time-string "%H" (current-time)))))
    (when (or (>= hour 21)
	      (and (>= hour 0) (<= hour 7)))
      (xwl-fetchmail-all))))

;; (remove-hook 'xwl-run-when-idle-hook 'xwl-fetchmail-all-at-night)


;;; POST

(set-default-font "10x20")

;; (defalias 'w3m-safe-view-this-url 'browse-url-at-point)
;; (defalias 'w3m-view-this-url 'browse-url-at-point)

(open-dribble-file "~/.emacs-key-log")

;; (toggle-debug-on-error)

;; (require 'gds)
;; (global-set-key (kbd "<f1> g") 'gds-help-symbol)

;; See `.xwl-emacs-main.el' and `.xwl-emacs-gnus.el' for next loadup
;; step.

;;; .xwl-emacs.el ends here
