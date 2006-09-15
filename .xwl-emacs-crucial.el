;;; .xwl-emacs-crucial.el --- Crucial non-standard extensions

;; Copyright (C) 2006 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Last updated: 2006/09/15 23:48:23

;; wajig.el
;; color-theme.el
;; highlight-tail.el
;; smart-compile+.el
;; smart-operator.el
;; browse-kill-ring.el
;; rect-mark.el
;; nuke-trailing-whitespace.el
;; cal-china-x.el
;; page-break.el
;; less.el
;; redo.el
;; goto-last-change.el
;; dired-isearch.el
;; cscope.el
;; bracketphobia.el
;; bbdb

;; path
(setq xwl-at-home-p t)

(unless xwl-outside-home-p
  (setq xwl-crucial-path
        (error "Set crucial path first, then comment me :P"))
  (setq load-path `(,@load-path ,xwl-crucial-path))
  (cd xwl-crucial-path)
  (normal-top-level-add-subdirs-to-load-path))

(require 'smart-operator)
(defun xwl-text-mode-hook ()
  (auto-compression-mode t)
  (abbrev-mode t)
  ;; (flyspell-mode t)

  (smart-insert-operator-hook)
  (local-unset-key (kbd "."))
  (local-set-key (kbd "M-S") 'wordnet-search)
  (local-set-key (kbd "M-s") 'dictionary-search))
;; (local-set-key (kbd "TAB") 'ispell-complete-word))

(add-hook 'text-mode-hook 'xwl-text-mode-hook)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(require 'rect-mark)

(require 'nuke-trailing-whitespace)
(defun nuke-trailing-whitespace-check-mode ()
  "Redefinition. Generally don't bother me!"
  (not (memq major-mode
	 nuke-trailing-whitespace-never-major-modes)))

(defun xwl-write-file-hook ()
  ;; write-file-hooks must return nil if success.
  (xwl-update-date)
  (nuke-trailing-whitespace))

(add-hook 'write-file-hooks 'xwl-write-file-hook)

(defun xwl-after-save-hook ()
  (xwl-chmod-file-executable))

(add-hook 'after-save-hook 'xwl-after-save-hook)

(defun xwl-kill-emacs-hook ()
  (when (fboundp 'gnus-group-exit)
    (gnus-group-exit)))

(add-hook 'kill-emacs-hook 'xwl-kill-emacs-hook)

(defadvice save-buffers-kill-emacs (before remove-less)
  "Remove `less-minor-mode-on' from `find-file-hook' while exiting."
  (remove-hook 'find-file-hook 'less-minor-mode-on))

(ad-activate 'save-buffers-kill-emacs)

(require 'calendar)
(require 'cal-china-x)

(setq calendar-latitude +39.55
      calendar-longtitude +116.25
      calendar-location-name "Beijing")
;; changting(+25.52, +116.20)

(global-set-key (kbd "<f12>") (lambda () (interactive)
                                (calendar)
                                (with-current-buffer "*Calendar*"
                                  (calendar-cursor-holidays))))

(setq mark-diary-entries-in-calendar t
      appt-issue-message t
      mark-holidays-in-calendar t
      view-calendar-holidays-initially t)

(setq other-holidays
      '((holiday-chinese 5 11 "我的生日！")
        (holiday-chinese 10 10 "海豚公主的生日！& 民国双十节")

        (holiday-fixed 6 3 "jojo 的生日！")
        (holiday-fixed 7 23 "xs 的生日！")
        (holiday-fixed 12 19 "rolian 的生日！")))

(setq calendar-holidays
      (append calendar-holidays other-holidays))

(define-key calendar-mode-map (kbd "j") 'calendar-forward-week)
(define-key calendar-mode-map (kbd "k") 'calendar-backward-week)
(define-key calendar-mode-map (kbd "l") 'calendar-forward-day)
(define-key calendar-mode-map (kbd "h") 'calendar-backward-day)

(require 'page-break)
(turn-on-page-break-mode)

(require 'less)
(global-set-key (kbd "C-c v") 'less-minor-mode)
(add-hook 'find-file-hook 'auto-less-minor-mode)
(mapc
 (lambda (hook)
   (add-hook hook 'less-minor-mode-on))
 '(help-mode-hook
   wajig-mode-hook
   wordnet-search-mode-hook
   dictionary-mode-hook
   custom-mode-hook
   ;; apropos-mode-hook ; ??
   emms-playlist-mode-hooks
   woman-post-format-hook
   Man-mode-hook
   bbdb-mode-hook))

(defun xwl-toggle-find-file-less ()
  "Toggling opening files with `less-minor-mode' automatically ."
  (interactive)
  (if (memq 'less-minor-mode-on find-file-hook)
      (progn
	(remove-hook 'find-file-hook 'less-minor-mode-on)
	(remove-hook 'after-revert-hook 'less-minor-mode-on)
	(message "Less minor mode after opening file disabled"))
    (add-hook 'find-file-hook 'less-minor-mode-on)
    (add-hook 'after-revert-hook 'less-minor-mode-on)
    (message "Less minor mode after opening file enabled")))

(global-set-key (kbd "C-c n v") 'xwl-toggle-find-file-less)

(add-hook 'find-file-hook 'less-minor-mode-on)
(add-hook 'after-revert-hook 'less-minor-mode-on)

;; (defun xwl-before-revert-hook ()
;;   (if less-minor-mode
;;       (add-hook 'after-revert-hook 'less-minor-mode-on)
;;     (remove-hook 'after-revert-hook 'less-minor-mode-on)))

;; (add-hook 'before-revert-hook 'xwl-before-revert-hook)

(add-hook 'Info-mode-hook 'less-minor-mode-on)

(require 'redo)

(require 'goto-last-change)

;; bbdb
(require 'bbdb)
(bbdb-initialize)
(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-user-mail-names
      (regexp-opt '("willam.xwl@gmail.com"
                    "willam.xwl@hotmail.com")))
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail)
(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
(setq bbdb-complete-name-allow-cycling t)
(setq bbdb-use-pop-up nil)

(define-key bbdb-mode-map (kbd "TAB") 'bbdb-toggle-records-display-layout)

(global-set-key (kbd "<f8>")
		(lambda ()
		  (interactive)
		  (if (get-buffer "*BBDB*")
		      (switch-to-buffer "*BBDB*")
		    (bbdb "" nil)
		    (other-window 1)
		    (delete-other-windows))))

(defun my-bbdb-search (field str)
  "Search records whose FIELD matches STR."
  (interactive
   (list
    (ido-completing-read
     "Search: "
     (append (mapcar (lambda (el) (car el)) (bbdb-propnames))
             '("name")))
    (read-string "Match: ")))
  (if (string= field "name")
      (bbdb-name str nil)
    (let ((matches '())
          (case-fold-search bbdb-case-fold-search)
          (invert (bbdb-search-invert-p)))
      (when (stringp field)
        (setq field (intern field)))
      (mapc
       (lambda (record)
         (condition-case nil
             (let ((matchedp
                    (string-match
                     str
                     (cdr (assoc field (bbdb-record-raw-notes record))))))
               (when (or (and (not invert) matchedp)
                         (and invert (not matchedp)))
                 (setq matches (append matches (list record)))))
           (error nil)))
       (bbdb-records))
      (bbdb-display-records matches))))

(defun my-bbdb-create (name)
  "Add a new entry to the bbdb database.

This is different from `bbdb-create', where `my-bbdb-create' only
prompts for name field."
  (interactive "sName: ")
  (let ((record
         (vector name "" nil nil nil nil nil nil
                 (make-vector bbdb-cache-length nil))))
    (bbdb-invoke-hook 'bbdb-create-hook record)
    (bbdb-change-record record t)
    (bbdb-display-records (list record))))

(defun my-bbdb-display-all ()
  (interactive)
  (bbdb-display-records (bbdb-records)))

(define-key bbdb-mode-map (kbd "a") 'my-bbdb-display-all)
(define-key bbdb-mode-map (kbd "\/") 'my-bbdb-search)

(require 'dired-isearch)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward-regexp)
(define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward)

(require 'smart-compile+)

(defun xwl-texinfo-mode-run ()
  (interactive)
  (info (smart-compile-replace "%n.info")))

;;   %F  absolute pathname            ( /usr/local/bin/netscape.bin )
;;   %f  file name without directory  ( netscape.bin )
;;   %n  file name without extention  ( netscape )
;;   %e  extention of file name       ( bin )

(setq smart-compile-alist
      '(("\\.c$"          . "gcc -O2 %f -lm -o %n")
	("\\.[Cc]+[Pp]*$" . "g++ -O2 %f -lm -o %n")
	("\\.java$"       . "javac %f")
	("\\.f90$"        . "f90 %f -o %n")
	("\\.[Ff]$"       . "f77 %f -o %n")
	("\\.pl$"         . "perl -cw %f")
	("\\.mp$"	  . "mptopdf %f")
	("\\.php$"        . "php %f")
	;; ("\\.tex$"        . "latex %f")	; not working??
	("\\.texi$"       . (makeinfo-buffer))
	("\\.l$"          . "lex -o %n.yy.c %f && gcc -O2 %n.yy.c -lm -o %n")
	("\\.y$"          . "yacc -o %n.tab.c %f && gcc -O2 %n.tab.c -lm -o %n")
	("\\.TOOL$"       . "tool %f")
	("\\.tool$"       . "tool %f")
	(emacs-lisp-mode  . (emacs-lisp-byte-compile))))

(setq smart-run-alist
      '(("\\.c$"          . "./%n")
	("\\.[Cc]+[Pp]*$" . "./%n")
	("\\.java$"       . "java %n")
	("\\.php$"	  . "php %f")
	("\\.m$"	  . "./%f")
	("\\.scm"         . "./%f")
	;; ("\\.tex$"        . "xdvi %n.dvi")
	("\\.pl$"         . "./%f")
	(texinfo-mode     . (xwl-texinfo-mode-run))))

(setq smart-executable-alist
      '("%n.class"
	"%n"
	"%n.m"
	"%n.php"
	"%n.scm"
	"%n.dvi"
	"%n.pl"
	"%n.info"))

(global-set-key (kbd "C-<up>")    'smart-compile)
(global-set-key (kbd "C-<down>")  'smart-run)

;; asm
(require 'asm-mode)
(define-key asm-mode-map (kbd ":") '(lambda () (interactive) (smart-insert-operator ":" t)))
(define-key asm-mode-map (kbd ",") '(lambda () (interactive) (smart-insert-operator "," t)))
(define-key asm-mode-map (kbd "RET") 'newline)

;; makefile
(defun xwl-makefile-mode-hook ()
  (smart-insert-operator-hook)
  (local-unset-key (kbd ".")))

(add-hook 'makefile-mode-hook 'xwl-makefile-mode-hook)

;; php
(defun xwl-php-mode-hook ()
  (interactive)
  (local-unset-key (kbd ">"))
  (local-unset-key (kbd "<")))

(add-hook 'php-mode-user-hook 'xwl-php-mode-hook)

;; perl
(require 'perl-mode)

(defun xwl-perl-mode-hook ()
  (define-key perl-mode-map (kbd "=") (lambda() (interactive) (smart-insert-operator "=")))
  (define-key perl-mode-map (kbd "+") (lambda() (interactive)  (smart-insert-operator "+")))
  (define-key perl-mode-map (kbd "-") (lambda() (interactive)  (smart-insert-operator "-")))
  (define-key perl-mode-map (kbd "/") (lambda() (interactive)  (smart-insert-operator "/")))

  ;;   (define-key perl-mode-map (kbd "%") (lambda() (interactive) (smart-insert-operator "%")))
  ;;   (define-key perl-mode-map (kbd "&") (lambda() (interactive) (smart-insert-operator "&")))
  ;;   (define-key perl-mode-map (kbd "*") (lambda() (interactive) (smart-insert-operator "*")))
  ;;   (define-key perl-mode-map (kbd "!") (lambda() (interactive) (smart-insert-operator "!")))
  (define-key perl-mode-map (kbd "|") (lambda() (interactive) (smart-insert-operator "|")))
  (define-key perl-mode-map (kbd "<") (lambda (&optional arg) (interactive "P")
					(if arg (progn (insert "<>") (backward-char))
					  (smart-insert-operator "<"))))
  (define-key perl-mode-map (kbd ">") (lambda() (interactive) (smart-insert-operator ">")))
  (define-key perl-mode-map (kbd ",") (lambda() (interactive) (insert ", ")))
  (define-key perl-mode-map (kbd ";") (lambda() (interactive) (insert ";
"))))

(xwl-perl-mode-hook)

;; c
(require 'cc-mode)
;; M-x semanticdb-save-all-db
;; M-x semanticdb-create-system-database
;; M-x bovinate
;; M-x hide-ifdef-mode

;; C-c C-e (expand macros)
;; C-c C-\ (indent beautifully)

(defun xwl-c-mode-common-hook ()
  ;; common
  ;;   (setq c-macro-shrink-window-flag t
  ;; 	c-macro-preprocessor "cpp"
  ;; 	c-macro-cppflags " "
  ;; 	c-macro-prompt-flag t)

  (c-set-style "k&r")

  (c-toggle-auto-hungry-state t)
  (c-toggle-hungry-state t)

  (setq tab-width 4
	indent-tabs-mode nil)

  (setq c-cleanup-list
	'(scope-operator
	  empty-defun-braces
	  defun-close-semi))

  ;; modes, hooks,
  (smart-insert-operator-hook)

  ;; conficts with outline-minor-mode?
;;   (hs-minor-mode 1)
;;   (hs-hide-all)
;;   (xwl-hs-minor-mode-hook)
;;   (hide-ifdef-mode 1)
  (setq hide-ifdef-initially t)

  (abbrev-mode 1)
  ;; (glasses-mode -1)
  (cwarn-mode 1)
;;  (which-func-mode 1)
  (doxymacs-mode 1)
  (doxymacs-font-lock)
  (set (make-local-variable 'outline-regexp) "///+ ")
  (outline-minor-mode 1)

  ;; keys
  (local-unset-key (kbd "("))
  (local-unset-key (kbd "."))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd "%"))

  (local-set-key (kbd "C-c m a") 'align)
  (local-set-key (kbd "*") 'c-electric-star)
  (local-set-key (kbd ";") 'c-electric-semi&comma)
  (local-set-key (kbd "ESC TAB") 'semantic-ia-complete-symbol)
  (local-set-key (kbd "<C-home>") 'gdb)
  ;; 'semantic-chart-nonterminal-complexity-token)
  (local-set-key (kbd "<C-prior>") 'expand-member-functions))

(add-hook 'c-mode-common-hook 'xwl-c-mode-common-hook)

;; python

(defun xwl-python-mode-hook ()
  (smart-insert-operator-hook)
  (local-unset-key (kbd ".")))

(add-hook 'python-mode-hook 'xwl-python-mode-hook)

;;TODO
;;(load-file "/usr/share/emacs/site-lisp/xcscope.el")
;; The following line corresponds to be beginning of the "Cscope" menu.
(global-set-key "\C-css" 'cscope-find-this-symbol)
(global-set-key "\C-csd" 'cscope-find-global-definition)
(global-set-key "\C-csg" 'cscope-find-global-definition)
(global-set-key "\C-csG" 'cscope-find-global-definition-no-prompting)
(global-set-key "\C-csc" 'cscope-find-functions-calling-this-function)
(global-set-key "\C-csC" 'cscope-find-called-functions)
(global-set-key "\C-cst" 'cscope-find-this-text-string)
(global-set-key "\C-cse" 'cscope-find-egrep-pattern)
(global-set-key "\C-csf" 'cscope-find-this-file)
(global-set-key "\C-csi" 'cscope-find-files-including-file)
;; --- (The '---' indicates that this line corresponds to a menu separator.)
(global-set-key "\C-csb" 'cscope-display-buffer)
(global-set-key "\C-csB" 'cscope-display-buffer-toggle)
(global-set-key "\C-csn" 'cscope-next-symbol)
(global-set-key "\C-csN" 'cscope-next-file)
(global-set-key "\C-csp" 'cscope-prev-symbol)
(global-set-key "\C-csP" 'cscope-prev-file)
(global-set-key "\C-csu" 'cscope-pop-mark)
;; ---
(global-set-key "\C-csa" 'cscope-set-initial-directory)
(global-set-key "\C-csA" 'cscope-unset-initial-directory)
;; ---
(global-set-key "\C-csL" 'cscope-create-list-of-files-to-index)
(global-set-key "\C-csI" 'cscope-index-files)
(global-set-key "\C-csE" 'cscope-edit-list-of-files-to-index)
(global-set-key "\C-csW" 'cscope-tell-user-about-directory)
(global-set-key "\C-csS" 'cscope-tell-user-about-directory)
(global-set-key "\C-csT" 'cscope-tell-user-about-directory)
(global-set-key "\C-csD" 'cscope-dired-directory)

(require 'bracketphobia)

;; debian pkg manager
(autoload 'wajig "wajig"
  "Create a *wajig* buffer." t)
(global-set-key (kbd "<f10>") 'wajig)

(setq wajig-frequent-commands
      '("ps -ef" "ps -u william u"))

(add-hook 'w3m-mode-hook 'less-minor-mode-on)

;;;; color-theme

;; color-theme: `list-colors-display', `color-theme-print'.
(require 'color-theme)

(defvar theme-circle
  (list
   'color-theme-blue-mood
   'color-theme-ld-dark
   'color-theme-robin-hood
   'color-theme-billw
   'color-theme-shaman
   'color-theme-classic
   'color-theme-dark-green
   'color-theme-kingsajz
   'color-theme-hober
   'color-theme-jonadabian-slate
   'color-theme-jsc-dark
   'color-theme-kingsajz
   'color-theme-resolve
   'color-theme-sitaramv-nt
   'color-theme-sitaramv-solaris
   'color-theme-snow
   'color-theme-snowish
   'color-theme-subtle-blue
   'color-theme-wheat))
(defvar theme-current theme-circle)

(defun xwl-theme-set-default ()
  (interactive)
  (setq theme-current theme-circle)
  (funcall (car theme-circle)))

(defun xwl-describe-theme ()
  (interactive)
  (message "%s" (car theme-current)))

(defun xwl-theme-circle ()
  (interactive)
  (if (null (cdr theme-current))
      (setq theme-current theme-circle)
    (setq theme-current (cdr theme-current)))
  (funcall (car theme-current))
  (message "%S" (car theme-current)))

(global-set-key (kbd "M-3") 'xwl-theme-circle)

;; (when window-system
;; ;  (color-theme-classic)
;;   (color-theme-kingsajz))
;; (color-theme-hober)

;; `color-theme-print'
(defun color-theme-xwl ()
  "Color theme by William XWL, created 2005-05-13."
  (interactive)
  (color-theme-install
   '(color-theme-xwl

     ((foreground-color  . "#c0c0c0")
      (background-color  . "black")
      (mouse-color       . "black")
      (cursor-color      . "medium turquoise")
      (border-color      . "black")
      (background-mode   .  dark))

     ((Man-overstrike-face . bold)
      (Man-underline-face . underline)
      (browse-kill-ring-separator-face . bold)
      (display-time-mail-face . mode-line)
      (erc-button-face . bold)
      (erc-button-mouse-face . highlight)
      (gnus-article-button-face . bold)
      (gnus-article-mouse-face . highlight)
      (gnus-cite-attribution-face . gnus-cite-attribution-face)
      (gnus-mouse-face . highlight)
      (gnus-server-agent-face . gnus-server-agent-face)
      (gnus-server-closed-face . gnus-server-closed-face)
      (gnus-server-denied-face . gnus-server-denied-face)
      (gnus-server-offline-face . gnus-server-offline-face)
      (gnus-server-opened-face . gnus-server-opened-face)
      (gnus-signature-face . gnus-signature-face)
      (gnus-summary-selected-face . gnus-summary-selected-face)
      (gnus-treat-display-face . head)
      (help-highlight-face . underline)
      (hl-line-face . highlight)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face)
      (list-matching-lines-face . bold)
      (setnu-line-number-face . setnu-line-number-face)
      (view-highlight-face . highlight)
      (w3m-form-mouse-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "darkslategrey" :foreground
                            "wheat" :inverse-video nil :box nil
                            :strike-through nil :overline nil :underline
                            nil :slant normal :weight normal :width
                            normal :family "outline-andale mono"))))
     (Info-title-1-face ((t (:bold t :foreground "yellow" :weight bold))))
     (Info-title-2-face ((t (:bold t :foreground "lightblue" :weight bold))))
     (Info-title-3-face ((t (:bold t :weight bold))))
     (Info-title-4-face ((t (:bold t :weight bold))))

     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (nil))))
     (calendar-today-face ((t (:underline t))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (cursor ((t (nil))))
     (diary-face ((t (:foreground "yellow"))))
     (emacs-wiki-bad-link-face ((t (:bold t :foreground "coral" :underline "coral" :weight bold))))
     (emacs-wiki-link-face ((t (:bold t :foreground "cyan" :underline "cyan" :weight bold))))
     (emacs-wiki-verbatim-face ((t (:foreground "gray"))))
     (emms-pbi-current-face ((t (:bold t :foreground "magenta" :weight bold))))
     (emms-pbi-song-face ((t (:foreground "green"))))

     (fixed-pitch ((t (:family "courier"))))
     (font-lock-builtin-face ((t (:foreground "blue" :weight light))))
     (font-lock-comment-face ((t (:foreground "red1"))))
     (font-lock-constant-face ((t (:foreground "magenta"))))
     (font-lock-doc-face ((t (:foreground "green"))))
     (font-lock-function-name-face ((t (:bold t :foreground "royal blue" :weight bold)))) ;
     (font-lock-keyword-face ((t (:bold t :foreground "cyan" :weight bold))))
     (font-lock-string-face ((t (:foreground "green"))))
     (font-lock-type-face ((t (:foreground "green"))))
     (font-lock-variable-name-face ((t (:foreground "yellow" :weight light))))
     (font-lock-warning-face ((t (:foreground "red"))))
     (fringe ((t (:background "grey10"))))
     (fvwm-button-conf-face ((t (:bold t :foreground "orange3" :weight bold :family "lucida"))))
     (fvwm-conf-face ((t (:bold t :foreground "brown" :weight bold :family "lucida"))))
     (fvwm-func-menu-face ((t (:bold t :foreground "palevioletred" :weight bold :family "lucida"))))
     (fvwm-module-face ((t (:bold t :foreground "black" :weight bold :family "lucida"))))
     (fvwm-mycomments-face ((t (:bold t :foreground "purple" :weight bold :family "lucida"))))
     (fvwm-piperead-face ((t (:bold t :foreground "#900090" :weight bold :family "lucida"))))
     (fvwm-special-face ((t (:bold t :foreground "#ff5555" :weight bold :family "lucida"))))

     (gnus-cite-attribution-face ((t (:family "arial"))))
     (gnus-cite-face-1 ((t (:foreground "DarkGoldenrod3"))))
     (gnus-cite-face-10 ((t (nil))))
     (gnus-cite-face-11 ((t (nil))))
     (gnus-cite-face-2 ((t (:foreground "IndianRed3"))))
     (gnus-cite-face-3 ((t (:foreground "tomato"))))
     (gnus-cite-face-4 ((t (:foreground "yellow green"))))
     (gnus-cite-face-5 ((t (:foreground "SteelBlue3"))))
     (gnus-cite-face-6 ((t (:foreground "Azure3"))))
     (gnus-cite-face-7 ((t (:foreground "Azure4"))))
     (gnus-cite-face-8 ((t (:foreground "SpringGreen4"))))
     (gnus-cite-face-9 ((t (:foreground "SlateGray4"))))
     (gnus-emphasis-bold ((t (:bold t :foreground "greenyellow" :weight bold :family "Arial"))))
     (gnus-emphasis-bold-italic
      ((t (:italic t :bold t :foreground "OrangeRed1" :slant italic :weight bold :family "arial"))))
     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "khaki"))))
     (gnus-emphasis-italic
      ((t (:italic t :bold t :foreground "orange" :slant italic :weight bold :family "Arial"))))
     (gnus-emphasis-underline ((t (:foreground "greenyellow" :underline t))))
     (gnus-emphasis-underline-bold
      ((t (:bold t :foreground "khaki" :underline t :weight bold :family "Arial"))))
     (gnus-emphasis-underline-bold-italic
      ((t (:italic t :bold t :underline t :slant italic :weight bold :family "Arial"))))
     (gnus-emphasis-underline-italic
      ((t (:italic t :foreground "orange" :underline t :slant italic :family "Arial"))))
     (gnus-group-mail-1-empty-face ((t (:foreground "Salmon4"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "firebrick1" :weight bold))))
     (gnus-group-mail-2-empty-face ((t (:foreground "turquoise4"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise" :weight bold))))
     (gnus-group-mail-3-empty-face ((t (:foreground "LightCyan4"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "LightCyan1" :weight bold))))
     (gnus-group-mail-low-empty-face ((t (:foreground "SteelBlue4"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "SteelBlue2" :weight bold))))
     (gnus-group-news-1-empty-face ((t (:foreground "Salmon4"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "FireBrick1" :weight bold))))
     (gnus-group-news-2-empty-face ((t (:foreground "darkorange3"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "dark orange" :weight bold))))
     (gnus-group-news-3-empty-face ((t (:foreground "turquoise4"))))
     (gnus-group-news-3-face ((t (:bold t :foreground "Aquamarine" :weight bold))))
     (gnus-group-news-4-empty-face ((t (:foreground "SpringGreen4"))))
     (gnus-group-news-4-face ((t (:bold t :foreground "SpringGreen2" :weight bold))))
     (gnus-group-news-5-empty-face ((t (:foreground "OliveDrab4"))))
     (gnus-group-news-5-face ((t (:bold t :foreground "OliveDrab2" :weight bold))))
     (gnus-group-news-6-empty-face ((t (:foreground "DarkGoldenrod4"))))
     (gnus-group-news-6-face ((t (:bold t :foreground "DarkGoldenrod3" :weight bold))))
     (gnus-group-news-low-empty-face ((t (:foreground "wheat4"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "tan4" :weight bold))))

     (gnus-header-name-face ((t (:bold t :foreground "SeaGreen" :italic t))))
     (gnus-header-content-face ((t (:foreground "SeaGreen"))))

     (gnus-header-from-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (gnus-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "SeaGreen" :slant italic))))

     (gnus-signature-face ((t (:italic t :foreground "yellow2" :slant italic))))
     (gnus-splash-face ((t (:foreground "Firebrick1"))))
     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MistyRose4" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "coral" :weight bold))))
     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground
                                                 "red1" :slant italic
                                                 :weight bold))))
     (gnus-summary-low-ancient-face ((t (:italic t :foreground "DarkSeaGreen4" :slant italic))))
     (gnus-summary-low-read-face ((t (:foreground "SeaGreen4"))))
     (gnus-summary-low-ticked-face ((t (:italic t :foreground "Green4" :slant italic))))
     (gnus-summary-low-unread-face ((t (:italic t :foreground "green3" :slant italic))))
     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
     (gnus-summary-normal-read-face ((t (:foreground "khaki4"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "khaki3"))))
     (gnus-summary-normal-unread-face ((t (:foreground "khaki"))))

     (gnus-summary-selected-face ((t (:foreground "yellow" :background "blue4"))))
     (gnus-summary-high-read-face ((t (:foreground "magenta"))))

     (header-line ((t (:underline nil))))
     (highlight ((t (:background "tomato3"))))
     (holiday-face ((t (:background "chocolate4"))))
     (ibuffer-deletion-face ((t (:foreground "red"))))
     (ibuffer-marked-face ((t (:foreground "green"))))
     (ido-first-match-face ((t (:foreground "magenta" :bold t :weight bold))))
     (ido-indicator-face ((t (:background "red" :foreground "yellow" :width condensed))))
     (ido-only-match-face ((t (:foreground "red"))))
     (ido-subdir-face ((t (:foreground "yellow"))))

     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))
     (info-header-xref ((t (:bold t :weight bold :foreground "cyan"))))
     (info-menu-5 ((t (:foreground "red1"))))
     (info-menu-header ((t (:bold t :underline t :weight bold))))
     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
     (info-xref ((t (:bold t :foreground "cyan" :weight bold))))

     (isearch ((t (:background "magenta4" :foreground "cyan1"))))
     (isearch-lazy-highlight-face ((t (:background "turquoise3"))))
     (italic ((t (:italic t :slant italic))))
     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "White"))))
     (message-header-cc-face ((t (:foreground "light cyan"))))
     (message-header-name-face ((t (:foreground "DodgerBlue1"))))
     (message-header-newsgroups-face ((t (:italic t :bold t :foreground
                                                  "LightSkyBlue3" :slant
                                                  italic :weight
                                                  bold))))
     (message-header-other-face ((t (:foreground "LightSkyBlue3"))))
     (message-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (message-header-to-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (message-header-xheader-face ((t (:foreground "DodgerBlue3"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (message-separator-face ((t (:background "cornflower blue" :foreground "chocolate"))))

     (mode-line ((t (:background "LightSlateGray" :foreground "black"))))
     (modeline ((t (:background "LightSlateGray" :foreground "black"))))
     (modeline-buffer-id ((t (:background "LightSlateGray" :foreground "blue4"))))
     (modeline-mousable ((t (:background "LightSlateGray" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:background "LightSlateGray" :foreground "green4"))))

     (mouse ((t (nil))))
     (muse-bad-link-face ((t (:bold t :foreground "coral" :underline "coral" :weight bold))))
     (muse-header-1 ((t (:bold t :family "helv" :weight bold :height 1.4 :foreground "blue"))))
     (muse-header-2 ((t (:bold t :family "helv" :weight bold :height 1.3 :foreground "yellow"))))
     (muse-header-3 ((t (:bold t :family "helv" :weight bold :height 1.2 :foreground "cyan"))))
     (muse-header-4 ((t (:bold t :family "helv" :weight bold :height 1.1))))
     (muse-header-5 ((t (:bold t :family "helv" :weight bold :height 1.0))))
     (muse-header-6 ((t (:bold t :family "helv" :weight bold :height 0.9))))
     (muse-link-face ((t (:bold t :foreground "cyan" :underline "cyan" :weight bold))))
     (region ((t (:background "RoyalBlue3" :foreground "white")))) ; blue
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "cyan" :foreground "black"))))
     (setnu-line-number-face ((t (:bold t :weight bold))))
     (show-paren-match-face ((t (:background "yellow4")))) ;;yellow
     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
     (tabbar-button-face ((t (:background "gray72" :family "helv"
     :foreground "magenta" :box (:line-width 2 :color "white"
     :style released-button) :height 0.8 :weight bold))))
     (template-message-face ((t (:bold t :weight bold))))
     (texinfo-heading-face ((t (:bold t :weight bold :foreground "blue"))))
     (tool-bar ((t (nil))))
     (trailing-whitespace ((t (:background "red"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "helv"))))
     (w3m-anchor-face ((t (:foreground "cyan"))))
     (w3m-arrived-anchor-face ((t (:foreground "LightSkyBlue"))))
     (w3m-bold-face ((t (:bold t :weight bold))))
     (w3m-current-anchor-face ((t (:bold t :underline t :weight bold))))
     (w3m-form-button-face ((t (:foreground "red" :underline t))))
     (w3m-form-button-mouse-face ((t (:foreground "red" :underline t))))
     (w3m-form-button-pressed-face ((t (:foreground "red" :underline t))))
     (w3m-form-face ((t (:foreground "red" :underline t))))
     (w3m-header-line-location-content-face ((t (:background "Gray20"
                                                             :foreground
                                                             "LightGoldenrod"))))

     (w3m-header-line-location-title-face ((t (:background "Gray20" :foreground "Cyan"))))
     (w3m-history-current-url-face ((t (:background "cyan" :foreground "LightSkyBlue"))))
     (w3m-image-face ((t (:foreground "PaleGreen"))))
     (w3m-strike-through-face ((t (:strike-through t))))
     (w3m-tab-background-face ((t (:background "white" :foreground "black"))))
     (w3m-tab-selected-face ((t (:background "cyan" :foreground "black"))))
     (w3m-tab-selected-retrieving-face ((t (:background "cyan" :foreground "red"))))
     (w3m-tab-unselected-face ((t (:background "blue" :foreground "black"))))
     (w3m-tab-unselected-retrieving-face ((t (:background "blue" :foreground "OrangeRed"))))
     (w3m-underline-face ((t (:underline t))))
     (widget-button-face ((t (:bold t :weight bold))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "DarkCyan"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "green3"))))
     (woman-addition-face ((t (:foreground "orange"))))
     (woman-bold-face ((t (:bold t :foreground "green2" :weight bold))))
     (woman-italic-face ((t (:italic t :underline t :slant italic))))
     (woman-unknown-face ((t (:foreground "cyan")))))))

(when window-system
  (color-theme-xwl))


;;; FUN

(require 'highlight-tail)
(highlight-tail-reload)

;;; .xwl-emacs-crucial.el ends here
