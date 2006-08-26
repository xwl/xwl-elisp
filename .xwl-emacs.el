;;; .xwl-emacs.el --- love life, love zly!

;; Copyright (C) 2003, 2004, 2005, 2006 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 2.11
;; Last updated: 2006/08/26 18:58:59
;;; Commentary:

;; William Xu's Ultimate `.emacs' !

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



;;; CONVENIENCE

(setq default-directory "~/")

(setq canlock-password "ac9f2efab48b539dd67c289f5eb50f1721edbd74")

;; general
;; -------

(require 'xwl-lib)

;; xwl-word-count-analysis (how many times a word has appeared).
(defun xwl-word-count-analysis (start end)
  "Count how many times each word is used in the region.
    Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
	(let* ((word (intern (match-string 0)))
	       (cell (assq word words)))
	  (if cell
	      (setcdr cell (1+ (cdr cell)))
	    (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))

;; insert line number before each line.
(defun xwl-numerate-lines ()
  "Insert line numbers into buffer"
  (interactive)
  (save-excursion
    (let ((max (count-lines (point-min) (point-max)))
	  (line 1))
      (goto-char (point-min))
      (while (<= line max)
	(insert (format "%4d " line))
	(beginning-of-line 2)
	(setq line (+ line 1))))))

;; a simple way of aligning columns
(defun his-align-cols (start end max-cols)
  "Align text between point and mark as columns.  Columns are separated by
whitespace characters.  Prefix arg means align that many columns. (default
is all)"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
	  pos
	  end-of-line
	  word
	  count
	  (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
	  (pos-list nil)
	  (ref-list nil))

      ;; find the positions
      (goto-char start)
      (while (< p end)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(setq pos (current-column))	;start of first word
	(if (null (car ref-list))
	    (setq pos-list (list pos))
	  (setq pos-list (list (max pos (car ref-list))))
	  (setq ref-list (cdr ref-list)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq word (- (current-column) pos))
	  ;; length of next word including following whitespaces
	  (setq pos (current-column))
	  (if (null (car ref-list))
	      (setq pos-list (cons word pos-list))
	    (setq pos-list (cons (max word (car ref-list)) pos-list))
	    (setq ref-list (cdr ref-list))))
	(while ref-list
	  (setq pos-list (cons (car ref-list) pos-list))
	  (setq ref-list (cdr ref-list)))
	(setq ref-list (nreverse pos-list))
	(forward-line)
	(setq p (point)))

      ;; align the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start
	    (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(goto-char (match-end 0))
	(setq pos (nth count pos-list))
	(while (< (current-column) pos)
	  (insert-char ?\040 1))
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq pos   (+  pos (nth count pos-list)))
	  (goto-char (match-end 0))
	  (while (< (current-column) pos)
	    (insert-char ?\040 1))
	  (setq end-of-line (save-excursion (end-of-line) (point))))
	(forward-line -1)
	(if (= p (point-min)) (setq p (1- p))
	  (setq p (point)))))))

;; resolve file names
(defun xwl-resolve-file-name (file type)
  "Resolve file name in various ways.

file is the abosolute filename.

type stands for different kinds of resolve.

 F  absolute pathname            ( /usr/local/bin/netscape.bin )
 f  file name without directory  ( netscape.bin )
 n  file name without extention  ( netscape )
 e  extention of file name       ( bin )"
  (cond
   ((string= type "F") file)
   ((string= type "f") (file-name-nondirectory file))
   ((string= type "n") (file-name-sans-extension (file-name-nondirectory file)))
   (t (file-name-extension file))))

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

;; count Chinese, English words
(defun xwl-count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
	(en-word 0)
	(total-word 0)
	(total-byte 0))
    (if xwl-emacs-special-p
	(setq cn-word (string-to-number (count-matches "\\cc" beg end))
	      en-word (string-to-number (count-matches "\\w+\\W" beg end)))
      (setq cn-word (count-matches "\\cc" beg end)
	    en-word (count-matches "\\w+\\W" beg end)))
    (setq total-word (+ cn-word en-word)
	  total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
		     total-word cn-word en-word total-byte))))

(defun xwl-hide-buffer ()
  "Hide current buffer, and enlarge the other one if exists."
  (interactive)
  (delete-windows-on (buffer-name)))

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

(defun xwl-list-ref (list ref)
  "Return the ref-th element of list."
  (if (= ref 0)
      (car list)
    (xwl-list-ref (cdr list) (1- ref))))

(defun xwl-info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))

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

;; dos <--> unix
(defun his-dos2unix ()
  "\r\n --> \r."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

(defun his-unix2dos ()
  "\n --> \r\n."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

;; hooks
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

;; compilation-mode, grep-find
;; (defun xwl-compilation-mode-hook ()
;;     (define-key compilation-mode-map (kbd "p") 'previous-line)
;;     (define-key compilation-mode-map (kbd "n") 'next-line)
;;     (define-key compilation-mode-map (kbd "q") 'xwl-hide-buffer))

;; (add-hook 'compilation-mode-hook 'xwl-compilation-mode-hook)

;; use whitespaces instead of \t
(setq-default indent-tabs-mode nil)

;; calendar
;; --------

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
      '((holiday-chinese 5 11 "My Day !")
        (holiday-chinese 7 7 "情人节")
        (holiday-chinese 10 10 "海豚公主的生日！& 民国双十节")))

(setq calendar-holidays
      (append calendar-holidays other-holidays))

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

;; Info-mode
(add-hook 'Info-mode-hook 'less-minor-mode-on)

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

;; mode-line
(setq default-mode-line-format
      (list "-"
	    'mode-line-mule-info
	    'mode-line-modified              " "
	    'mode-line-buffer-identification " "
	    '("%p")                          " "
	    '(line-number-mode "(%l, ")
	    '(column-number-mode "%c) ")
	    'global-mode-string
	    "%[("
	    'mode-name
	    'mode-line-process
	    'minor-mode-alist
	    "%n)%]--"
	    ;;'(which-func-mode ("" which-func-format "--"))
	    "-%-")
      mode-line-format default-mode-line-format)

;; flyspell
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode 1)
;; (add-hook 'cperl-mode-hook    'flyspell-prog-mode 1)
;; (add-hook 'makefile-mode-hook 'flyspell-prog-mode 1)
;; (add-hook 'python-mode-hook   'flyspell-prog-mode 1)
;; (add-hook 'sh-mode-hook       'flyspell-prog-mode 1)

(unless window-system
  (set-face-background 'highlight "red")
  (set-face-background 'show-paren-match-face "yellow"))

(setq ange-ftp-ftp-program-name "ftp"
      ange-ftp-gateway-ftp-program-name "ftp"
      ftp-program "ftp")
(setq visible-bell t)
;; (setq sentence-end "\\([]\\|\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
;;       sentence-end-double-space nil)
(setq sentence-end "\\([]\\|[.?!][]\"')}]*\\($\\| $\\|	\\|  \\)\\)[ 	\n]*")

;; recentf
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
	 (to-compl (mapcar (function
			    (lambda (x)
			      (cons (file-name-nondirectory x) x))) all-files))
	 (prompt (append '("File name: ") to-compl))
	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname to-compl)))))

(require 'redo)
(require 'goto-last-change)

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


;; bbdb
;; ----

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

;;zone
(setq zone-idle 300)

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

;; battery
;; -------

(require 'battery)
(setq battery-mode-line-format "[%b%p%% %t]"
      battery-mode-line-string
      (battery-format battery-mode-line-format
		      (funcall battery-status-function)))

(defadvice battery-update-handler (around check-on-line)
  "If on-line (not using battery), don't display on mode line."
  (if (>=  (string-to-number
	    (battery-format "%p" (funcall battery-status-function)))
	   90)
      (progn
	(setq battery-mode-line-string "")
	(force-mode-line-update))
    ad-do-it))

(ad-activate 'battery-update-handler)

(run-with-timer 0 battery-update-interval 'battery-update-handler)

(setq history-delete-duplicates t)

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

;;;; dired

(require 'wdired)
(autoload 'wdired-change-to-wdired-mode "wdired")

(require 'dired-x)
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top)

;; (require 'dired-view)

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

;; sort
(setq dired-listing-switches "-lh")

;; sort:directories first
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
    (save-excursion
       (let (buffer-read-only)
            (forward-line 2) ;; beyond dir. header
	         (sort-regexp-fields t "^.*$" "[ ]*." (point)
		 (point-max))))
      (and (featurep 'xemacs)
             (fboundp 'dired-insert-set-properties)
	            (dired-insert-set-properties (point-min) (point-max)))
        (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'sof/dired-sort)

;; sort::long-time
(defun xwl-dired-sort-by-default ()
  (interactive)
  (setq dired-listing-switches "-lh")
  (dired-sort-other dired-listing-switches))

(defun xwl-dired-sort-by-show-all ()
  (interactive)
  (setq dired-listing-switches "-lhA")
  (dired-sort-other dired-listing-switches))

(defun xwl-dired-sort-by-for-solaris ()
  "Solaris `ls' doesn't support `-h' option, stupid!"
  (interactive)
  (setq dired-listing-switches "-lA")
  (dired-sort-other dired-listing-switches))

;; sort::tmp
(defun xwl-dired-sort-by-date ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "tr")))

(defun xwl-dired-sort-by-extenstion ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "X")))

(defun xwl-dired-sort-by-invisible-only ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "d .*")))

;; dired-omit-files
(setq dired-omit-extensions
      '("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg"
	".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm"
	".class" ".fas" ".lib" ".x86f" ".sparcf" ".lo" ".la"
	".toc" ".log" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps"
	".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof"
	".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky"
	".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs"))

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

(defun xwl-dired-mode-hook ()
;;   (local-set-key (kbd "f") 'xwl-dired-forward)
;;   (local-set-key (kbd "b") 'xwl-dired-backward)

  (local-set-key (kbd "* f") 'find-name-dired)
  (local-set-key (kbd "* g") 'grep-find)
  ; (define-key dired-mode-map (kbd "RET") 'joc-dired-single-buffer)
  (define-key dired-mode-map (kbd "T") 'dired-advertised-find-file)
  (define-key dired-mode-map (kbd "v") 'xwl-dired-w3m-find-file)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

  (define-key dired-mode-map (kbd "s") nil)
  (define-key dired-mode-map (kbd "s RET") 'xwl-dired-sort-by-default)
  (define-key dired-mode-map (kbd "s a") 'xwl-dired-sort-by-show-all)
  (define-key dired-mode-map (kbd "s t") 'xwl-dired-sort-by-date)
  (define-key dired-mode-map (kbd "s X") 'xwl-dired-sort-by-extenstion)
  (define-key dired-mode-map (kbd "s s") 'xwl-dired-sort-by-for-solaris)
  (define-key dired-mode-map (kbd "s .") 'xwl-dired-sort-by-invisible-only)

  ;; (define-key dired-mode-map (kbd "l") 'xwl-dired-last-dir)
;;   (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
;;   (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)

)

(add-hook 'dired-mode-hook 'xwl-dired-mode-hook)
;;(remove-hook 'dired-mode-hook 'dired-view-minor-mode-on)

(require 'dired-isearch)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward-regexp)
(define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward)

;;;; buffers

;; tabbar

(setq xwl-hated-buffers
      '("*info*" "*Bookmark List*" "*scratch*" "*Compile-Log*"
	"*Completions*" "do" "*Occur*" ".diary" ".bbdb" "*desktop*"
	"*Playlist*" "*Messages*" "*compilation*" "*Ediff Registry*"
	"*Kill Ring*" "*Shell Command Output*" "*Holidays*"
	"*Help*" "*Async Shell Command*" ".newsrc-dribble"
	"*Calendar*" "*eshell*"))

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-fontification-level t)
(setq ibuffer-never-show-regexps nil)

(setq ibuffer-formats
      '((mark modified read-only
	      " " (name 16 -1)
	      " " (mode 16 16)
	      " " (size 6 -1 :right)
	      " " filename) (mark " " (name 16 -1) " " filename)))

(defun xwl-ibuffer-forward-line ()
  (interactive)
  (ibuffer-forward-line)
  (ibuffer-visit-buffer-other-window-noselect))

(defun xwl-ibuffer-backward-line ()
  (interactive)
  (ibuffer-backward-line)
  (ibuffer-visit-buffer-other-window-noselect))

(defun xwl-ibuffer ()
  (interactive)
  (split-window-horizontally -25)
  (windmove-right)
  (ibuffer)
  (ibuffer-do-sort-by-major-mode))

(defun xwl-ibuffer-quit ()
  (interactive)
  (ibuffer-quit)
  (xwl-hide-buffer))

(global-set-key (kbd "<f4>") 'xwl-ibuffer)

(define-key ibuffer-mode-map (kbd "n") 'xwl-ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "p") 'xwl-ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "q") 'xwl-ibuffer-quit)

(setq ibuffer-saved-filters
      `(("mail"
	 ((or (mode . message-mode)
	      (mode . mail-mode)
	      (mode . gnus-group-mode)
	      (mode . gnus-summary-mode)
	      (mode . gnus-article-mode))))
	("lisp"
	 ((or (mode . lisp-mode)
	      (mode . emacs-lisp-mode)
	      (mode . lisp-interaction-mode)
	      (mode . sawfish-mode)
	      (mode . scheme-mode)
	      (mode . inferior-scheme-mode)
	      (mode . log-edit-mode))))
	("c_c++"
	 ((or (mode . c-mode)
	      (mode . c++-mode)
	      (mode . makefile-mode)
	      (mode . makefile-gmake-mode))))
	("java"
	 ((mode . java-mode)))
	("dired"
	 ((mode . dired-mode)))
	("web"
	 ((or (mode . sql-mode)
	      (mode . sql-interactive-mode)
	      (mode . php-mode)
	      (mode . html-mode)
	      (mode . css-mode)
	      (filename . "\\*SQL\\*"))))
	("help"
	 ((or (mode . help-mode)
	      (mode . apropos-mode)
	      (mode . Info-mode)
	      (mode . Man-mode)
	      (mode . woman-mode)
	      (mode . custom-mode))))
	("text"
	 ((or (mode . text-mode)
	      (mode . outline-mode)
	      (mode . emacs-wiki-mode)
	      (mode . muse-mode)
	      (mode . tex-mode)
	      (mode . latex-mode)
	      (mode . metapost-mode))))
	("process"
	 ((or (mode . comint-mode)
	      (mode . compilation-mode)
	      (mode . internal-ange-ftp-mode)
	      (mode . term-mode)
	      (mode . eshell-mode))))
	("erc"
	 ((mode . erc-mode)))
	("hated"
	 ((name . ,(regexp-opt xwl-hated-buffers))
	  (mode . fundamental-mode)))))

(when xwl-emacs-special-p
  (setq ibuffer-saved-limits ibuffer-saved-filters))

;; ido

(when (string= (substring emacs-version 0 2) "21")
  (load "~/.emacs.d/site-lisp-21/ido.el"))

(require 'ido)
(ido-mode 1)

(setq xwl-regexp-ignore-buffers
      '("\\*.*\\*" "\\.newsrc-dribble" "\\.bbdb" "sent-mail"
	"diary" "^\\ .*" "NEWS" "TAGS.*" "do"))

(setq ido-max-work-file-list 30
      ido-max-work-directory-list 10
      ido-ignore-buffers xwl-regexp-ignore-buffers
      ido-create-new-buffer 'always
      ido-max-window-height 5)

(unless (string= (substring emacs-version 0 2) "21")
  (ido-everywhere 1))

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

(require 'midnight)
;; Setting this variable outside customize has no effect
;; (setq midnight-mode t)
(setq clean-buffer-list-kill-never-buffer-names
      '("*Messages*" ".scratch" ".xwl-emacs.el" ".xwl_diary"))

;; session
;; * In the minibuffer, enter <M-?> to display a completion list with
(setq session-initialize t)
(setq session-globals-exclude
      '(load-history register-alist vc-comment-ring
		     flyspell-auto-correct-ring
		     planner-browser-file-display-rule-ring)
      session-globals-regexp "-\\(ring\\|history\\)\\'")

;; save cursor's place
;; possible bugs, cause open/close file error.
(require 'saveplace)
(setq-default save-place t)
;;;; keys

;; about keys' organizations

;; C-c c *    invoke external programs, e.g., scheme, mysql.
;; C-c e *    emms

(global-set-key (kbd "C-x <left>") 'next-buffer)
(global-set-key (kbd "C-x <right>") 'previous-buffer)

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

(global-set-key (kbd "<prior>") 'scroll-other-window-down)
(global-set-key (kbd "<next>")  'scroll-other-window)
(global-set-key (kbd "<home>")  'xwl-scroll-other-window-down-one-line)
(global-set-key (kbd "<end>")   'xwl-scroll-other-window-up-one-line)
(global-set-key (kbd "C-x <up>")   'delete-other-windows)
(global-set-key (kbd "C-x <down>") 'xwl-hide-buffer)

(global-set-key (kbd "C-<up>")    'smart-compile)
(global-set-key (kbd "C-<down>")  'smart-run)
(global-set-key (kbd "C-<left>")  'previous-error)
(global-set-key (kbd "C-<right>") 'next-error)

;; conflicts

(dolist (hook '(dired-mode-hook calendar-move-hook gnus-summary-mode-hook
				gnus-group-mode-hook clone-buffer-hook))
  (add-hook hook (lambda ()
		   (if (not (one-window-p))
		       (local-set-key (kbd "<end>") 'xwl-scroll-other-window-up-one-line))
		   (local-set-key (kbd "M-n") 'xwl-scroll-up-one-line)
		   (local-set-key (kbd "M-p")
				'xwl-scroll-down-one-line))))

(add-hook 'w3m-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-n") 'xwl-scroll-up-one-line)
	    (local-set-key (kbd "M-p") 'xwl-scroll-down-one-line)
	    (local-set-key (kbd "<left>") 'w3m-previous-buffer)
	    (local-set-key (kbd "<right>") 'w3m-next-buffer)
	    (local-set-key (kbd "p") 'w3m-previous-buffer)
	    (local-set-key (kbd "n") 'w3m-next-buffer)
	    (local-set-key (kbd "c") 'w3m-delete-buffer)))

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "M-n") 'xwl-scroll-up-one-line)
	     (local-set-key (kbd "M-p") 'xwl-scroll-down-one-line)))

(when window-system
  (global-unset-key (kbd "C-z")))

(global-set-key (kbd "C-c n t") 'toggle-truncate-lines)

(global-set-key (kbd "C-c m k") 'kill-this-buffer)

(global-set-key (kbd "C-c n D") 'delete-region)

;;;; window

;; redo and undo
(unless xwl-emacs-special-p
 (winner-mode 1))

;; jump by name
;; (require 'winring)
;; (winring-initialize)

;; (setq winring-show-names nil)
;; (define-key winring-map "n" 'winring-next-configuration)
;; (define-key winring-map "o" 'winring-new-configuration)

;; jump by registers
;; C-x r w
;; C-x r j

;; comint, shell, ansi
;; -------------------

(require 'ansi-color)

(defun xwl-comint-mode-hook ()
  (local-set-key (kbd "M-p") (lambda () (interactive) (comint-previous-input 1)))
  (local-set-key (kbd "M-n") (lambda () (interactive) (comint-next-input 1)))

  (ansi-color-for-comint-mode-on)
  (turn-off-auto-fill))

(add-hook 'comint-mode-hook 'xwl-comint-mode-hook)

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)

;; cursor/page movements
;; ---------------------

;; "f" in vi(go to char)
(defun xwl-forward-char (n char)
  "Move forward to Nth occurence of CHAR.  Typing `xwl-forward-char-key'
again will move forwad to the next Nth occurence of CHAR."
  (interactive "p\ncForward to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(defun xwl-backward-char (n char)
  "Move backward to Nth occurence of CHAR.  Typing `xwl-backward-char-key'
again will move forwad to the next Nth occurence of CHAR."
  (interactive "p\ncBackward to char: ")
  (search-backward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-backward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(global-set-key (kbd "C-o") 'xwl-forward-char)
(global-set-key (kbd "M-o") 'xwl-backward-char)

;; scroll up and down
(defun xwl-scroll-down-one-line (&optional arg)
  "Scroll text of current window down ARG lines; or 1 line if no ARG."
  (interactive "P")
  (scroll-down (if arg (prefix-numeric-value arg) 1)))

(defun xwl-scroll-up-one-line (&optional arg)
  "Scroll text of current window up ARG lines; or 1 line if no ARG."
  (interactive "P")
  (scroll-up (if arg (prefix-numeric-value arg) 1)))

(defun xwl-scroll-other-window-down-one-line (&optional arg)
  "Scroll text of other window down ARG lines; or 1 line if no ARG."
  (interactive "P")
  (scroll-other-window (if arg (prefix-numeric-value arg) -1)))

(defun xwl-scroll-other-window-up-one-line (&optional arg)
  "Scroll text of other window up ARG lines; or 1 line if no ARG."
  (interactive "P")
  (scroll-other-window (if arg (prefix-numeric-value arg) 1)))

(global-set-key (kbd "M-p") 'xwl-scroll-down-one-line)
(global-set-key (kbd "M-n") 'xwl-scroll-up-one-line)

;; date
;; ----

(defun xwl-insert-date ()
  (interactive)
  (insert (xwl-get-date)))

(defun xwl-get-date ()
  (format-time-string "%Y/%m/%d %H:%M:%S" (current-time)))

(defun xwl-update-date ()
  "Auto update '[Ll]ast [Uu]pdated:' part if exists, after file saved."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward-regexp "Last\\ updated:" nil t)
      (progn
	(kill-line)
	(insert " ")
	(xwl-insert-date)))
    nil))

(global-set-key (kbd "C-c m d") 'xwl-insert-date)

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
;;       (set-default-font "fontset-bvsmono110"))
;;     (defun xwl-setup-font() 'font-setup-done)))

(if (or (string= (substring emacs-version 0 2) "21") ; Emacs <--> X
	(not window-system))
    (set-clipboard-coding-system 'gb2312)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq x-select-enable-clipboard t)

(prefer-coding-system 'utf-8)

;; set preferred coding system
(setq file-coding-system-alist
      '(("\\.dz\\'" no-conversion . no-conversion)
	("\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\'" no-conversion . no-conversion)
	("\\.tgz\\'" no-conversion . no-conversion)
	("\\.tbz\\'" no-conversion . no-conversion)
	("\\.bz2\\'" no-conversion . no-conversion)
	("\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'" no-conversion . no-conversion)
	("\\.elc\\'" emacs-mule . emacs-mule)
	("\\.utf\\(-8\\)?\\'" . utf-8)
	("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
	("\\.tar\\'" no-conversion . no-conversion)
	("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
	("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system)
;;	("" undecided)
	("^/home/william/studio/darcs/mt/src/" chinese-gbk . chinese-gbk)
	("" utf-8 . utf-8)))

(defun xwl-revert-buffer-with-coding-system ()
  "Revert buffer with 'gbk coding."
  (interactive)
  (revert-buffer-with-coding-system 'gbk))

(global-set-key (kbd "C-c n r") 'xwl-revert-buffer-with-coding-system)

(setq auto-mode-alist
      (append
       `(,@(mapcar (lambda (arg)
		     (cons (concat (regexp-opt `(,(car arg))) "$")
			   (cadr arg)))
		   '(("makefile" makefile-mode)
		     (".h"    c++-mode)
		     (".lrc"  text-mode)
		     (".sh"   shell-script-mode)
		     (".m"    octave-mode)
		     (".java" java-mode)
		     (".l"    c-mode)
		     (".jl"   sawfish-mode)
		     (".JPG"  image-mode)))
	 ("\\(rc\\|.conf\\)$"   . conf-mode)
	 ("\\(.mac\\|.lst\\)$"  . asm-mode)
	 ("\\(.html\\|.htm\\)$" . html-helper-mode))
       auto-mode-alist))

;; recentf
(require 'recentf)
(recentf-mode t)
(define-key recentf-dialog-mode-map (kbd "ESC TAB") 'widget-backward)
(define-key recentf-dialog-mode-map (kbd "n") 'widget-forward)
(define-key recentf-dialog-mode-map (kbd "p") 'widget-backward)

(setq default-major-mode 'text-mode)

;; occur
(defun xwl-occur-previous-line ()
  (interactive)
  (previous-line 1)
  (occur-mode-goto-occurrence)
  (other-window 1))

(defun xwl-occur-next-line ()
  (interactive)
  (next-line 1)
  (occur-mode-goto-occurrence)
  (other-window 1))

(define-key occur-mode-map (kbd "p") 'xwl-occur-previous-line)
(define-key occur-mode-map (kbd "n") 'xwl-occur-next-line)
(define-key occur-mode-map (kbd "M-p") 'xwl-scroll-down-one-line)
(define-key occur-mode-map (kbd "M-n") 'xwl-scroll-up-one-line)
(define-key occur-mode-map (kbd "q")
  '(lambda () (interactive) (delete-windows-on (buffer-name))))

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default auto-fill-function 'do-auto-fill)
;; (setq default-justification 'full)
(setq default-fill-column 72
      adaptive-fill-regexp
      "[	]*\\([-|#;>*]+[	]*\\|(?[0-9]+[.)][	]*\\)*"
      adaptive-fill-first-line-regexp "\\`[	]*\\'"
      fill-column 72)
(show-paren-mode t)
(menu-bar-mode -1)
(setq column-number-mode t
      line-number-mode t)
(setq show-paren-style 'expression
      scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000
      hscroll-step 1
      hscroll-margin 3)
(resize-minibuffer-mode 1)
(global-auto-revert-mode 1)

;; display time
(setq display-time-format "%a %m/%d/%H:%M")
(display-time)

;; colors & faces
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq transient-mark-mode t)

;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-whole-kill
;;	senator-try-expand-semantic
	try-expand-dabbrev-visible
	try-expand-dabbrev-from-kill
	try-expand-dabbrev-all-buffers
	try-expand-all-abbrevs
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-list
;	try-complete-lisp-symbol-partially
;;	try-complete-lisp-symbol
        try-expand-line
	try-expand-line-all-buffers))

;; woman
(require 'woman)
(setq woman-fill-column default-fill-column
      woman-cache-level 3)    ; `C-u woman' sometimes to update manpages.

;; deal with large buffer
(setq font-lock-support-mode 'fast-lock-mode)
;; (setq font-lock-support-mode 'lazy-lock-mode
;;       lazy-lock-defer-on-scrolling t
;;       font-lock-support-mode 'lazy-lock-mode
;;       font-lock-maximum-decoration t)
(setq font-lock-maximum-size
      '((c-mode . 256000)
	(c++-mode . 256000)
	(rmail-mode . 1048576)
	(tex-mode . 1048576)))

;;;; misc
(setq kill-ring-max 100
      ring-bell-function 'ignore
      uniquify-buffer-name-style 'forward
      auto-image-file-mode nil
      inhibit-startup-message t
      max-lisp-eval-depth 20000
      c-echo-syntactic-information-p nil)
(fset 'yes-or-no-p 'y-or-n-p)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
(put 'overwrite-mode 'disabled t)
(setq require-final-newline 't)
(setq-default truncate-lines t
	      truncate-partial-width-windows t)

(require 'page-break)
(turn-on-page-break-mode)

(mouse-avoidance-mode 'animate)

;; less
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

(defun his-one-whitespace-between-ce (&optional start end)
  "Automatically insert a whitespace between Chinese and English,
Chinese and digits, which is useful when editing TeX files."
  (interactive)
  (save-excursion
    (unless start (setq start (point-min)))
    (unless end (setq end (point-max)))
    (goto-char start)
    (while (re-search-forward "\\(\\cc\\)\\([0-9a-zA-Z]\\)" end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char start)
    (while (re-search-forward "\\([0-9a-zA-Z]\\)\\(\\cc\\)" end t)
      (replace-match "\\1 \\2" nil nil))))

(global-set-key (kbd "C-c m o") 'his-one-whitespace-between-ce)

;; help/doc buffers
;; ----------------

(defun xwl-jump-to-help ()
  "Focus cusor on the help-mode buffer."
  (unless (eq major-mode 'help-mode)
    (other-window 1)))

(defadvice describe-mode (after jump-to-help)
  (xwl-jump-to-help))

(defadvice describe-bindings (after jump-to-help)
  (xwl-jump-to-help))

(defadvice describe-function (after jump-to-help)
  (xwl-jump-to-help))

(defadvice describe-variable (after jump-to-help)
  (xwl-jump-to-help))

(defadvice describe-key (after jump-to-help)
  (xwl-jump-to-help))

(ad-activate 'describe-mode)
(ad-activate 'describe-bindings)
(ad-activate 'describe-function)
(ad-activate 'describe-variable)
(ad-activate 'describe-key)

;; transpose(interchange) two windows
(defun his-transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-c m t") 'his-transpose-windows)

;; passwd protect
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;; Bindings
;; --------

;; special
(keyboard-translate ?\C-h ?\C-?)
(keyboard-translate ?\C-? ?\C-d)
;; (global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(when window-system
  (keyboard-translate ?\C-m ?\C-@))

(keyboard-translate ?\C-i ?\M-%)

(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f2>") 'woman)
(global-set-key (kbd "<f5>") 'w3m)
(global-set-key (kbd "<f9>") 'shell);xwl-term) ;xwl-run-scsh)
(global-set-key (kbd "<f11>") 'repeat)
(global-set-key (kbd "<f13>") 'kill-this-buffer)


(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<left>")  'windmove-left)
(global-set-key (kbd "<up>")    'windmove-up)
(global-set-key (kbd "<down>")  'windmove-down)

(global-set-key (kbd "M-C-<left>")  'windmove-left)
(global-set-key (kbd "M-C-<right>") 'windmove-right)
(global-set-key (kbd "M-C-<up>")    'windmove-up)
(global-set-key (kbd "M-C-<down>")  'windmove-down)

;; common
(global-set-key (kbd "ESC C-s") 'isearch-forward)
(global-set-key (kbd "ESC C-r") 'isearch-backward)
(global-set-key (kbd "ESC \\") 'just-one-space)
(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'replace-regexp)
(global-set-key (kbd "M-_") 'highlight-changes-previous-change)
(global-set-key (kbd "M-+") 'highlight-changes-next-change)
(global-set-key (kbd "M-s") 'dictionary-search)
(global-set-key (kbd "M-S") 'dictionary-search)
;; (global-set-key (kbd "M-g") 'goto-line)
;; (global-set-key (kbd "%") 'xwl-match-paren)
(global-set-key (kbd "C-M-k") 'kill-paragraph)
(global-set-key (kbd "M-K") 'kill-sexp)
(global-set-key (kbd "M-H") 'mark-sexp)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "M-C") 'capitalize-region)
(global-set-key (kbd "M-U") 'upcase-region)
(global-set-key (kbd "M-L") 'downcase-region)

;; C-x
(global-set-key (kbd "C-x C-_") 'redo)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

(global-set-key (kbd "C-x ~ p") 'c++-mode)
(global-set-key (kbd "C-x ~ c") 'c-mode)
(global-set-key (kbd "C-x ~ s") 'sh-mode)
(global-set-key (kbd "C-x ~ l") 'lisp-mode)
(global-set-key (kbd "C-x ~ e") 'emacs-lisp-mode)
(global-set-key (kbd "C-x ~ O") 'outline-mode)
(global-set-key (kbd "C-x ~ t") 'text-mode)
(global-set-key (kbd "C-x ~ w") 'emacs-wiki-mode)
(global-set-key (kbd "C-x ~ S") 'scheme-mode)
(global-set-key (kbd "C-x ~ m") 'asm-mode)
(global-set-key (kbd "C-x ~ b") 'blank-mode)
(global-set-key (kbd "C-x ~ v") 'view-mode)


(global-set-key (kbd "C-x ~ F") 'follow-mode)
;; (global-set-key (kbd "C-x ~ d") 'folding-mode)
(global-set-key (kbd "C-x ~ L") 'latex-mode)
(global-set-key (kbd "C-x ~ M") 'metapost-mode)
(global-set-key (kbd "C-x ~ P") 'picture-mode)
(global-set-key (kbd "C-x ~ T") 'font-lock-mode)

(global-set-key (kbd "C-x ~ a") 'auto-fill-mode)
(global-set-key (kbd "C-x ~ f") 'flyspell-mode)

(global-set-key (kbd "C-x ~ r") 'toggle-read-only)
(global-set-key (kbd "C-x ~ x") 'tex-mode)
(global-set-key (kbd "C-x ~ o") 'octave-mode)
(global-set-key (kbd "C-x ~ h") 'highlight-changes-mode)

;; (global-set-key (kbd "C-x r C-@") 'rm-set-mark)
(global-set-key (kbd "C-x r C-m") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(global-set-key (kbd "C-x r C-y") 'yank-rectangle)

;; C-c
(global-set-key (kbd "C-c f") 'recentf-open-files-compl)
(global-set-key (kbd "C-c F") 'ffap)
(global-set-key (kbd "C-c a") 'apropos)
(global-set-key (kbd "C-c o") '(lambda () (interactive)
				 (call-interactively 'occur)
				 (other-window 1)))
(global-set-key (kbd "C-c j") 'imenu)
(global-set-key (kbd "C-c l") 'hide-lines)
(global-set-key (kbd "C-c L") 'show-all-invisible)
;; (global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c p b") 'planner-browser-directory)

(global-set-key (kbd "C-c m r") 'revert-buffer)
(global-set-key (kbd "C-c m R") 'rename-buffer)
;; (global-set-key (kbd "C-c m n") 'nuke-trailing-whitespace)
(global-set-key (kbd "C-c m a") 'his-align-cols)
(global-set-key (kbd "C-c m m") 'apply-macro-to-region-lines)
(global-set-key (kbd "C-c m q") 'query-replace-regexp)
(global-set-key (kbd "C-c m h") 'htmlize-file)
(global-set-key (kbd "C-c m v") 'visit-tags-table)
(global-set-key (kbd "C-c m b") 'smart-beautify-operator)
(global-set-key (kbd "C-c m s") '(lambda () (interactive)
				   (unless (get-buffer ".scratch")
				     (find-file "~/.scratch"))
				   (switch-to-buffer ".scratch")))
(global-set-key (kbd "C-c m c") 'xwl-count-ce-word)
(global-set-key (kbd "C-c m e") '(lambda () (interactive)
				   (call-interactively 'eval-region)
				   (message "eval-region...done")))
(global-set-key (kbd "C-c C-\\") 'c-backslash-region)

;; (global-set-key (kbd "C-c b") (lambda () (interactive)
;; 				(xwl-start-process-shell-command
;; 				 "sawfish-client -f iswitch-window")))

;; operators
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

(global-set-key (kbd "C-c n d")
		(lambda ()
		  (interactive)
		  (insert
		   (car
		    (split-string
		     (shell-command-to-string "LC_ALL=C date") "\n")))))

;; html
;; (define-key html-mode-map (kbd "<") 'skeleton-pair-insert-maybe)


(setq exec-path
      ' ("~/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/bin/X11"
         "/usr/games" "/usr/lib/emacs/22.0.50/powerpc-linux-gnu"))

;; (redefined)Use file's full path as buffer name when opening files
;; with same names.
(defun create-file-buffer (filename)
  (let ((lastname (file-name-nondirectory filename)))
    (if (string= lastname "")
    (setq lastname filename))
    (if (get-buffer lastname)
        (generate-new-buffer filename)
      (generate-new-buffer lastname))))

(defun xwl-revert-buffer-with-sudo ()
  "Revert buffer using tramp sudo.
And also reserve changes made by non-root user before."
  (interactive)
  (let ((buf (current-buffer))
        (filename (buffer-file-name))
        (buf-content (when (buffer-modified-p)
                       (widen)
                       (buffer-string))))
    (with-current-buffer buf
      (save-excursion
        (if (file-writable-p filename)
            (revert-buffer)
          (kill-buffer (current-buffer))
          (find-file (concat "/sudo::" filename))
          (when buf-content
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert buf-content)))))))

(global-set-key (kbd "C-c m R") 'xwl-revert-buffer-with-sudo)


;;; PROGRAMMING

;; skeletons
;; ---------

;;  c
(define-skeleton skeleton-c-mode-main-fun
  "generate main(int argc, char *argv[])"
  > "int\nmain(int argc, char *argv[])\n{\n"
  > _ " "
  > "\n\nexit(0);"
  > "\n}")

(define-skeleton skeleton-c-mode-main-fun1
  "generate main()"
  > "int\nmain()\n{\n"
  > _ " "
  > "\n\nexit(0);"
  > "\n}")

(define-skeleton skeleton-c-mode-include
  "Generate include<>."
  > "#include <"
  (ido-completing-read
   "Include File: "
   (apply 'append
          (mapcar (lambda (dir) (directory-files dir))
                  '("/usr/include"
                    "~/include")))) ">\n")

;; c++
(define-skeleton skeleton-c++-mode-main-fun
  "generate int main(int argc, char *argv[])"
  > "int main(int argc, char *argv[])\n{\n"
  > _ " "
  > "\n}")

(define-skeleton skeleton-c++-mode-main-fun1
  "generate int main()"
  > "int main()\n{\n"
  > _ ""
  > "\n}")

(define-skeleton skeleton-c++-mode-include
  "Generate include<>."
  > "#include <"
  (ido-completing-read
   "Include file: "
   (apply 'append
          (mapcar (lambda (dir) (directory-files dir))
                  '("/usr/include"
                    "/usr/include/c++/4.0" )))) ">\n")

;; java
(define-skeleton skeleton-jde-mode-main-fun
  "Generate: public static void main(String[] args)"
  > "public static void main(String[] args)\n"
  > "{\n"
  > _ " "
  > "\n        }")

(define-skeleton skeleton-jde-mode-print-fun
  "Generate: System.out.println()"
  > "System.out.println("
  > _ ""
  > ");")

;; emacs-wiki
(define-skeleton skeleton-emacs-wiki-mode-example
  "Generate: <example></example>."
  > "<example>\n"
  > _ " "
  > "\n</example>")

(define-skeleton skeleton-emacs-wiki-mode-div-text
  "Generate: <div class = \"my_text\"> </example></div>."
  > "<div class = \"my_text\"><example>\n"
  > _ " "
  > "\n</example></div>")

(define-skeleton skeleton-emacs-wiki-mode-div-code
  "Generate: <div class = \"my_code\"> </example></div>."
  > "<div class = \"my_code\"><example>\n"
  > _ " "
  > "\n</example></div>")

(define-skeleton skeleton-emacs-wiki-mode-div-emph
  "Generate: <div class = \"my_emph\"> </example></div>."
  > "<div class = \"my_emph\"><example>\n"
  > _ " "
  > "\n</example></div>")

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

;; asm
(require 'asm-mode)
(define-key asm-mode-map (kbd ":") '(lambda () (interactive) (smart-insert-operator ":" t)))
(define-key asm-mode-map (kbd ",") '(lambda () (interactive) (smart-insert-operator "," t)))
(define-key asm-mode-map (kbd "RET") 'newline)

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

;; makefile
(defun xwl-makefile-mode-hook ()
  (smart-insert-operator-hook)
  (local-unset-key (kbd ".")))

(add-hook 'makefile-mode-hook 'xwl-makefile-mode-hook)

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

;; misc
(global-set-key (kbd "C-x v =") 'ediff-revision)
(global-set-key (kbd "C-c m D") 'toggle-debug-on-error)


;; code browsing

;; imenu, cscope, etags
;; speedbar, semantic, eieio, ecb, xref

;; hs-minor-mode
(require 'hideshow)
(defun xwl-hs-minor-mode-hook ()
  (define-key hs-minor-mode-map (kbd "C-c @ DEL") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c @ ESC DEL") 'hs-hide-all))

(global-set-key (kbd "C-c m g") 'grep)

;; Highlight Special Keywords
;; --------------------------

(setq xwl-keyword-highlight-modes
      '(
	php-mode java-mode c-mode c++-mode emacs-lisp-mode scheme-mode
	text-mode outline-mode
	))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-todo-face)

(modify-face 'font-lock-fixme-face "black" "yellow" nil t nil t nil nil)
(modify-face 'font-lock-todo-face  "black" "yellow" nil t nil nil nil nil)

(defun xwl-highlight-special-keywords ()
  (mapc (lambda (mode)
	  (font-lock-add-keywords
	   mode
	   '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t)
	     ("\\<\\(TODO\\)" 1 'font-lock-todo-face t))))
	xwl-keyword-highlight-modes))

(xwl-highlight-special-keywords)

;; gdb
(setq gdb-many-windows t)
(global-set-key (kbd "<C-end>") 'gdb-restore-windows)

;;;; smart-compile, smart-operator

;; smart-compile, smart-operator
(require 'smart-compile+)
(require 'smart-operator)

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
;;;; c, c++, java

;; doxymacs

;; cc-mode
(require 'cc-mode)
(require 'ctypes)
(ctypes-auto-parse-mode 1)
(autoload 'expand-member-functions
  "member-functions" "Expand C++ member function declarations" t)
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
;;   (doxymacs-mode 1)
;;   (doxymacs-font-lock)
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

;; lex, yacc
(defun xwl-lex-yacc-mode ()
  "Redefine some operators to enhace readability, when editing lex, or
yacc source files."
  (interactive)

  (defun insert-little-line ()
    (interactive)
    (forward-line 0)
    (insert "\t\t| "))

  (defun insert-semicolon ()
    (interactive)
    (insert "           ")		; ten whitespaces
    (forward-line 0)
    (forward-char 8)
    (insert ": "))

  (local-unset-key (kbd "{"))
  (local-unset-key (kbd ";"))
  (local-set-key (kbd "|") 'insert-little-line)
  (local-set-key (kbd ":") 'insert-semicolon))

;; java
;; (require 'jde)

;; python

(defun xwl-python-mode-hook ()
  (smart-insert-operator-hook)
  (local-unset-key (kbd ".")))

(add-hook 'python-mode-hook 'xwl-python-mode-hook)

;; cscope
(load-file "/usr/share/emacs/site-lisp/xcscope.el")
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
;;;; lisp, scheme, guile

(require 'bracketphobia)

;; lisp
(require 'lisp-mnt)
(defun xwl-lisp-mode-hook ()
  (which-func-mode 1)
  (eldoc-mode 1)

  (set (make-local-variable 'outline-regexp) ";;;+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "<backtab>") 'lisp-complete-symbol)
  (local-set-key (kbd "C-x C-r") 'eval-region)
  (local-set-key (kbd "C-x C-b") 'eval-buffer))

(add-hook 'lisp-mode-hook 'xwl-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'xwl-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'xwl-lisp-mode-hook)

;; scheme
(require 'scheme)
(require 'srfi)
(setq scheme-program-name "guile")
(defun xwl-scheme-mode-hook ()
  (setq comment-add 1)

  (set (make-local-variable 'outline-regexp) ";;;+ ")
  (outline-minor-mode 1)

  (local-set-key (kbd "C-x C-r") 'scheme-send-region)
  (local-set-key (kbd "C-x C-b") 'xwl-scheme-send-buffer))

(add-hook 'scheme-mode-hook 'xwl-scheme-mode-hook)

(defun xwl-scheme-send-buffer ()
  (interactive)
  (scheme-send-region (point-min) (point-max)))

;; change `|' to normal (default, it's set to \")
(modify-syntax-entry ?\| "_   " scheme-mode-syntax-table)

;; (defun xwl-scheme-print-output ()
;;   "Get last session's output."
;;   (interactive)
;;   (with-current-buffer scheme-buffer
;;     (save-excursion
;;       (goto-char (point-max))
;;       (when (<= (current-column) (length "guile> "))
;;         (search-backward "guile>")
;;         (re-search-backward "guile> \\(\\(.*\n\\)+\\)")
;;         (let ((str (match-string-no-properties 1)))
;;           (message (substring str 0 (1- (length str)))))))))


;; (defadvice scheme-send-last-sexp (after scheme-send-last-sexp-advice)
;;   "Print output in minibuf."
;;   (xwl-scheme-print-output))

;; (ad-activate 'scheme-send-last-sexp)


;; guile
(setq guile-program "guile")

;; guile debugger

;; (defadvice gds-help-symbol (after jump-to-help)
;;   (other-window 1))
;;   (less-minor-mode-on))

;; (ad-activate 'gds-help-symbol)

;; (require 'guile-scheme)
;; (setq initial-major-mode 'scheme-interaction-mode)

(global-set-key (kbd "C-c c s") 'run-scheme)


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
(load-file "/usr/share/emacs/site-lisp/sawfish/sawfish.el") ; oops
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

;; debian pkg manager
(autoload 'wajig "wajig"
  "Create a *wajig* buffer." t)
(global-set-key (kbd "<f10>") 'wajig)

(setq wajig-frequent-commands
      '("ps -ef" "ps -u william u"))

;; RSS: newsticker

;; (setq newsticker-url-list-defaults
;;       '(("douban-tokyo-love-story" "http://www.douban.com/feed/group/11197/discussion")
;; 	("newsmth-blog" "http://www.newsmth.com/pc/rssrec.php")))

;; (global-set-key (kbd "C-c m n") 'newsticker-show-news)

;; (add-hook 'newsticker-mode-hook 'less-minor-mode-on)

;;;; ERC
;; -----

;; ERC (M-x erc-select)
;; Set system locale to zh_CN.utf-8 first!
;; /me do sth; /ctcp xwl version
;; http://rafb.net/paste

;; irc.pchome.net:7000
;; 211.92.88.40:7000 #linuxfire
;; irc.debian.org #debian-zh

(require 'erc)

(setq erc-echo-notices-in-minibuffer-flag t
      erc-default-coding-system '(utf-8 . utf-8)
      erc-encoding-coding-alist nil
      erc-kill-buffer-on-part t
      erc-auto-query t)

(setq erc-server "irc.freenode.net"
      erc-port 7000
      erc-nick "xwl"
      erc-user-full-name "William Xu")

(setq erc-common-server-suffixes nil
      erc-mode-line-format "%t %a")

;; autojoin
(require 'erc-autojoin)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("rootdir.de"
	 "&bitlbee")
	("freenode.net"
	 "#emacs" "#guile" "#scheme")
        ("debian.org"
         "#debian-zh")))

;; match & track
(require 'erc-match)
(erc-match-mode 1)
(setq erc-current-nick-highlight-type 'nick-or-keyword)
(setq erc-keywords '("xwl" "emms"))
(setq erc-pals nil)

;; (global-set-key (kbd "C-c C-2") 'erc-track-switch-buffer)

(setq erc-track-faces-priority-list
      '(erc-current-nick-face
	erc-keyword-face
	erc-pal-face
	erc-default-face
	))
(setq erc-track-priority-faces-only 'all)

(defun xwl-toggle-erc-busy ()
  "Toggle `erc-default-face' in `erc-track-faces-priority-list'
so as to keep an eye on work when necessarily."
  (interactive)
  (if (memq 'erc-default-face erc-track-faces-priority-list)
      (progn
	(setq erc-track-faces-priority-list
	      (remove 'erc-default-face
		      erc-track-faces-priority-list))
	(message "Keep an eye on work"))
    (setq erc-track-faces-priority-list
	  (append erc-track-faces-priority-list
		  '(erc-default-face)))
    (message "Ah, time for tea")))

(global-set-key (kbd "C-c n e") 'xwl-toggle-erc-busy)

;; fill
(require 'erc-fill)
(erc-fill-mode 1)
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 10
      erc-fill-prefix "      ")

;; timestamp
(require 'erc-stamp)
(erc-timestamp-mode 1)
(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; spelling
(require 'erc-spelling)
(erc-spelling-mode 1)

;; ignore
(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT"))

;; ;; sound
;; (erc-sound-enable)
;; (setq erc-sound-path "/home/william/music/sound")
;; (setq erc-default-sound "/home/william/music/sound/reflection.mp4")
;; (setq erc-play-command "mplayer")

;; log
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t
      ;; erc-hide-timestamps t
      erc-log-insert-log-on-open nil
      erc-log-file-coding-system 'utf-8)

;; FIXME
;; bbdb
;; (require 'erc-bbdb)
;; (erc-bbdb-mode 1)
;; (setq erc-bbdb-popup-type nil)

;; goodies
(require 'erc-goodies)
(erc-readonly-mode 1)
(erc-smiley-mode 1)

(defun xwl-erc-select ()
  (interactive)
  (erc-select :server "irc.freenode.net"
	      :port 7000
	      :nick "xwl"
	      :password pwerc)

  (erc-select :server "irc.debian.org"
              :port 7000
              :nick "xwl"
              :password pwerc)

  (erc-select :server "im.rootdir.de"
	      :port 6668
	      :nick "xwl"
	      :password pwerc))

(global-set-key (kbd "C-c n E") 'xwl-erc-select)

(defun xwl-erc-cmd-WHOIS (nick)
  "Run /whois easily by key sequences."
  (interactive
   (list
    (ido-completing-read
     "/whois "
     (erc-get-channel-nickname-list))))
  (let ((inhibit-read-only t))
    (insert (concat "/whois " nick))
    (erc-send-current-line)))

(defun xwl-erc-mode-hook ()
  (auto-fill-mode -1)

  (define-key erc-mode-map (kbd "C-c C-w") 'xwl-erc-cmd-WHOIS))

(add-hook 'erc-mode-hook 'xwl-erc-mode-hook)

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

;; (setq emms-player-finished-hook
;;       '(emms-mode-line-blank
;;         emms-playing-time-stop
;;         emms-lyrics-stop
;;         my-emms-player-finished-hook))
;;;; trueice

;; interface
(setq trueice-playlist "~/music/misc/trueice-EDU"
      trueice-buffer-name "*trueice*"
      ;; directory for saving trueice songs
      trueice-directory "~/download/music/trueice"
      ;; files containing saved songs' list
      trueice-file "~/download/music/trueice/list"
      trueice-playing-p nil)

(defun trueice-playlist ()
  "Switch to `trueice-buffer-name'."
  (interactive)
  (if (get-buffer trueice-buffer-name)
      (switch-to-buffer trueice-buffer-name)
    (error "Trueice not started")))

(defun trueice-start ()
  "Start playing songs randomly from music.trueice.net."
  (interactive)
  (if trueice-playing-p
      (error "A trueice process already exists")
    (setq trueice-playing-p t)
    (shell trueice-buffer-name)
    (set-buffer-process-coding-system 'gbk 'gbk)
    (trueice-send
     (concat "mplayer -shuffle -playlist "
	     trueice-playlist
	     " 2>/dev/null | grep ^Playing"))
    (trueice-buffer-ensure
     (less-minor-mode-on))))

(defun trueice-next ()
  "Select next song."
  (interactive)
  (trueice-send ">")
  (sleep-for 0.5)
  (trueice-show))

(defun trueice-show ()
  "Show current playing."
  (interactive)
  (let ((song (trueice-current)))
    (if song
	(message song)
      (message "Nothing playing right now"))))

(defun trueice-save ()
  "TODO: Save current song to `trueice-directory'. At present we just
store into a file to download later."
  (interactive)
  (let ((song (trueice-current)))
    (if (not song)
	(message "Nothing playing right now")
      (let ((path
	      (replace-regexp-in-string
	       "^Playing \\|\\.$"
	       ""
	       song)))
	(with-current-buffer (find-file-noselect trueice-file)
	  (goto-char (point-max))
	  (let ((inhibit-read-only t))
	    (insert (concat path "\n")))
	  (save-buffer))))))

(defun trueice-stop ()
  "Stop trueice."
  (interactive)
  (trueice-send "q")
  (setq trueice-playing-p nil))

;; low level
(defmacro trueice-buffer-ensure (&rest body)
  "Make sure we are in `trueice-buffer-name' and at the bottom."
  `(with-current-buffer ,trueice-buffer-name
    (save-excursion
      (goto-char (point-max))
      ,@body)))

(defun trueice-current ()
  "Return current playing song."
  (trueice-buffer-ensure
   (if (re-search-backward "Playing.*")
       (match-string 0))))

(defun trueice-send (str)
  "Send STR to `trueice-buffer-name'."
  (trueice-buffer-ensure
   (let ((inhibit-read-only t))
     (insert str)
     (comint-send-input))))

;; bindings
(global-set-key (kbd "C-c t x") 'trueice-start)
(global-set-key (kbd "C-c t v") 'trueice-stop)
(global-set-key (kbd "C-c t n") 'trueice-next)
(global-set-key (kbd "C-c t o") 'trueice-show)
(global-set-key (kbd "C-c t s") 'trueice-save)
(global-set-key (kbd "C-c t l") 'trueice-playlist)

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

(add-hook 'w3m-mode-hook 'less-minor-mode-on)


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
         ("重要" "每天" "长期" "读书" "杂项" "工作" "TaskPool"))))
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

;; planner-browser
;; (require 'planner-browser)

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

;;;; outline

(require 'outline)

(defadvice outline-mode (after hide-sublevels)
  "Enter overview after start up `outline-mode'."
  (hide-sublevels 1))

(defadvice outline-minor-mode (after hide-sublevels)
  "Enter overview after start up `outline-minor-mode'."
  (hide-sublevels 2))

(ad-activate 'outline-mode)
;; (ad-activate 'outline-minor-mode)

(if (string= (substring emacs-version 0 2) "21")
    (setq outline-font-lock-keywords
	  '((eval list
		  (concat "^" outline-regexp ".+")
		  0
		  '(or (cdr (assq
			     (outline-font-lock-level)
			     '((1 . font-lock-function-name-face)
			       (2 . font-lock-variable-name-face)
			       (3 . font-lock-keyword-face)
			       (4 . font-lock-builtin-face)
			       (5 . font-lock-comment-face)
			       (6 . font-lock-constant-face)
			       (7 . font-lock-type-face)
			       (8 . font-lock-string-face))))
		       font-lock-warning-face)
		  'prepend t)))
  (setq outline-font-lock-keywords
	'((eval list
		(concat "^\\(?:" outline-regexp "\\).+")
		0
		'(outline-font-lock-face)
		nil t))))

;; outline extra
(require 'foldout)
(require 'out-xtra)

;; keys
(defun xwl-hide-body ()
  "Make `hide-body' take effects at any moment."
  (interactive)
  (show-all)
  (hide-body))

(defun xwl-outline-invisible-p ()
  "Are we inside a outline fold?"
  (interactive)
  (let ((overlays (overlays-at (line-end-position))))
    (and overlays
	 (eq (overlay-get (car overlays) 'invisible)
	     'outline))))

(defun xwl-foldout-exit-fold ()
  "Goto current folded line."
  (interactive)
  (call-interactively 'foldout-exit-fold) ; FIX ME
  (previous-line 1)
  (next-line 1))

(defun xwl-outline-toggle-enter-exit ()
  "Toggle entering and exiting fold."
  (interactive)
  (if (xwl-outline-invisible-p)
      (foldout-zoom-subtree)
    (xwl-foldout-exit-fold)))

(defun xwl-outline-toggle-show-hide ()
  "Toggle showing or hiding contents."
  (interactive)
  (if (xwl-outline-invisible-p)
      (show-subtree)
    (hide-subtree)))

(define-key outline-minor-mode-map (kbd "C-c C-i") 'hide-sublevels)
(define-key outline-minor-mode-map (kbd "C-c C-c") 'xwl-hide-body)
(define-key outline-minor-mode-map (kbd "C-c C-u") 'xwl-outline-toggle-enter-exit)
(define-key outline-minor-mode-map (kbd "C-c C-q") 'xwl-outline-toggle-show-hide)
(define-key outline-minor-mode-map (kbd "C-c C-n") (kbd "C-c @ C-n"))
(define-key outline-minor-mode-map (kbd "C-c C-p") (kbd "C-c @ C-p"))
(define-key outline-minor-mode-map (kbd "C-c C-a") (kbd "C-c @ C-a"))

(define-key outline-mode-map (kbd "C-c C-i") 'hide-sublevels)
(define-key outline-mode-map (kbd "C-c C-c") 'xwl-hide-body)
(define-key outline-mode-map (kbd "C-c C-u") 'xwl-outline-toggle-enter-exit)
(define-key outline-mode-map (kbd "C-c C-q") 'xwl-outline-toggle-show-hide)
(define-key outline-mode-map (kbd "C-c C-a") 'show-all)

;;;; org

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


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
         (signature xwl-fortune-signature))))

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
	     emms-patches@gnu.org
             ;;	     darcs-users@darcs.net

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

	  (to "matchsticker@newsmth.*" "newsmth")
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
      '((".*" . gb2312)))

(setq gnus-summary-show-article-charset-alist
      '((1 . utf-8)
        (2 . cn-gb2312)
	(3 . big5)))

(setq gnus-group-name-charset-method-alist
      '(((nntp "news.newsfan.net") . gb2312)))

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
          (concat "ip.scm " (match-string-no-properties 1)))
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


;;; XWIN

;;;; general

(when window-system
  ;; keys
  (global-set-key (kbd "M-1") 'delete-other-windows)
  (global-set-key (kbd "M-2") 'xwl-hide-buffer)
  (global-set-key (kbd "<H-s-tab>") 'lisp-complete-symbol)
  ;;(define-key TeX-mode-map (kbd "<H-s-tab>") 'TeX-complete-symbol)
  (global-set-key (kbd "<S-delete>")
		  '(lambda () (interactive) (delete-windows-on (buffer-name))))
  (global-set-key (kbd "<insert>") 'delete-other-windows)
  (global-set-key (kbd "M-<insert>") 'enlarge-window)
  (global-set-key (kbd "M-<delete>") 'shrink-window)
  (global-set-key (kbd "C--") 'undo)
  (global-set-key (kbd "C-x C--") 'redo)
  ;; (global-set-key (kbd "C-2") 'set-mark-command)
  ;; (global-set-key (kbd "C-x r C-2") 'rm-set-mark)
  ;; (global-set-key (kbd "<C-f1>") 'kill-this-buffer)

  ;; modes
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (mouse-wheel-mode 1)

  (setq woman-use-own-frame nil)
  (setq frame-title-format "the Church of Emacs"))
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
