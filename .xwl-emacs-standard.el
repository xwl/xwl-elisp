;;; .xwl-emacs-standard.el --- stuffs depending only on standard features

;; Copyright (C) 2006 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1
;; Last updated: 2006/09/15 23:06:48

;; This file includes configs that depend only on Emacs' standard
;; features, thus could be used universally on different platforms.

;;; GENERAL

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

(defun xwl-list-ref (list ref)
  "Return the ref-th element of list."
  (if (= ref 0)
      (car list)
    (xwl-list-ref (cdr list) (1- ref))))

(defun xwl-info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))

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

(mouse-avoidance-mode 'animate)

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

(unless window-system
  (set-face-background 'highlight "red")
  (set-face-background 'show-paren-match-face "yellow"))

(setq visible-bell t)
;; (setq sentence-end "\\([]\\|\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
;;       sentence-end-double-space nil)
(setq sentence-end "\\([]\\|[.?!][]\"')}]*\\($\\| $\\|	\\|  \\)\\)[ 	\n]*")

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

;; date
;; ----

(defun xwl-insert-date ()
  (interactive)
  (insert (xwl-get-date)))

(defun xwl-get-date ()
  (format-time-string "%Y/%m/%d %H:%M:%S" (current-time)))

(defun xwl-update-date ()
  "Auto update '[Ll]ast [Uu]pdated:' part if exists when saving.
This should not affect `buffer-undo-list'."
  (interactive)
  (let ((old-list buffer-undo-list))
    (save-excursion
      (beginning-of-buffer)
      (when (search-forward-regexp "Last\\ updated:" nil t)
        (kill-line)
        (insert " ")
        (xwl-insert-date)))
    (setq buffer-undo-list old-list))
  nil)

(global-set-key (kbd "C-c m d") 'xwl-insert-date)

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


(setq default-major-mode 'text-mode)

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

;; use whitespaces instead of \t
(setq-default indent-tabs-mode nil)

;;zone
(setq zone-idle 300)

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


;;; DIRED

(require 'wdired)
(autoload 'wdired-change-to-wdired-mode "wdired")

(require 'dired-x)
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top)

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
)

(add-hook 'dired-mode-hook 'xwl-dired-mode-hook)


;;; BUFFERS

(setq xwl-hated-buffers
      '("*info*" "*Bookmark List*" "*scratch*" "*Compile-Log*"
	"*Completions*" "do" "*Occur*" ".diary" ".bbdb" "*desktop*"
	"*Playlist*" "*Messages*" "*compilation*" "*Ediff Registry*"
	"*Kill Ring*" "*Shell Command Output*" "*Holidays*"
	"*Help*" "*Async Shell Command*" ".newsrc-dribble"
	"*Calendar*" "*eshell*"))

;; ibuffer
(require 'ibuffer)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
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

(ido-everywhere 1)

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
(require 'saveplace)
(setq-default save-place t)

;; recentf
(require 'recentf)
(recentf-mode t)
(define-key recentf-dialog-mode-map (kbd "ESC TAB") 'widget-backward)
(define-key recentf-dialog-mode-map (kbd "n") 'widget-forward)
(define-key recentf-dialog-mode-map (kbd "p") 'widget-backward)

(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
	 (to-compl (mapcar (function
			    (lambda (x)
			      (cons (file-name-nondirectory x) x))) all-files))
	 (prompt (append '("File name: ") to-compl))
	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname to-compl)))))

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


;;; KEYS

;; keys' organization

;; C-c c *    invoke external programs, e.g., scheme, mysql.
;; C-c e *    emms
;; C-c m *    misc bindings

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

(global-set-key (kbd "C-x <left>") 'next-buffer)
(global-set-key (kbd "C-x <right>") 'previous-buffer)

(global-set-key (kbd "<prior>") 'scroll-other-window-down)
(global-set-key (kbd "<next>")  'scroll-other-window)
(global-set-key (kbd "<home>")  'xwl-scroll-other-window-down-one-line)
(global-set-key (kbd "<end>")   'xwl-scroll-other-window-up-one-line)
(global-set-key (kbd "C-x <up>")   'delete-other-windows)
(global-set-key (kbd "C-x <down>") 'xwl-hide-buffer)

(global-set-key (kbd "C-<left>")  'previous-error)
(global-set-key (kbd "C-<right>") 'next-error)

;; conflicts

(dolist (hook '(dired-mode-hook
                calendar-move-hook
                gnus-summary-mode-hook
                gnus-group-mode-hook
                clone-buffer-hook))
  (add-hook hook
            (lambda ()
		   (if (not (one-window-p))
		       (local-set-key (kbd "<end>")
                                      'xwl-scroll-other-window-up-one-line))
		   (local-set-key (kbd "M-n") 'xwl-scroll-up-one-line)
		   (local-set-key (kbd "M-p") 'xwl-scroll-down-one-line))))

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

(global-set-key (kbd "C-x v =") 'ediff-revision)
(global-set-key (kbd "C-c m D") 'toggle-debug-on-error)

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

(global-set-key (kbd "C-x r C-@") 'rm-set-mark)
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


;;; WINDOW

;; redo and undo
(winner-mode 1)

;; jump by name
;; (require 'winring)
;; (winring-initialize)

;; (setq winring-show-names nil)
;; (define-key winring-map "n" 'winring-next-configuration)
;; (define-key winring-map "o" 'winring-new-configuration)

;; jump by registers
;; C-x r w
;; C-x r j


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
      '(php-mode java-mode c-mode c++-mode emacs-lisp-mode scheme-mode
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

;; lisp

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


;;; INTEFACES

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


;;; HYPERMEDIA

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

;;; Misc


;;; .xwl-emacs-standard.el ends here
