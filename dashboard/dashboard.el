;;; dashboard.el --- Emacs dashboard !

;; Copyright (C) 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Dashboard implemented in elisp. FIXME: it seems rather difficult to
;; make widgets layout customizable by user. :(

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'dashboard "dashboard")

;;; Code:

;;; Customizations And Interfaces

(defgroup dashboard nil
  "Dashboard mode."
  :group 'dashboard)

(defcustom dashboard-mode-hook nil
  "Normal hook run after entering `dashboard-mode'."
  :type 'hook
  :group 'dashboard)

(defcustom dashboard-layout '()
  "TODO."
  :type 'hook
  :group 'dashboard)

;; ;; TODO
;; (setq dashboard-layout '(nba-standing nba-scoreboard))
;;                          ;;tenseijingo)

;;;###autoload
(defun dashboard ()
  "Create a *Dashboard* buffer."
  (interactive)
  (let ((dashboard-exist-p (get-buffer dashboard-buffer)))
    (switch-to-buffer dashboard-buffer)
    (unless dashboard-exist-p
      (dashboard-mode)
      (dashboard-update))))

(defun dashboard-update ()
  "Update all widgets."
  (interactive)
  ;; TODO, convert a user defined list to a sequence pair
  ;; Should run sequencely for the first time
 (dashboard-insert-by-column 'nba-standing 'nba-scoreboard)
  (dashboard-insert-by-row 'nba-standing 'tenseijingo)
  )


;;; dashboard-mode

(defvar dashboard-buffer "*dashboard*"
  "Name of the buffer used for the dashboard.")

(defvar dashboard-exist-p nil)

(defvar dashboard-base-point nil)
(defvar dashboard-insert-by-row-p nil)

(defvar dashboard-headline-face font-lock-keyword-face)
(defvar dashboard-widget-face font-lock-function-name-face)

(defvar dashboard-nba-8th-team-face font-lock-variable-name-face
  "Highlight 8th team.")

(defvar dashboard-nba-favorite-team-face font-lock-comment-face
  "Highlight my favorite team.")

(defvar dashboard-font-lock-keywords
  '(("\\(EASTERN\\|WESTERN\\).*STREAK"
     (0 dashboard-headline-face nil t))
    ("NBA Standing\\|NBA Scoreboard"
     (0 dashboard-widget-face nil t))
    ("Houston.*[LW]\\ [0-9]+\\|rockets"
     (0 dashboard-nba-favorite-team-face nil t))
    ("\\*.*\\*.*[LW]\\ [0-9]+"
     (0 dashboard-nba-8th-team-face t t)))
  "Keywords to highlight in dashboard mode.")

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'dashboard-update)
    map)
  "Keymap for `dashboard-mode'.")

(define-derived-mode dashboard-mode nil "Dashboard"
  "Emacs interface for dashboard.
\\{dashboard-mode-map}"
  (setq font-lock-defaults '(dashboard-font-lock-keywords))
  (setq dashboard-exist-p t)
  (setq buffer-read-only t)
  (setq dashboard-base-point nil)
  (run-hooks 'dashboard-mode-hook))

(defun dashboard-padding (s n)
  "Padding spaces equally around string S to be N bytes long.
If we can't padding equally, place the extra space at the end of S."
  (let ((len (length s))
        (ret s))
    (when (> n len)
      (let ((spaces (make-string (/ (- n len) 2) ? )))
        (setq ret (concat spaces ret spaces))
        (when (< (length ret) n)
          (setq ret (concat ret " ")))))
    ret))

(defun dashboard-insert-as-rectangle (widget s)
  "Insert WIDGET with content string S as rectangle.
The starting point is at `dashboard-base-point'. WIDGET is a symbol."
  (let ((rect "")
        (width 0))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (move-end-of-line 1)
      (while (not (eobp))
        (when (> (current-column) width)
          (setq width (current-column)))
        (forward-line 1)
        (move-end-of-line 1))
      (goto-char (point-max))
      (when (> width (current-column))
        (insert (make-string (- width (current-column)) ? ))
        (insert " "))       ; an extra space indicating rectangle corner
      (setq rect (extract-rectangle (point-min) (point-max))))
    (with-current-buffer dashboard-buffer
      (let* ((inhibit-read-only t)
             (upper-left (intern (format "dashboard-%s-upper-left-marker" widget)))
             (upper-left-point (marker-position (symbol-value upper-left)))
             (upper-right (intern (format "dashboard-%s-upper-right-marker" widget)))
             (upper-right-point (marker-position (symbol-value upper-right)))
             (lower-left (intern (format "dashboard-%s-lower-left-marker" widget)))
             (lower-left-point (marker-position (symbol-value lower-left)))
             (lower-right (intern (format "dashboard-%s-lower-right-marker" widget)))
             (lower-right-point (marker-position (symbol-value lower-right))))
        (save-excursion
          (if upper-left-point
              (delete-rectangle (1+ upper-left-point) (1- lower-right-point))
            (if dashboard-base-point
                (goto-char dashboard-base-point)
              (goto-char (point-min)))
            (when dashboard-insert-by-row-p
              (let ((col (current-column))
                    (next-line-add-newlines t))
                ;; FIXME, if we are not at (eolp), (forward-line 1)
                ;; returns 0? (suppose we are actually at last line).
                (next-line 1)
                (when (< (current-column) col)
                  (insert (make-string (- col (current-column)) ? )))))
            (set upper-left (point-marker))
            (setq upper-left-point (marker-position (symbol-value upper-left)))
            (insert "  ")
            (set upper-right (point-marker))
            (setq upper-right-point (marker-position (symbol-value upper-right))))
          (goto-char (1+ upper-left-point))
          (insert-rectangle rect)
          (unless lower-left-point
            (set lower-right (point-marker))
            (set lower-left (progn (goto-char (- (point) width 1))
                                   (point-marker)))))))))

(defun dashboard-insert-by-column (w1 w2)
  (let* ((w1-update (intern (format "dashboard-%s" w1)))
         (w2-update (intern (format "dashboard-%s" w2)))
         (base (intern (format "dashboard-%s-upper-right-marker" w1))))
    (funcall w1-update)
    (while (not (marker-position (symbol-value base)))
      (sleep-for 1))
    (setq dashboard-base-point (marker-position (symbol-value base)))
    (setq dashboard-insert-by-row-p nil)
    (funcall w2-update)))

(defun dashboard-insert-by-row (w1 w2)
  (let* ((w1-update (intern (format "dashboard-%s" w1)))
         (w2-update (intern (format "dashboard-%s" w2)))
         (base (intern (format "dashboard-%s-lower-left-marker" w1))))
    (funcall w1-update)
    (while (not (marker-position (symbol-value base)))
      (sleep-for 1))
    (setq dashboard-base-point (marker-position (symbol-value base)))
    (setq dashboard-insert-by-row-p t)
    (funcall w2-update)))

(defun dashboard-decode-html ()
  "Decode html buffer retrieved by `url-retrieve'."
  (let ((coding 'utf-8))
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward "charset=[[:blank:]]*\\([a-zA-Z_-]+\\)" nil t 1))
      (setq coding (intern (downcase (match-string 1)))))
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) coding)))


;;; Sample Widgets: nba-standing, nba-scoreboard

(defvar dashboard-nba-standing-upper-left-marker (make-marker))
(defvar dashboard-nba-standing-upper-right-marker (make-marker))
(defvar dashboard-nba-standing-lower-left-marker (make-marker))
(defvar dashboard-nba-standing-lower-right-marker (make-marker))

(defun dashboard-nba-standing ()
  "Get nba standing from www.nba.com."
  (let ((url "http://www.nba.com/standings/team_record_comparison/conferenceNew_Std_Cnf.html"))
    (if (marker-position dashboard-nba-standing-upper-left-marker)
        (url-retrieve url 'dashboard-nba-standing-callback)
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall 'dashboard-nba-standing-callback)))))

(defun dashboard-nba-standing-callback (&optional status)
  (when (eq :error (car status))
    (error "dashboard-nba-standing-callback: %S" status))
  (let ((eastern '(("EASTERN" "NICK" "W" "L" "PCT" "GB" "CONF" "DIV" "HOME" "ROAD" "L 10" "STREAK")))
        (western '(("WESTERN" "NICK" "W" "L" "PCT" "GB" "CONF" "DIV" "HOME" "ROAD" "L 10" "STREAK"))))
    (goto-char (point-min))
    (when (re-search-forward "<tr><td colspan=15 class=\"confTitle\">Eastern Conference</td></tr>" nil t 1)
      (dotimes (i 15)                   ; team numbers
        (re-search-forward "<td class=\"team\"><a href=\"/\\(.*\\)/\">\\(.*\\)</a></td> ")
        (let ((team (list (if (= i 7)   ; highlight the 8-th team
                              (concat "*" (match-string 2) "*")
                            (match-string 2))
                          (match-string 1))))
          (dotimes (j 10)               ; columns
            (re-search-forward "<td>\\(.*\\)</td>" nil 1)
            (setq team (append team (list (replace-regexp-in-string
                                           "\ +$" "" (match-string 1))))))
          (setq eastern (append eastern (list team))))))
    (when (re-search-forward "<tr><td colspan=15 class=\"confTitle\">Western Conference</td></tr>" nil t 1)
      (dotimes (i 15)                   ; team numbers
        (re-search-forward "<td class=\"team\"><a href=\"/\\(.*\\)/\">\\(.*\\)</a></td> ")
        (let ((team (list (if (= i 7)   ; highlight the 8-th team
                              (concat "*" (match-string 2) "*")
                            (match-string 2))
                          (match-string 1))))
          (dotimes (j 10)               ; columns
            (re-search-forward "<td>\\(.*\\)</td>" nil 1)
            (setq team (append team (list (replace-regexp-in-string
                                           "\ +$" "" (match-string 1))))))
          (setq western (append western (list team))))))
    (kill-buffer (current-buffer))
    (dashboard-nba-standing-format eastern western)))

(defun dashboard-nba-standing-format (eastern western)
  "Called by `dashboard-nba-standing-callback'.
EASTERN and WESTERN are lists in the form of:

    '((\"EASTERN\" \"NICK\" \"W\" \"L\" \"PCT\" \"GB\" \"CONF\" \"DIV\" \"HOME\" \"ROAD\" \"L 10\" \"STREAK\"))"
  (with-temp-buffer
    (let ((width 0))
      (dolist (i eastern)
        (let ((line (format "%-15s%-15s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s"
                            (nth 0 i) (nth 1 i) (nth 2 i) (nth 3 i) (nth 4 i)
                            (nth 5 i) (nth 6 i) (nth 7 i) (nth 8 i) (nth 9 i)
                            (nth 10 i) (nth 11 i))))
          (when (> (length line) width)
            (setq width (length line)))
          (insert (concat line "\n"))))
      (insert (concat (make-string width ?-) "\n"))
      (dolist (i western)
        (insert (format "%-15s%-15s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s\n"
                        (nth 0 i) (nth 1 i) (nth 2 i) (nth 3 i) (nth 4 i)
                        (nth 5 i) (nth 6 i) (nth 7 i) (nth 8 i) (nth 9 i)
                        (nth 10 i) (nth 11 i))))
      (insert "\n")
      (let ((title (format-time-string "NBA Standing  %Y/%m/%d %H:%M:%S"
                                       (current-time))))
        (insert (dashboard-padding title width)))
      (dashboard-insert-as-rectangle 'nba-standing (buffer-string)))))

(defvar dashboard-nba-scoreboard-upper-left-marker (make-marker))
(defvar dashboard-nba-scoreboard-upper-right-marker (make-marker))
(defvar dashboard-nba-scoreboard-lower-left-marker (make-marker))
(defvar dashboard-nba-scoreboard-lower-right-marker (make-marker))

(defun dashboard-nba-scoreboard ()
  "Get nba standing from www.nba.com."
  (let ((url (format-time-string
              "http://www.nba.com/games/%Y%m%d/scoreboard.html"
              (time-subtract (current-time) (seconds-to-time 86400))))) ; tokyo time -> us time
    (if (marker-position dashboard-nba-scoreboard-upper-left-marker)
        (url-retrieve url 'dashboard-nba-scoreboard-callback)
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall 'dashboard-nba-scoreboard-callback)))))

(defun dashboard-nba-scoreboard-callback (&optional status)
  (when (eq :error (car status))
    (error "dashboard-nba-scoreboard-callback: %S" status))
  (let ((score '()))    ; '(visit-team visit-score home-team home-score)
    (goto-char (point-min))
    (while (re-search-forward "<div class=\"scoreBoardGame\">" nil t 1)
      (let ((visit-team "")
            (visit-score "")
            (home-team "")
            (home-score ""))
        (re-search-forward ".*visteam.*>\\([a-z]+\\)<" nil t 1)
        (setq visit-team (match-string 1))
        (when (re-search-forward ">\\([0-9]+\\)<" nil t 1)
          (setq visit-score (match-string 1)))
        (re-search-forward ".*hometeam.*>\\([a-z]+\\)<" nil t 1)
        (setq home-team (match-string 1))
        (when (re-search-forward ">\\([0-9]+\\)<" nil t 1)
          (setq home-score (match-string 1)))
        (setq score (cons (list visit-team visit-score home-team home-score)
                          score))))
    (kill-buffer (current-buffer))
    (dashboard-nba-scoreboard-format score)))

(defun dashboard-nba-scoreboard-format (score)
  "SCORE is a list of the form:

    '((visit-team visit-score home-team home-score))."
  (with-temp-buffer
    (let ((width 0))
      (dolist (i (nreverse score))
        (let ((line (format "%-18s%s"
                            (concat (nth 0 i) (if (nth 1 i)
                                                  (concat "(" (nth 1 i) ")" )
                                                ""))
                            (concat (nth 2 i) (if (nth 3 i)
                                                  (concat "(" (nth 3 i) ")" )
                                                "")))))
          (when (> (length line) width)
            (setq width (length line)))
          (insert (concat line "\n"))))
      (insert "\n")
      (insert
       (dashboard-padding
        (format-time-string "NBA Scoreboard  %Y/%m/%d %H:%M:%S" (current-time))
        width))
      (dashboard-insert-as-rectangle 'nba-scoreboard (buffer-string)))))

(provide 'dashboard)

;;; dashboard.el ends here
