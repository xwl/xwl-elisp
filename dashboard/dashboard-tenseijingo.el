;;; dashboard-tenseijingo.el --- tenseijingo colum from www.asahi.com

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

;; A dashboard widget. It will grab tenseijingo column from
;; www.asahi.com, so that one can read them happily inside Emacs
;; dashboard.

;;; Code:

(require 'dashboard)

(defvar dashboard-tenseijingo-upper-left-marker (make-marker))
(defvar dashboard-tenseijingo-upper-right-marker (make-marker))
(defvar dashboard-tenseijingo-lower-left-marker (make-marker))
(defvar dashboard-tenseijingo-lower-right-marker (make-marker))

(defun dashboard-tenseijingo ()
  "Get tenseijingo colum from http://www.asahi.com."
  (let ((url "http://www.asahi.com/paper/column.html"))
    (if (marker-position dashboard-tenseijingo-upper-left-marker)
        (url-retrieve url 'dashboard-tenseijingo-callback)
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall 'dashboard-tenseijingo-callback)))))

(defun dashboard-tenseijingo-callback (&optional status)
  (when (eq :error (car status))
    (error "dashboard-tenseijingo-callback: %S" status))
  (dashboard-decode-html)
  ;; (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let ((date "")
        (content ""))
    (when (search-forward "<h1 id=\"genre\">天声人語</h1>" nil t 1)
      (re-search-forward "<p id=\"date\">\\(.*\\)</p>" nil t 1)
      (setq date (match-string 1))
      (search-forward "<div class=\"kiji\">" nil t 1)
      (search-forward "<p><p>" nil t 1)
      (skip-chars-forward "[[:blank:]\n]")
      (let ((start (point)))
        (search-forward "</p>" nil t 1)
        (backward-char (1+ (length "</p>")))
        (setq content (buffer-substring-no-properties start (point))))
      (kill-buffer (current-buffer))
      (dashboard-tenseijingo-format content date))))

(defun dashboard-tenseijingo-format (content date)
  "Called by `dashboard-tenseijingo-callback'."
  (with-temp-buffer
    (let ((width 0))
      (insert "\n")
      (insert content)
      (fill-region (point-min) (point-max))
      (goto-char (point-max))
      (setq width (current-column))
      (insert "\n\n")
      (let ((title (concat "天声人語 " date)))
        (insert (dashboard-padding title width)))
      (dashboard-insert-as-rectangle 'tenseijingo (buffer-string)))))


;;; Setup

;; FIXME, this does not work.
(setq dashboard-font-lock-keywords
      (cons '("天声人語" (0 dashboard-widget-face nil t))
            dashboard-font-lock-keywords))

(provide 'dashboard-tenseijingo)

;;; dashboard-tenseijingo.el ends here
