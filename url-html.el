;;; url-html.el --- Tools for dealing with html pages

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

;;; Code:

;;;###autoload
(defun url-html-decode-buffer (&optional buffer)
  "Decode html BUFFER(default is current buffer).
Usually used in buffer retrieved by `url-retrieve'. If no charset info
is specified in html tag, default is 'utf-8."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((coding 'utf-8))
      ;; (switch-to-buffer buffer)
      (when (save-excursion
              (goto-char (point-min))
              (re-search-forward "<meta http-equiv.*charset=[[:blank:]]*\\([a-zA-Z0-9_-]+\\)" nil t 1))
        (setq coding (intern (downcase (match-string 1)))))
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) coding))))


(provide 'url-html)

;;; url-html.el ends here
