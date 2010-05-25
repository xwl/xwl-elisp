;;; url-extra.el --- Extra url http/html related utilities

;; Copyright (C) 2008, 2009, 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.2

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

(defun url-extra-html-decode-buffer (&optional buffer)
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

(defun url-extra-http-encode-string (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		       (format "%%%02x" c)))
		 (encode-coding-string str content-type))))

(defun url-extra-http-encode-string-without-escape (str content-type)
  "Similar to `url-extra-http-encode-string' but treat \"\\\" as regular
character."
  (let ((back-slash (format "%%%02x" ?\\)))
    (replace-regexp-in-string
     back-slash
     (concat back-slash back-slash)
     (url-extra-http-encode-string str content-type))))

(defun url-extra-http-post (url data &optional charset)
  "Retrieve URL synchronously with `url-retrieve-synchronously'.

DATA is an alist, e.g., '((field-name . \"value\")).
CHARSET defaults to 'utf-8."
  (or charset (setq charset 'utf-8))
  (let ((url-request-method "POST")
        (url-request-data
         (mapconcat
          'identity
          (mapcar (lambda (field)
                    (concat (symbol-name (car field))
                            "="
                            (url-extra-http-encode-string
                             (cdr field) charset)))
                  data)
          "&"))
        (url-mime-charset-string (symbol-name charset))
        (url-request-extra-headers
         `(("Content-Type" . ,(concat
                               "application/x-www-form-urlencoded;charset="
                               (symbol-name charset))))))
    (url-retrieve-synchronously url)))


(provide 'url-extra)
;;; url-extra.el ends here
