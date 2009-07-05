;;; ga-apt-cyg.el --- Cygwin Backend (Windows XP)

;; Copyright (C) 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary

;; This relies on apt-cyg from:
;;   http://code.google.com/p/apt-cyg/

;;; Code:

(require 'ga)

;; Variables
(defvar ga-apt-cyg-font-lock-keywords
  `((,(concat (regexp-opt '("sdesc" "ldesc" "category" "requires" "version"
                            "install" "source" "install"))
              ":")
     (0 font-lock-keyword-face nil t))
    ("@ \\(.+\\)"
     (1 font-lock-function-name-face nil t))))

(defcustom ga-apt-cyg-sources-file "c:/cygwin/etc/setup"
  "Cygwin setup directory."
  :type 'string
  :group 'ga)

;; Interfaces
(defun ga-apt-cyg-update ()
  (ga-run-command (list "update")))

(defun ga-apt-cyg-search (pkg)
  (ga-run-command (list "find" pkg)))

(defun ga-apt-cyg-show (pkg)
  (ga-run-command (list "describe" pkg)))

(defun ga-apt-cyg-install (pkg)
  (ga-run-command (list "install" pkg)))

;; (defun ga-apt-cyg-listfiles (pkg)
;;   (ga-run-other-command (list "dpkg" "--listfiles" pkg)))

;; (defun ga-apt-cyg-upgrade (pkg)
;;   (ga-run-other-command (list "--yes" "update" pkg)))

;; (defun ga-clean ()
;;   (ga-run-other-command (list "cleanup")))

(defun ga-apt-cyg-remove (pkg)
  (ga-run-command (list "remove" pkg)))

;; Misc

(defun ga-apt-cyg-update-available-pkgs ()
  (setq ga-available-pkgs
        (cons
         (list 'apt-cyg
               (split-string
                ;; FIXME: Doesn't work when calling with start-process? 
                ;; (ga-run-other-command-to-string
                (shell-command-to-string
                 (format "grep @ '%s' | sed 's/@ //'" (ga-apt-cyg-get-setup-path)))))
         (remove-if (lambda (i) (eq (car i) 'apt-cyg))
                    ga-available-pkgs))))

(defun ga-apt-cyg-get-setup-path ()
  "Full path for setup.ini."
  (let (last-cache last-mirror ret)
    (with-temp-buffer 
      ;; last-cache
      (insert-file-contents (concat ga-apt-cyg-sources-file "/last-cache"))
      (setq last-cache
            (replace-regexp-in-string
             "\n" "" (buffer-substring-no-properties (point-min) (point-max))))

      (erase-buffer)

      ;; last-mirror
      (insert-file-contents (concat ga-apt-cyg-sources-file "/last-mirror"))
      (setq last-mirror 
            (replace-regexp-in-string
             "\n" "" (buffer-substring-no-properties (point-min) (point-max))))

      ;; cache-and-mirror
      (setq ret
            (concat last-cache
                    "/"
                    (replace-regexp-in-string 
                     "/" "%2f"
                     (replace-regexp-in-string ":" "%3a" last-mirror))
                    "/setup.ini")))
    ret))


(provide 'ga-apt-cyg)

;;; ga-apt-cyg.el ends here
