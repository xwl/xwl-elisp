;;; ga-fink.el --- Fink Backend (Mac OS X)

;; Copyright (C) 2008, 2009 William Xu

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

;;; Code:

(require 'ga)

;; Variables
(defvar ga-fink-font-lock-keywords
  '(("^\n\\([a-zA-Z0-9].*: \\)\\(.*\\)"
     (1 font-lock-keyword-face nil t)
     (2 font-lock-function-name-face nil t))
    ("Web site:\\|Maintainer:"
     (0 font-lock-keyword-face t t))))

(defvar ga-fink-sources-file "/sw/etc/fink.conf")

;; Interfaces
(defun ga-fink-update ()
  (ga-run-command (list "selfupdate")))

(defun ga-fink-search-by-name (pkg)
  (ga-run-command (list "list" pkg)))

(defun ga-fink-search (pkg)
  (ga-run-command (list "apropos" pkg)))

(defun ga-fink-show (pkg)
  (ga-run-command (list "describe" pkg)))

(defun ga-fink-install (pkg)
  (ga-run-command (list "--yes" "install" pkg)))

(defun ga-fink-listfiles (pkg)
  (ga-run-other-command (list "dpkg" "--listfiles" pkg)))

(defun ga-fink-upgrade (pkg)
  (ga-run-command (list "--yes" "update" pkg)))

(defun ga-clean ()
  (ga-run-command (list "cleanup")))

(defun ga-fink-remove (pkg)
  (ga-run-command (list "--yes" "remove" pkg)))

;; Misc

(defun ga-fink-update-available-pkgs ()
  (setq ga-available-pkgs
        (cons
         (list 'fink 
               (split-string
                (ga-run-command-to-string
                 ;; FIXME: Why doesn't "sed 's/.\{4\}'"  work?
                 "list | sed 's/....//' | awk '{print $1}'")))
         (remove-if (lambda (i) (eq (car i) 'fink))
                    ga-available-pkgs))))

;; (defun ga-fink-upgrade-all ()
;;   (ga-run-command (list "update-all")))


(provide 'ga-fink)

;;; ga-fink.el ends here
