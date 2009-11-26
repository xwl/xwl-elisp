;;; ga-yum.el --- Yum Backend (Redhat, Fedora)

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

;;; Code:

(require 'ga)

;; Variables
(defvar ga-yum-font-lock-keywords
  '(("^Name +:\\(.*\\)"
     (1 font-lock-function-name-face nil t))
    ("^\\([a-zA-Z0-9]+\\) *:"
     (1 font-lock-keyword-face t t))))

(defvar ga-yum-sources-file "/etc/yum.repos.d")

;; Interfaces
(defun ga-yum-update ()
  (ga-run-other-command (list "echo" "Running rpm -qa...")))

(defun ga-yum-search-by-name (pkg)
  (ga-yum-search pkg))

(defun ga-yum-search (pkg)
  (ga-run-command (list "search" pkg)))

(defun ga-yum-show (pkg)
  (ga-run-command (list "info" pkg)))

(defun ga-yum-install (pkg)
  (ga-run-command (list "-y" "install" pkg)))

(defun ga-yum-listfiles (pkg)
  (ga-run-other-command (list "rpm" "-ql" pkg)))

(defun ga-yum-upgrade (pkg)
  (ga-run-command (list "-y" "update" pkg)))

(defun ga-yum-clean ()
  (ga-run-command (list "clean" "all")))

(defun ga-yum-remove (pkg)
  (ga-run-command (list "-y" "erase" pkg)))

;; Misc

;; actually only installed
(defun ga-yum-update-available-pkgs ()
  (setq ga-available-pkgs
        (cons
         (list 'yum (split-string
                     (ga-run-other-command-to-string 
                      "rpm -qa | sed s/-[0-9].*//")))
         (remove-if (lambda (i) (eq (car i) 'yum)) ga-available-pkgs))))

(defun ga-yum-upgrade-all ()
  (ga-run-command (list "-y" "upgrade")))


(provide 'ga-yum)

;;; ga-yum.el ends here
