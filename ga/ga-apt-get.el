;;; ga-apt-get.el --- apt-get backend (Debian GNU/Linux)

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

;;; Commentary:

;; sample /etc/apt/apt.conf setup:

;;    APT {
;;      Get {
;;        Assume-Yes "true";
;;        Fix-Broken "true";
;;      };
;;    };

;;; Code:

(require 'ga)

;; Variables
(defvar ga-apt-get-font-lock-keywords
  `(("^Package:\\(.*\\)"
     (1 font-lock-function-name-face nil t))
    ("^Conflicts:"
     (0 font-lock-warning-face nil t))
    ("^Description:\\(.*\n\\)"
     (1 font-lock-function-name-face nil t))
    (,(concat
       "^\\("
       (regexp-opt
	'("Package" "Priority" "Section" "Installed-Size" "Maintainer"
	  "Architecture" "Version" "Depends" "Suggests" "Filename"
	  "Size" "MD5sum" "Description" "Tag" "Status" "Replaces"
	  "Conffiles" "Source" "Provides" "Pre-Depends" "Recommends"
          "SHA1" "SHA256" "Enhances" "Config-Version" "Task"))
       "\\):")
     (0 font-lock-keyword-face t t))))

(defvar ga-apt-get-sources-file "/etc/apt/sources.list")

;; Interfaces
(defun ga-apt-get-update ()
  (ga-run-command (list "update")))

(defun ga-apt-get-search-by-name (pkg)
  (ga-run-other-command (list "apt-cache" "search" "-n" pkg)))

(defun ga-apt-get-search (pkg)
  (ga-run-other-command (list "apt-cache" "search" pkg)))

(defun ga-apt-get-show (pkg)
  (ga-run-other-command (list "apt-cache" "show" pkg)))

(defun ga-apt-get-install (pkg)
  (ga-run-command (list "install" pkg)))

(defun ga-apt-get-listfiles (pkg)
  (ga-run-other-command (list "dpkg" "-L" pkg)))

(defun ga-apt-get-upgrade (pkg)
  (ga-run-command (list "upgrade" pkg)))

(defun ga-apt-get-clean ()
  (ga-run-other-command (list "apt-cache" "clean")))

(defun ga-apt-get-remove (pkg)
  (ga-run-command (list "remove" pkg)))

;; Misc

(defun ga-apt-get-update-available-pkgs ()
  (setq ga-available-pkgs
        (cons
         (list 'apt-get 
               (split-string
                (ga-run-other-command-to-string "apt-cache pkgnames")))
         (remove-if (lambda (i) (eq (car i) 'apt-get))
                    ga-available-pkgs))))

(provide 'ga-apt-get)

;;; ga-apt-get.el ends here
