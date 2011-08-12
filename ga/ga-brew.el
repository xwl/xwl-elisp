;;; ga-brew.el --- Homebrew backend (Mac OS X)

;; Copyright (C) 2011 William Xu

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
(defvar ga-brew-font-lock-keywords nil)
(defvar ga-brew-sources-file nil)

;; Interfaces
(defun ga-brew-update ()
  (ga-run-command '("update")))

(defun ga-brew-search-by-name (pkg)
  (ga-run-command `("search" ,pkg)))

(defalias 'ga-brew-search 'ga-brew-search-by-name)

(defun ga-brew-show (pkg)
  (ga-run-command `("info" ,pkg)))

(defun ga-brew-install (pkg)
  (ga-run-command `("install" ,pkg)))

(defun ga-brew-listfiles (pkg)
  (ga-run-command `("list" ,pkg)))

(defun ga-brew-upgrade (pkg)
  (ga-run-command `("install" "--force" "--HEAD" ,pkg)))

(defun ga-brew-clean ()
  (ga-run-command `("cleanup")))

(defun ga-brew-remove (pkg)
  (ga-run-command `("remove" ,pkg)))

;; Misc

(defun ga-brew-update-available-pkgs ()
  (setq ga-available-pkgs
        `((brew ,(split-string
                  (ga-run-command-to-string "search")))
          ,@(remove-if (lambda (i) (eq (car i) 'brew))
                       ga-available-pkgs))))

(provide 'ga-brew)

;;; ga-brew.el ends here
