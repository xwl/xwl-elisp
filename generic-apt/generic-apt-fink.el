;;; generic-apt-fink.el --- Fink Backend (Mac OS X)

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

(require 'generic-apt)

;;; Required Interfaces

(defun generic-apt-fink-edit-sources ()
  '"/sw/etc/fink.conf")

(defun generic-apt-fink-search (pkg)
  (generic-apt-run-command (list "list" pkg)))

(defun generic-apt-fink-update ()
  (generic-apt-run-command (list "selfupdate")))

(defun generic-apt-fink-install (pkg)
  (generic-apt-run-command (list "install" pkg)))

(defun generic-apt-fink-upgrade (pkg)
  (generic-apt-run-command (list "update" pkg)))

(defun generic-apt-fink-remove (pkg)
  (generic-apt-run-command (list "remove" pkg)))

(defun generic-apt-fink-show (pkg)
  (generic-apt-run-command (list "describe" pkg)))

(defun generic-apt-fink-update-available-pkgs ()
  (setq generic-apt-available-pkgs
        (split-string
         (generic-apt-run-command-to-string
          ;; FIXME: Why doesn't "sed 's/.\{4\}'"  work?
          "list | sed 's/....//' | awk '{print $1}'"))))

(provide 'generic-apt-fink)

;;; generic-apt-fink.el ends here
