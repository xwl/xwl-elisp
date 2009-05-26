;;; ga-pkgsrc.el --- pkgsrc backend (*nix)

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
(require 'cl)

(defcustom ga-pkgsrc-dir ""
  "pkgsrc source directory."
  :type 'string
  :group 'ga)

;; Variables
(defvar ga-pkgsrc-font-lock-keywords
  `((,(concat (regexp-opt '("Comment" "Requires" "Description" "Homepage"))
              ":")
     (0 font-lock-keyword-face nil t))
    ("Information for \\(.+\\):"
     (1 font-lock-function-name-face nil t))))

;; FIXME: is this the right place?
(defvar ga-pkgsrc-sources-file "/usr/pkg/etc/mk.conf")

(defvar ga-pkgsrc-make
  (if (eq system-type 'darwin)
      "bmake"
    "make"))

;; Interfaces

(defun ga-pkgsrc-update ()
  (let ((default-directory (concat ga-pkgsrc-dir "/")))
    (ga-run-other-command (list "cvs" "update" "-dP"))))

;; (defun ga-pkgsrc-search-by-name (pkg)
;;   (ga-run-command (list "list" pkg)))

;; (defun ga-pkgsrc-search (pkg)
;;   (ga-run-command (list "apropos" pkg)))

(defun ga-pkgsrc-show (pkg)
  (let ((cmd
         (if (ga-pkgsrc-binary-exists-p pkg)
             (concat "pkg_info "  pkg "-*")
           (concat "cat " (concat (ga-pkgsrc-pkg-dir pkg) "/DESCR")))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert 
       ;; Only show filename in output.
       (let ((default-directory (concat ga-pkgsrc-dir "/packages/All/")))
         (shell-command-to-string cmd)))
      (ga-insert-end-string))))

(defun ga-pkgsrc-install (pkg)
  (let ((default-directory (concat (ga-pkgsrc-pkg-dir pkg) "/")))
    (ga-run-other-command (list ga-pkgsrc-make "bin-install"))))

(defun ga-pkgsrc-listfiles (pkg)
  (ga-run-other-command (list "pkg_info" "-L" pkg)))

;; (defun ga-pkgsrc-upgrade (pkg)
;;   (ga-run-command (list "--yes" "update" pkg)))

;; (defun ga-clean ()
;;   (ga-run-command (list "cleanup")))

(defun ga-pkgsrc-remove (pkg)
  (ga-run-command (list "pkg_delete" pkg)))

;; Misc

(defun ga-pkgsrc-update-available-pkgs ()
  (setq ga-cache-list
        (cons
         (list 'pkgsrc 
               (split-string
                (let ((default-directory (concat ga-pkgsrc-dir "/")))
                  (shell-command-to-string 
                   "find . -type d -depth 2 | grep -v '/CVS' | cut -d / -f 2-"
                   ))))
         (remove-if (lambda (i) (eq (car i) 'pkgsrc))
                    ga-cache-list)
         ))

  (setq ga-available-pkgs
        (cons
         (list 'pkgsrc 
               (mapcar (lambda (i) (file-name-nondirectory i))
                       (cadr (assoc 'pkgsrc ga-cache-list))))
         (remove-if (lambda (i) (eq (car i) 'pkgsrc))
                    ga-available-pkgs))))

(defun ga-pkgsrc-binary-exists-p (pkg)
  (let ((pkgs
         (shell-command-to-string (concat "pkg_info  | cut -f 1 -d - | xargs echo -n")))
        (ret nil))
    (setq ret (string-match (concat "\\<" pkg "\\>") pkgs))
    (unless ret
      (setq pkgs
            (shell-command-to-string
             (concat "ls -1 " ga-pkgsrc-dir "/packages/All | cut -f 1 -d - |  xargs echo -n")))
      (setq ret (string-match (concat "\\<" pkg "\\>") pkgs)))
    ret))

(defun ga-pkgsrc-pkg-dir (pkg)
  "Source directory for PKG."
  (let ((pkg-dir-list (cadr (assoc 'pkgsrc ga-cache-list)))
        d ret)
    (while pkg-dir-list
      (setq d (car pkg-dir-list)
            pkg-dir-list (cdr pkg-dir-list))
      (when (string= (file-name-nondirectory d) pkg)
        (setq ret d)
        (setq pkg-dir-list nil)))
    (concat ga-pkgsrc-dir "/" ret)))

(defun ga-pkgsrc-jump (pkg)
  "Jump to corresponding dired buffer for PKG config directory."
  (interactive
   (list
    (ido-completing-read "Jump to: " 
                         (cadr (assoc ga-backend ga-available-pkgs)))))
  (dired (ga-pkgsrc-pkg-dir pkg)))

(add-hook 'ga-pkgsrc-hook (lambda ()
                            (local-set-key (kbd "C-x C-j") 'ga-pkgsrc-jump)))

(provide 'ga-pkgsrc)

;;; ga-pkgsrc.el ends here
