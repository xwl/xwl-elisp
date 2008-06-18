;;; finkinfo-mode.el --- Major mode for editing fink info files

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

;;; Commentary:

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'finkinfo-mode "finkinfo-mode")

;;; Code:

(defvar finkinfo-keywords
  '("Package" "Version" "Revision" "Description" "License"
    "Maintainer" "Architecture" "BuildDepends" "Depends"
    "Recommends" "Source" "Source-MD5" "NoSetCPPFLAGS"
    "SetCPPFLAGS" "NoSetLDFLAGS" "SetLIBRARY_PATH"
    "CompileScript" "InstallScript" "Shlibs" "DocFiles" "DescPort"
    "DescDetail" "DescUsage" "Homepage" "SplitOff"
    "BuildDependsOnly" "Files" "Distribution" "Type" "Epoch"
    "Provides" "Conflicts" "BuildConflicts" "Replaces" "Suggests"
    "Enhances" "Pre-Depends" "Essential" "CustomMirror"
    "SourceDirectory" "NoSourceDirectory" "SourceRename"
    "TarFilesRename" "UpdateConfigGuess" "UpdateConfigGuessInDirs"
    "UpdateLibtool" "UpdateLibtoolInDirs" "UpdatePoMakefile" "Patch"
    "PatchFile" "PatchFile-MD5" "PatchScript" "SetENVVAR" "NoSetENVVAR"
    "ConfigureParams" "GCC" "NoPerlTests" "InfoTest" "UpdatePOD"
    "AppBundles" "JarFiles" "RuntimeVars" "PreInstScript"
    "PostInstScript" "PreRmScript" "PostRmScript" "ConfFiles" "InfoDocs"
    "DaemonicFile" "DaemonicName" "DescPackaging"))

(defvar finkinfo-font-lock-keywords
  `((,(format "\\(%s\\|%s\\)\\(:\\) \\(.*\\)"
              (regexp-opt finkinfo-keywords)
              (mapconcat 'identity
                         '("Info[2-9]" "Source[2-9]"
                           "Source[2-9]ExtractDir" "Source[2-9]Rename"
                           "Source[2-9]-MD5" "Tar[2-9]FilesRename"
                           "SplitOff[2-9]")
                         "\\|"))
     (1 font-lock-keyword-face nil t)
     (2 font-lock-keyword-face nil t)
     (3 font-lock-function-name-face nil t))
    ("^<<"
     (0 font-lock-function-name-face nil t))
    ("#.*"
     (0 font-lock-comment-face nil t)))
  "Keywords to highlight in finkinfo mode.")

;;;###autoload
(define-derived-mode finkinfo-mode text-mode "Finkinfo"
  "Major mode for fink info files in Mac OS X.
\\{finkinfo-mode-map}"
  (set-syntax-table finkinfo-mode-syntax-table)
  (setq font-lock-defaults '(finkinfo-font-lock-keywords))
  (run-hooks 'finkinfo-mode-hook))

(define-key finkinfo-mode-map (kbd "C-c C-o") 'finkinfo-insert-keyword)

(defun finkinfo-insert-keyword ()
  "Insert keywords defined by `finkinfo-keywords'."
  (interactive)
  (insert (ido-completing-read "Insert keyword: " finkinfo-keywords)
          ": "))


(provide 'finkinfo-mode)

;;; finkinfo-mode.el ends here
