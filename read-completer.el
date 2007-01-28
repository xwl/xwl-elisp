;;; read-completer.el --- read completer

;; Copyright (C) 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1
;; Last updated: 2007/01/28 17:33:23

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; This package defines a ido-style read completer. I find it useful for
;; writing various types of tags, like muse tags, latex tags, etc.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'read-completer)

;;; Code:

;; Customizations

(defgroup read-completer nil
  "Read completer package."
  :group 'read-completer)

(defcustom read-completer-latex-tags
      `(,@(mapcar
           (lambda (el)
             `(,el . ,(format "\\%s{.}" el el)))
           '("chapter" "paragraph" "part" "section" "subsection"
             "subparagraph" "subsubsection" "textbf" "textsc" "emph"
             "textsf" "textit" "textmd" "textnormal" "textrm" "textsl"
             "texttt" "textup" "mathcal" "mathbf" "mathsf" "mathit"
             "mathnormal" "mathrm" "mathtt"))
        ,@(mapcar
           (lambda (el)
             `(,el . ,(format "\\begin{%s}\n.\n\\end{%s}" el el)))
           '("abstract" "array" "center" "description" "displaymath"
             "document" "enumerate" "eqnarray" "eqnarray*" "equation"
             "figure" "figure*" "filecontents" "filecontents*" "flushleft"
             "flushright" "itemize" "list" "math" "minipage" "picture"
             "quotation" "quote" "sloppypar" "tabbing" "table" "table*"
             "tabular" "tabular*" "thebibliography" "theindex" "titlepage"
             "trivlist" "verbatim" "verbatim*" "verse")))
      "latex tags."
      :type 'symbol
      :group 'read-completer)



(defmacro define-read-completer (name l)
  "Declare a read completer.
This completer takes one argument, say TAG, it will complete TAG in
ido's style by matching against alist L.
NAME is for naming this kind of completion.
L is like,

    ((\"a\" . \"\\\\begin{hi}{.}\\\\end{hi}\")
     (\"b\" . \"hehe\"))

Cursor will be placed at `.' if it exists."

  `(defun ,(intern (format "read-completer-%s" name)) (tag)
     ,(format "Complete TAG against `%s'." l)
     (interactive
      (list (ido-completing-read "Tag: " (mapcar 'car ,l))))
     (let ((bound (point)))
       (insert (cdr (assoc tag ,l)))
       (let ((point (re-search-backward "\\." bound t)))
         (if point
             (progn (goto-char point)
                    (delete-char 1))
           (insert "\n"))))))

(define-read-completer latex read-completer-latex-tags)

(provide 'read-completer)

;;; read-completer.el ends here
