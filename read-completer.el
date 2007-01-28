;;; read-completer.el --- read completer

;; Copyright (C) 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1
;; Last updated: 2007/01/28 16:48:33

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
      '( ;; sections
        ("chapter"       . "\\chapter{.}")
        ("paragraph"     . "\\paragraph{.}")
        ("part"          . "\\part{.}")
        ("section"       . "\\section{.}")
        ("subsection"    . "\\subsection{.}")
        ("subparagraph"  . "\\subparagraph{.}")
        ("subsubsection" . "\\subsubsection{.}")
        ;; fonts
        ("textbf"     . "\\textbf{.}")
        ("textsc"     . "\\textsc{.}")
        ("emph"       . "\\emph{.}")
        ("textsf"     . "\\textsf{.}")
        ("textit"     . "\\textit{.}")
        ("textmd"     . "\\textmd{.}")
        ("textnormal" . "\\textnormal{.}")
        ("textrm"     . "\\textrm{.}")
        ("textsl"     . "\\textsl{.}")
        ("texttt"     . "\\texttt{.}")
        ("textup"     . "\\textup{.}")
        ("mathcal"    . "\\mathcal{.}")  ; math fonts?
        ("mathbf"     . "\\mathbf{.}")
        ("mathsf"     . "\\mathsf{.}")
        ("mathit"     . "\\mathit{.}")
        ("mathnormal" . "\\mathnormal{.}")
        ("mathrm"     . "\\mathrm{.}")
        ("mathtt"     . "\\mathtt{.}")
        ;; environments
        ("abstract"        . "\\begin{abstract}\n.\n\\end{abstract}")
        ("array"           . "\\begin{array}\n.\n\\end{array}")
        ("center"          . "\\begin{center}\n.\n\\end{center}")
        ("description"     . "\\begin{description}\n.\n\\end{description}")
        ("displaymath"     . "\\begin{displaymath}\n.\n\\end{displaymath}")
        ("document"        . "\\begin{document}\n.\n\\end{document}")
        ("enumerate"       . "\\begin{enumerate}\n.\n\\end{enumerate}")
        ("eqnarray"        . "\\begin{eqnarray}\n.\n\\end{eqnarray}")
        ("eqnarray*"       . "\\begin{eqnarray*}\n.\n\\end{eqnarray*}")
        ("equation"        . "\\begin{equation}\n.\n\\end{equation}")
        ("figure"          . "\\begin{figure}\n.\n\\end{figure}")
        ("figure*"         . "\\begin{figure*}\n.\n\\end{figure*}")
        ("filecontents"    . "\\begin{filecontents}\n.\n\\end{filecontents}")
        ("filecontents*"   . "\\begin{filecontents*}\n.\n\\end{filecontents*}")
        ("flushleft"       . "\\begin{flushleft}\n.\n\\end{flushleft}")
        ("flushright"      . "\\begin{flushright}\n.\n\\end{flushright}")
        ("itemize"         . "\\begin{itemize}\n.\n\\end{itemize}")
        ("list"            . "\\begin{list}\n.\n\\end{list}")
        ("math"            . "\\begin{math}\n.\n\\end{math}")
        ("minipage"        . "\\begin{minipage}\n.\n\\end{minipage}")
        ("picture"         . "\\begin{picture}\n.\n\\end{picture}")
        ("quotation"       . "\\begin{quotation}\n.\n\\end{quotation}")
        ("quote"           . "\\begin{quote}\n.\n\\end{quote}")
        ("sloppypar"       . "\\begin{sloppypar}\n.\n\\end{sloppypar}")
        ("tabbing"         . "\\begin{tabbing}\n.\n\\end{tabbing}")
        ("table"           . "\\begin{table}\n.\n\\end{table}")
        ("table*"          . "\\begin{table*}\n.\n\\end{table*}")
        ("tabular"         . "\\begin{tabular}\n.\n\\end{tabular}")
        ("tabular*"        . "\\begin{tabular*}\n.\n\\end{tabular*}")
        ("thebibliography" . "\\begin{thebibliography}\n.\n\\end{thebibliography}")
        ("theindex"        . "\\begin{theindex}\n.\n\\end{theindex}")
        ("titlepage"       . "\\begin{titlepage}\n.\n\\end{titlepage}")
        ("trivlist"        . "\\begin{trivlist}\n.\n\\end{trivlist}")
        ("verbatim"        . "\\begin{verbatim}\n.\n\\end{verbatim}")
        ("verbatim*"       . "\\begin{verbatim*}\n.\n\\end{verbatim*}")
        ("verse"           . "\\begin{verse}\n.\n\\end{verse}"))
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
