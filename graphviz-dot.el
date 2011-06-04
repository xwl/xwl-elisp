;;; graphviz-dot.el --- Major mode for editing graphviz dot files

;; Copyright (C) 2011  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cc-mode)

(defvar graphviz-dot-grammer-keywords
  '("node" "edge" "graph" "digraph" "subgraph" "strict"))

(defvar graphviz-dot-attribute-keywords
  '("Damping" "K" "URL" "area" "arrowhead" "arrowsize" "arrowtail" "aspect" "bb"
    "bgcolor" "center" "charset" "clusterrank" "color" "colorscheme" "comment"
    "compound" "concentrate" "constraint" "decorate" "defaultdist" "dim" "dimen"
    "dir" "diredgeconstraints" "distortion" "dpi" "edgeURL" "edgehref"
    "edgetarget" "edgetooltip" "epsilon" "esep" "fillcolor" "fixedsize"
    "fontcolor" "fontname" "fontnames" "fontpath" "fontsize" "group" "headURL"
    "headclip" "headhref" "headlabel" "headport" "headtarget" "headtooltip"
    "height" "href" "id" "image" "imagescale" "label" "labelURL" "label_scheme"
    "labelangle" "labeldistance" "labelfloat" "labelfontcolor" "labelfontname"
    "labelfontsize" "labelhref" "labeljust" "labelloc" "labeltarget"
    "labeltooltip" "landscape" "layer" "layers" "layersep" "layout" "len"
    "levels" "levelsgap" "lhead" "lheight" "lp" "ltail" "lwidth" "margin"
    "maxiter" "mclimit" "mindist" "minlen" "mode" "model" "mosek" "nodesep"
    "nojustify" "normalize" "nslimit" "nslimit1" "ordering" "orientation"
    "orientation" "outputorder" "overlap" "overlap_scaling" "pack" "packmode"
    "pad" "page" "pagedir" "pencolor" "penwidth" "peripheries" "pin" "pos"
    "quadtree" "quantum" "rank" "rankdir" "ranksep" "ratio" "rects" "regular"
    "remincross" "repulsiveforce" "resolution" "root" "rotate" "rotation"
    "samehead" "sametail" "samplepoints" "scale" "searchsize" "sep" "shape"
    "shapefile" "showboxes" "sides" "size" "skew" "smoothing" "sortv" "splines"
    "start" "style" "stylesheet" "tailURL" "tailclip" "tailhref" "taillabel"
    "tailport" "tailtarget" "tailtooltip" "target" "tooltip" "truecolor"
    "vertices" "viewport" "voro_margin" "weight" "width" "z")
  "See http://www.graphviz.org/doc/info/attrs.html.")

(defvar graphviz-dot-value-keywords
  '("true" "false" "normal" "inv" "dot" "invdot" "odot" "invodot"
    "none" "tee" "empty" "invempty" "diamond" "odiamond" "box" "obox"
    "open" "crow" "halfopen" "local" "global" "none" "forward" "back"
    "both" "none" "BL" "BR" "TL" "TR" "RB" "RT" "LB" "LT" ":n" ":ne" ":e"
    ":se" ":s" ":sw" ":w" ":nw" "same" "min" "source" "max" "sink" "LR"
    "box" "polygon" "ellipse" "circle" "point" "egg" "triangle"
    "plaintext" "diamond" "trapezium" "parallelogram" "house" "hexagon"
    "octagon" "doublecircle" "doubleoctagon" "tripleoctagon" "invtriangle"
    "invtrapezium" "invhouse" "Mdiamond" "Msquare" "Mcircle" "record"
    "Mrecord" "dashed" "dotted" "solid" "invis" "bold" "filled"
    "diagonals" "rounded"))

(defvar graphviz-dot-font-lock-keywords
  `(("/\\*.*\\*/\\|//.*"                ; comment
     (0 font-lock-comment-face t t))
    (,(regexp-opt graphviz-dot-grammer-keywords)
     (0 font-lock-keyword-face nil t))
    (,(regexp-opt graphviz-dot-attribute-keywords)
     (0 font-lock-variable-name-face nil t))
    (,(regexp-opt graphviz-dot-value-keywords)
     (0 font-lock-constant-face nil t))))

(defvar graphviz-dot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

(defvar graphviz-dot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ";") (lambda ()
                                (interactive)
                                (insert ";")
                                (newline-and-indent)))
    (define-key map (kbd "{") (lambda ()
                                (interactive)
                                (insert "{")
                                (newline-and-indent)))
    (define-key map (kbd "}") (lambda ()
                                (interactive)
                                (insert "}")
                                (backward-char)
                                (indent-according-to-mode)
                                (forward-char)
                                (newline-and-indent)))
    map))

;;;###autoload
(define-derived-mode graphviz-dot-mode text-mode "Graphviz-Dot"
  "Major mode for editing Graphviz dot files.
\\{graphviz-dot-mode-map}"
  :syntax-table graphviz-dot-mode-syntax-table
  (setq font-lock-defaults '(graphviz-dot-font-lock-keywords))
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'indent-line-function)
       'graphviz-dot-indent-line)
  (run-hooks 'graphviz-dot-mode-hook))

(defun graphviz-dot-indent-line ()
  "Indent line according to Graphviz Dot indentation rules."
  (interactive)
  (unless (bobp)
    (let ((col tab-width))
      (save-excursion
        (forward-line -1)
        (setq col (max col (current-indentation)))
        (goto-char (line-end-position))
        (cond
         ((looking-back (concat "\\({\\|\\[\\(.+,\\)?\\)" "[[:blank:]]*"))
          (setq col (+ col tab-width)))
         ((looking-back "^[^[]*\\][[:blank:];]*")
          (setq col (- col tab-width)))))
      (when (looking-at "}")
        (setq col (- col tab-width)))
      (setq col (max col 0))
      (indent-line-to col))))

(provide 'graphviz-dot)
;;; graphviz-dot.el ends here
