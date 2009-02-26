;;; wordnet.el --- an interface for Word Net

;; Copyright (C) 2004, 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Created: 2004/10/21 19:44:23
;; Version: 2.0
;; Keywords: convenience
;; Url: http://xwl.appspot.com/ref/wordnet.el
;; Last updated: 2007/09/10 17:04:39

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A simple interface for the dictionary - Word Net

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'wordnet)

;; Recommended key binding:
;;   (global-set-key (kbd "M-s") 'wordnet-search)

;; History

;; wordnet.el is an interface for the dictionary -- Word Net, which's
;; very wonderful! Although there is already a dictionary.el, which is
;; used as an interface for dictd, the dictionary.el has not made full
;; use of Word Net! It only uses wordnet's "-overview" option. The other
;; options like "-syns, -hypo, ants, ..." are all ignored!

;; I've also found "Thomas Link AKA samul AT web DOT de" 's wordnet.el
;; before i wrote this, but it won't work, which kept on complaining
;; some functions not found. It was based on XEmacs, but i'm using GNU
;; Emacs.  Having tried to hack wordnet.el for a while, i gave up. :(
;; And I wrote this simple one myself.

;;; Code:

;;; Variables:

(defgroup wordnet nil
  "Wordnet interface."
  :group 'wordnet)

(defcustom wordnet-command "wn"
  "Shell command for wordnet."
  :type 'string
  :group 'wordnet)

(defcustom wordnet-mode-hook nil
  "Normal hook run after entering wordnet mode."
  :type 'hook
  :group 'wordnet)

(defvar wordnet-options
  "-antsn -antsv -antsa -antsr\
	-hypen -hypev\
	-hypon -hypov\
	-entav\
	-synsn -synsv -synsa -synsr\
	-smemn\
	-ssubn\
	-sprtn\
	-membn\
	-subsn\
	-partn\
	-meron\
	-holon\
	-causv\
	-perta -pertr\
	-attrn -attra\
	-derin -deriv\
	-domnn -domnv -domna -domnr\
	-domtn -domtv -domta -domtr\
	-famln -famlv -famla -famlr\
	-framv\
	-coorn -coorv\
	-simsv\
	-hmern\
	-hholn\
	-grepn -grepv -grepa -grepr\
	-over")

;;; Functions:

(defun wordnet-quit ()
  "Bury Word Net buffer."
  (interactive)
  (delete-windows-on (buffer-name)))

(defun wordnet-next-sense ()
  "Goto next Sense."
  (interactive)
  (end-of-line)
  (search-forward-regexp "^Sense\\ [0-9]\\|^[0-9]" nil t)
  (beginning-of-line))

(defun wordnet-prev-sense ()
  "Goto previous Sense."
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^Sense\\ [0-9]\\|^[0-9]" nil t)
  (beginning-of-line))

(defun wordnet-antonyms ()
  "Goto -ants{n|v|a|r}."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Antonyms\\ of" nil t)
  (beginning-of-line))

(defun wordnet-synonyms ()
  "Goto -syns{n|v|a|r}."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Synonyms\\ of\\|^Synonyms/Hypernyms\\|Similarity\\ of" nil t)
  (beginning-of-line))

(defun wordnet-hyponyms ()
  "Goto -hypo{n|v}, -tree{n|v}."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Hyponyms\\ of" nil t)
  (beginning-of-line))

(defun wordnet-overview ()
  "Goto -overview."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Overview\\ of" nil t)
  (beginning-of-line))

(defvar wordnet-font-lock-keywords
  `(,(concat "^\\("
             (regexp-opt '("Antonyms" "Synonyms" "Hyponyms" "Member"
                           "Substance" "Part" "Meronyms" "Holonyms"
                           "Attributes" "Derived" "Domain" "Familiarity"
                           "Coordinate" "Grep" "Overview" "Similarity"
                           "Pertainyms" "Troponyms" "Entailment"))
             "\\).*")
    (0 font-lock-keyword-face t t))
  "Keywords to highlight in wordnet mode.")

(defvar wordnet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'wordnet-quit)
    (define-key map (kbd "n") 'wordnet-next-sense)
    (define-key map (kbd "p") 'wordnet-prev-sense)
    (define-key map (kbd "a") 'wordnet-antonyms)
    (define-key map (kbd "s") 'wordnet-synonyms)
    (define-key map (kbd "h") 'wordnet-hyponyms)
    (define-key map (kbd "o") 'wordnet-overview)
    map))
  
(define-derived-mode wordnet-mode nil "WordNet"
  "Major mode for WordNet dictionary search.
\\{wordnet-mode-map}"
  (setq font-lock-defaults '(wordnet-font-lock-keywords))
  (run-hooks 'wordnet-mode-hook))

(defun wordnet-search (word)
  "Search the WORD with WordNet if given. 
It presents the word at point as default input and allows editing it."
  (interactive (list (read-string "Wordnet: " (current-word))))
  (unless word 
    (setq word (read-string "Wordnet: ")))
  (let ((buf (get-buffer-create "*WordNet*")))
    (with-current-buffer buf
      (unless (eq major-mode 'wordnet-mode)
        (wordnet-mode))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (shell-command-to-string 
          (format "%s %s %s" wordnet-command word wordnet-options)))
        (wordnet-overview)))
    ;; switch to *WordNet* buffer
    (unless (eq (current-buffer) buf)
      (unless (cdr (window-list))
        (split-window-vertically))
      (other-window 1)
      (switch-to-buffer buf))))

(defalias 'wordnet 'wordnet-search)

(provide 'wordnet)

;;; wordnet.el ends here
