;;; ga-chicken.el --- chicken backend (Chicken Scheme)

;; Copyright (C) 2010 William Xu

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
(defvar ga-chicken-font-lock-keywords nil)
(defvar ga-chicken-sources-file nil)

;; local repository path for chicken eggs
(defvar ga-chicken-repository nil)

;; Interfaces
;; (defun ga-chicken-update ()
;;   (ga-run-command (list "update")))

;; (defun ga-chicken-search-by-name (pkg)
;;   (ga-run-other-command (list "apt-cache" "search" "-n" pkg)))

;; (defun ga-chicken-search (pkg)
;;   (ga-run-other-command (list "apt-cache" "search" pkg)))

(defun ga-chicken-show (pkg)
  (let ((inhibit-read-only t)
        (s ""))
    (with-temp-buffer
      (insert-file-contents
       (format "%s/%s/trunk/%s.meta" ga-chicken-repository pkg pkg))
      (goto-char (point-max))
      (insert "\n;; --------------------------------------------\n\n")
      (insert-file-contents
       (format "%s/%s/trunk/%s.setup" ga-chicken-repository pkg pkg))
      (setq s (buffer-string)))

    (erase-buffer)
    (insert s)
    (ga-insert-end-string)))

(defun ga-chicken-install (pkg)
  (ga-run-command (list pkg)))

(defun ga-chicken-listfiles (pkg)
  (ga-run-other-command (list "chicken-status" "-files" pkg)))

(defalias 'ga-chicken-upgrade 'ga-chicken-install)

;; (defun ga-chicken-clean ()
;;   (ga-run-other-command (list "apt-cache" "clean")))

(defun ga-chicken-remove (pkg)
  (ga-run-other-command (list "chicken-uninstall" pkg)))

;; Misc

(defun ga-chicken-update-available-pkgs ()
  (setq ga-available-pkgs
        (cons
         (list
          'chicken
          (if ga-chicken-repository
              (directory-files ga-chicken-repository nil "^[^.]+")
            '("3viewer" "9ML-toolkit" "9p" "F-operator" "abnf" "advice" "aes"
              "agrep" "amb" "ansi-escape-sequences" "apropos" "args" "atlas-lapack" "atom"
              "autocompile" "autoform" "autoform-jquery" "autoform-postgresql" "autoload"
              "awful" "awful-postgresql" "awful-sql-de-lite" "awful-sqlite3" "awk"
              "banterpixra" "base64" "bb" "big-chicken" "binary-heap" "binary-parse" "bind"
              "blas" "blob-record" "bloom-filter" "box" "breadcrumbs" "byte-blob"
              "byte-blob-stream" "bytevector" "c3" "cairo" "char-set-literals" "charconv"
              "check-errors" "chickadee" "chicken-doc" "chicken-doc-admin" "coerce"
              "colorize" "combinators" "condition-utils" "coops" "coops-utils" "couchdb"
              "couchdb-view-server" "crc" "crunch" "crypto-tools" "csv" "datatype"
              "date-literals" "dbus" "debug" "defstruct" "dict" "digraph" "directory-utils"
              "dissector" "doctype" "dollar" "dot-locking" "dsssl-utils" "dyn-vector"
              "easyffi" "eggdoc" "eggdoc-svnwiki" "elliptic-curves" "embedded-test"
              "endian-blob" "endian-port" "environments" "epeg" "epoll" "err5rs-arithmetic"
              "error-utils" "estraier-client" "expand-full" "expat" "ext-direct" "ezxdisp"
              "fancypants" "fast-generic" "fast-loop" "fastcgi" "filepath" "fmt" "foof-loop"
              "foreigners" "format" "format-compiler" "format-compiler-base" "format-graph"
              "format-textdiff" "formular" "fpio" "fps" "free-gettext" "freetype" "ftl"
              "ftp" "gdbm" "geoip" "getopt-long" "glpk" "glut" "google-v8" "gopher" "gps"
              "graph-bfs" "graph-cycles" "graph-dfs" "graph-dominators" "graph-scc"
              "graph-separators" "graph-ssa" "graphics-gems" "hashes" "heap-o-rama"
              "henrietta" "hfs+" "high-load-scheduler" "honu" "hostinfo" "html-form"
              "html-parser" "html-tags" "html-utils" "htmlprag" "http-client" "http-session"
              "hyde" "iconv" "imlib2" "input-classes" "input-parse" "intarweb" "interfaces"
              "internet-message" "internet-timestamp" "interp1d" "ioctl" "irc" "iset"
              "javahack" "jsmin" "json" "json-abnf" "kanren" "lalr" "latch" "lazy-ffi"
              "lexgen" "libsvm" "linenoise" "list" "list-of" "list-utils" "locale" "log5scm"
              "lognum" "lookup-table" "loop" "loopy-loop" "lru-cache" "lzma" "macosx"
              "mailbox" "mailbox-threads" "make" "matchable" "mathh" "matrix-utils" "mbox"
              "md5" "message-digest" "mime" "miniML" "miscmacros" "mistie"
              "modular-arithmetic" "moremacros" "mpd-client" "mpfi" "mpi" "multidoc"
              "multimethod" "mw" "mw-core" "ncurses" "nemo" "neuromorpho" "npdiff"
              "number-limits" "numbers" "numspell" "objc" "object-graph" "oblist" "octave"
              "openal" "opengl" "openssl" "operations" "osxattr" "packedobjects" "packrat"
              "pandora" "patch" "pdf" "peep" "phoghorn" "php-s11n" "phricken" "pool" "pop3"
              "postgresql" "probdist" "procedure-decoration" "progress-indicators"
              "prometheus" "protobj" "pstk" "pty" "pyffi" "qt" "qt-light" "qwiki"
              "random-mtzig" "random-swb" "random-test" "rb-tree" "readline"
              "record-variants" "records" "regex" "regex-case" "regex-literals"
              "remote-mailbox" "rfc3339" "ripemd" "rpc" "rss" "s11n" "s48-modules"
              "salmonella" "sandbox" "sassy" "scbib" "scgi" "scss" "sdbm" "sdl" "sedna"
              "sendfile" "sets" "setup-helper" "sfht" "sha1" "sha2" "sigma" "signal-diagram"
              "silex" "simple-logging" "simple-units" "slice" "smtp" "sparse-vectors"
              "special-case" "specialized-io" "spiffy" "spiffy-cookies"
              "spiffy-directory-listing" "spiffy-request-vars" "spiffy-uri-match"
              "sql-de-lite" "sql-null" "sqlite3" "srfi-100" "srfi-101" "srfi-102" "srfi-19"
              "srfi-25" "srfi-27" "srfi-27-reference" "srfi-29" "srfi-33" "srfi-34"
              "srfi-37" "srfi-38" "srfi-4-comprehensions" "srfi-4-utils" "srfi-40" "srfi-41"
              "srfi-42" "srfi-45" "srfi-60" "srfi-63" "srfi-78" "srfi-9-ext" "srfi-95"
              "srfi-modules" "ssax" "stack" "static-modules" "statistics" "strictly-pretty"
              "string-utils" "stty" "suspension" "svn-client" "svnwiki-sxml" "sxml-fu"
              "sxml-informal" "sxml-serializer" "sxml-templates" "sxml-transforms" "sxpath"
              "symbol-utils" "syn-param" "synch" "syslog" "tabexpand" "tcp-server" "termite"
              "test" "testeez" "tiger-hash" "tiny-prolog" "tinyclos" "tinyclos-xerox" "tk"
              "tokyocabinet" "trace" "treap" "typeclass" "udp" "ugarit" "unitconv"
              "unix-sockets" "uri-common" "uri-dispatch" "uri-generic" "uri-match" "utf8"
              "uuid-lib" "uuid-ossp" "vandusen" "varsubst" "vector-lib" "vfs" "wiki-parse"
              "wmiirc" "x11-colors" "xlib" "xml-rpc" "xosd" "yasos" "yelp" "z3")))
         (remove-if (lambda (i) (eq (car i) 'chicken))
                    ga-available-pkgs))))


(provide 'ga-chicken)

;;; ga-chicken.el ends here
