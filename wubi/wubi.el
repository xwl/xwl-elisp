;;; wubi.el --- chinese-wubi input method in Emacs -*- coding: utf-8; -*-

;; Copyright (C) 2005, 2007, 2010, 2011 William Xu

;; Authors: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;
;; (require 'wubi)
;; (register-input-method
;;  "chinese-wubi" "Chinese" 'quail-use-package "wubi" "wubi")
;; (setq default-input-method "chinese-wubi")

;;; History

;; This is forked from Yuwen Dai <daiyuwen@freeshell.org>'s wubi.el
;; extension.

;;; Code:

(require 'quail)

(defgroup wubi nil
  "chinese-wubi input method.")

(defcustom wubi-phrases-file "~/.wubi-phrases.el"
  "User could add new phrases here."
  :type 'string
  :group 'wubi)

(defcustom wubi-quanjiao-p 1
  "Use quanjiao(全角) or banjiao(半角) for chinese punctuations.

Note: this variable should be set before loading wubi, or it won't work
properly."
  :type 'boolean
  :group 'wubi)

(defcustom wubi-traditional-p -1
  "Non-nil value will input traditional chinese characters.

Note: this variable should be set before loading wubi, or it won't work
properly."
  :type 'boolean
  :group 'wubi)

(defvar wubi-table (make-char-table 'wubi-table nil))

(defun wubi-quail-define-package ()
  (quail-define-package
   "chinese-wubi" "Chinese" (if wubi-traditional-p "五筆字型" "五笔字型")
   '((121 . "言y")
     (120 . "纟x")
     (119 . "人w")
     (118 . "女v")
     (117 . "立u")
     (116 . "禾t")
     (115 . "木s")
     (114 . "白r")
     (113 . "金r")
     (112 . "之p")
     (111 . "火o")
     (110 . "已n")
     (109 . "山m")
     (108 . "田l")
     (107 . "口k")
     (106 . "日j")
     (105 . "水i")
     (104 . "目h")
     (103 . "王g")
     (102 . "土f")
     (101 . "月e")
     (100 . "大d")
     (99  . "又c")
     (98  . "子b")
     (97  . "工a")
     )"汉字输入∷五笔字型∷
Created by Dai Yuwen. daiyuwen@freeshell.org

	五笔字型汉字编码方案

 键  区  码  键名  笔形、基本字根

	 g     (11)  王   一,五,戋
  f  横 (12)  土   二,士,十,干,寸,雨
	 d  起 (13)  大   三,犬,石,古,厂
	 s  类 (14)  木   西,丁
	 a     (15)  工   匚,七弋,戈,廾艹廿

	 h     (21)  目   上,止,卜
  j  竖 (22)  日   曰,早,虫,刂
	 k  起 (23)  口   川
	 l  类 (24)  田   甲,囗,四皿,车,力
	 m     (25)  山   由,冂,贝,几

	 t     (31)  禾   丿,竹,彳,攵夂
  r  撇 (32)  白   手扌,斤
	 e  起 (33)  月   彡,月,用,乃,豕
	 w  类 (34)  人   亻,八
	 q     (35)  金   金钅,勹夕犭,儿

	 y     (41)  言   讠,亠,广,文,方
  u  捺 (42)  立   冫丬,六,辛,疒,门
	 i  起 (43)  水   氵,小
	 o  类 (44)  火   灬,米
	 p     (45)  之   辶廴,冖宀

	 n     (51)  已   乙,已己巳,尸,心忄,羽
  b  折 (52)  子   孑,凵,了,阝,耳,卩,也
	 v  起 (53)  女   巛,刀,九,彐,臼
	 c  类 (54)  又   厶,巴,马
	 x     (55)  纟   幺,弓,匕

              键名谱
  (横区): 工 木 大 土 王
  (坚区): 目 日 口 田 山
  (撇区): 金 人 月 白 禾
  (捺区): 言 立 水 火 之
  (折区): 已 子 女 又 纟

              助记词
  G 王旁青头戋五一
  F 土士二干十寸雨
  D 大犬三羊古石厂
  S 木丁西
  A 工戈草头右框七

  H 目具上止卜虎皮 (具上 指具字的上部 且)
  J 日早两竖与虫依
  K 口与川, 字根稀
  L 田甲方框四车力
  M 山由贝, 下框几

  T 禾竹一撇双人立, 反文条头共三一
  R 白手看头三二斤 (三二 指键为 32)
  E 月彡乃用家衣底
  W 人和八, 三四里 (三四 即 34)
  Q 金勺缺点无尾鱼, 犬旁留X儿一点夕, 氏无七(妻)

  Y 言文方广在四一, 高头一捺谁人去
  U 立辛两点六门病
  I 水旁兴头小倒立
  O 火业头, 四点米
  P 之宝盖, 摘(示)(衣)

  N 已半巳满不出己 左框折尸心和羽
  B 子耳了也框向上
  V 女刀九臼山朝西
  C 又巴马 丢矢矣
  X 慈母无心弓和匕 幼无力
"
   '(("" . quail-delete-last-char)
     (" " . quail-select-current)
     ;;    ("." . quail-next-translation)
     ;;    (">" . quail-next-translation)
     ;;    ("," . quail-prev-translation)
     ;;    ("<" . quail-prev-translation)
     ))

  (put 'wubi-table 'char-table-extra-slots 0))

(defun wubi-load-local-phrases ()
  "Load phrases from `wubi-phrases-file'."
  (interactive)
  (when (file-exists-p wubi-phrases-file)
    (load wubi-phrases-file)
    (mapc (lambda (rule)
            (quail-defrule-internal
             (car rule) (cadr rule) (quail-map) t))
          wubi-local-phrases)))

(defvar wubi-ascii-quanjiao-banjiao-table
      '(("/" (["、"]) (["/"]))
	("," (["，"]) ([","]))
	("." (["。"]) (["."]))
	("*" (["·"]) (["*"]))
	("'" (["‘" "’"]) (["'"]))
	("\"" (["“" "”"]) (["\""]))
	(":" (["："]) ([":"]))
	("\;" (["；"]) ([";"]))
	("?" (["？"]) (["?"]))
	("!" (["！"]) (["!"]))
	("(" (["（"]) (["("]))
	(")" (["）"]) ([")"]))
	("<" (["《" "〈"]) (["<"]))
	(">" (["》" "〉"]) ([">"]))
	("[" (["[" "【" "〖"]) (["["]))
	("]" (["]" "】" "〗"]) (["]"]))
	("\\" (["…"]) (["\\"]))
	("-" (["-" "—"]) (["-"])))
      "ascii, quanjiao, banjiao table.")

(defun wubi-toggle-quanjiao-banjiao (&optional arg)
  "Toggle quanjiao(全角)/banjiao(半角).
Unequivocally use quanjiao for positive ARG; banjiao for negative ARG."
  (interactive "P")
  (if arg
      (setq wubi-quanjiao-p (> (prefix-numeric-value arg) 0))
    (setq wubi-quanjiao-p (not wubi-quanjiao-p)))
  (if wubi-quanjiao-p
      (message "进入全角标点模式")
    (message "进入半角标点模式"))
  (mapc
   (lambda (ascii-quajiao-banjiao)
     ;; redefine rules
     (quail-defrule-internal
      (car ascii-quajiao-banjiao)
      (if wubi-quanjiao-p
          (cadr ascii-quajiao-banjiao)
        (caddr ascii-quajiao-banjiao))
      (quail-map))
     ;; install new keys
     (quail-lookup-key (car ascii-quajiao-banjiao)))
   wubi-ascii-quanjiao-banjiao-table))

(defun wubi-toggle-simplified-or-traditional (&optional arg)
  "Toggle inputting simplified or traditional characters.
Unequivocally use traditional characters for positive ARG; simplified characters
for negative ARG."
  (interactive "P")
  (if arg
      (setq wubi-traditional-p (> (prefix-numeric-value arg) 0))
    (setq wubi-traditional-p (not wubi-traditional-p)))
  ;; Refresh mode line indicator.
  (wubi-quail-define-package)
  (when current-input-method
    (toggle-input-method)
    (toggle-input-method))
  ;; Load standard phrases
  (if wubi-traditional-p
      (load "wubi-rules-traditional")
    (load "wubi-rules"))
  (wubi-load-local-phrases))

;; setup
(wubi-toggle-simplified-or-traditional wubi-traditional-p)
(wubi-toggle-quanjiao-banjiao wubi-quanjiao-p)

(provide 'wubi)

;;; wubi.el ends here
