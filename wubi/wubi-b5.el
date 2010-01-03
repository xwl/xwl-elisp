;;; wubi.el --- chinese-wubi input method in Emacs -*- coding: big5; -*-

;; Copyright (C) 2005, 2007 William Xu

;; Authors: William Xu <william.xwl@gmail.com>

;; Last updated: 2007/03/17 20:50:35

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

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;
;; (require 'wubi)
;; (register-input-method
;;  "chinese-wubi" "Chinese" 'quail-use-package "wubi" "wubi")
;; (wubi-load-local-phrases)
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

(quail-define-package "chinese-wubi" "Chinese" "五筆字型"
 '((121 . "言y")
   (120 . "□x")
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
)"漢字輸入□五筆字型□
Created by Dai Yuwen. daiyuwen@freeshell.org

	五筆字型漢字編碼方案

 鍵  區  碼  鍵名  筆形、基本字根

	 g     (11)  王   一,五,戔
  f  橫 (12)  土   二,士,十,干,寸,雨
	 d  起 (13)  大   三,犬,石,古,廠
	 s  類 (14)  木   西,丁
	 a     (15)  工   匚,七弋,戈,廾□廿

	 h     (21)  目   上,止,卜
  j  豎 (22)  日   曰,早,虫,□
	 k  起 (23)  口   川
	 l  類 (24)  田   甲,囗,四皿,車,力
	 m     (25)  山   由,□,貝,几

	 t     (31)  禾   □,竹,彳,□□
  r  撇 (32)  白   手□,斤
	 e  起 (33)  月   □,月,用,乃,豕
	 w  類 (34)  人   □,八
	 q     (35)  金   金□,□夕□,兒

	 y     (41)  言   □,□,廣,文,方
  u  捺 (42)  立   □□,六,辛,□,門
	 i  起 (43)  水   □,小
	 o  類 (44)  火   □,米
	 p     (45)  之   □□,□□

	 n     (51)  已   乙,已己巳,尸,心□,羽
  b  折 (52)  子   孑,凵,了,□,耳,□,也
	 v  起 (53)  女   □,刀,九,□,臼
	 c  類 (54)  又   □,巴,馬
	 x     (55)  □   □,弓,匕

              鍵名譜
  (橫區): 工 木 大 土 王
  (堅區): 目 日 口 田 山
  (撇區): 金 人 月 白 禾
  (捺區): 言 立 水 火 之
  (折區): 已 子 女 又 □

              助記詞
  G 王旁青頭戔五一
  F 土士二干十寸雨
  D 大犬三羊古石廠
  S 木丁西
  A 工戈草頭右框七

  H 目具上止卜虎皮 (具上 指具字的上部 且)
  J 日早兩豎與虫依
  K 口與川, 字根稀
  L 田甲方框四車力
  M 山由貝, 下框几

  T 禾竹一撇雙人立, 反文條頭共三一
  R 白手看頭三二斤 (三二 指鍵為 32)
  E 月□乃用家衣底
  W 人和八, 三四里 (三四 即 34)
  Q 金勺缺點無尾魚, 犬旁留X兒一點夕, 氏無七(妻)

  Y 言文方廣在四一, 高頭一捺誰人去
  U 立辛兩點六門病
  I 水旁興頭小倒立
  O 火業頭, 四點米
  P 之寶蓋, 摘(示)(衣)

  N 已半巳滿不出己 左框折尸心和羽
  B 子耳了也框向上
  V 女刀九臼山朝西
  C 又巴馬 丟矢矣
  X 慈母無心弓和匕 幼無力
"
 '(("" . quail-delete-last-char)
   (" " . quail-select-current)
;;    ("." . quail-next-translation)
;;    (">" . quail-next-translation)
;;    ("," . quail-prev-translation)
;;    ("<" . quail-prev-translation)
   )
  nil nil nil nil)

(put 'wubi-table 'char-table-extra-slots 0)

(defvar wubi-table (make-char-table 'wubi-table nil))

;; standard phrases
(load "wubi-rules.el")

;; local phrases
(defun wubi-load-local-phrases ()
  "Load phrases from `wubi-phrases-file'."
  (interactive)
  (load wubi-phrases-file)
  (mapc (lambda (rule)
	  (quail-defrule-internal
	   (car rule) (cadr rule) (quail-map) t))
	wubi-local-phrases))


;;; Quanjiao(全角)/Banjiao(半角) symbols

(defvar wubi-quanjiaop t
  "Whether in quanjiao mode or not.")

(defvar wubi-ascii-quanjiao-banjiao-table
      '(("/" (["、"]) (["/"]))
	("," (["，"]) ([","]))
	("." (["。"]) (["."]))
	("*" (["·"]) (["*"]))
	("'" (["‘" "’"]) (["'"]))
	("\"" (["“" "”"]) (["\""]))
	(":" (["："]) ([":"]))
	("\;" (["﹔"]) ([";"]))
	("?" (["？"]) (["?"]))
	("!" (["！"]) (["!"]))
	("(" (["（"]) (["("]))
	(")" (["）"]) ([")"]))
	("<" (["《" "〈"]) (["<"]))
	(">" (["》" "〉"]) ([">"]))
	("[" (["[" "【" "□"]) (["["]))
	("]" (["]" "】" "□"]) (["]"]))
	("\\" (["…"]) (["\\"]))
	("-" (["-" "—"]) (["-"])))
      "ascii, quanjiao, banjiao table.")

(defun wubi-toggle-quanjiao-banjiao ()
  "Toggle quanjiao(全角)/banjiao(半角)."
  (interactive)
  (setq wubi-quanjiaop (not wubi-quanjiaop))
  (if wubi-quanjiaop
      (message "進入全角標點模式")
    (message "進入半角標點模式"))
  (mapc
   (lambda (ascii-quajiao-banjiao)
     ;; redefine rules
     (quail-defrule-internal
      (car ascii-quajiao-banjiao)
      (if wubi-quanjiaop
	  (cadr ascii-quajiao-banjiao)
	(caddr ascii-quajiao-banjiao))
      (quail-map))
     ;; install new keys
     (quail-lookup-key (car ascii-quajiao-banjiao)))
   wubi-ascii-quanjiao-banjiao-table))

(provide 'wubi)

;;; wubi.el ends here
