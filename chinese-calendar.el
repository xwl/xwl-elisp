;;; chinese-calendar.el --- calendar for chinese

;; Copyright (C) 2004 Charles Wang
;; Copyright (C) 2005, 2006 William Xu

;; Authors: Charles Wang <charleswang@peoplemail.com.cn>
;; Authors: William Xu <william.xwl@gmail.com>

;; $Id: chinese-calendar.el, v 0.2 2006/05/11 15:14:02 xwl Exp $
;; Keywords: calendar, i18n

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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'chinese-calendar)

;;; Code:

(require 'calendar)
(require 'cal-china)

(setq chinese-calendar-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "已" "庚" "辛" "壬" "癸"])

(setq chinese-calendar-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(setq chinese-number
  ["一" "二" "三" "四" "五" "六" "七" "八" "九" "十"])

(setq solar-n-hemi-seasons
  '( "春分" "夏至" "秋分"  "冬至"))

(setq solar-s-hemi-seasons
  '("秋分" "夏至" "春分" "冬至"))

(setq wcy-chinese-month-name
  ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "腊月"])

(setq wcy-chinese-day-name
  ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"  "廿"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
   "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])



(add-hook 'calendar-load-hook 'wcy-calendar-remapkey)

(defun wcy-calendar-move-hook ()
  (wcy-calender-set-mode-line))

(add-hook 'calendar-move-hook 'wcy-calendar-move-hook)

(defun wcy-calendar-init-windows-hook()
  (wcy-calender-set-mode-line))

(add-hook 'initial-calendar-window-hook
	  'wcy-calendar-init-windows-hook)

(defun wcy-chinese-day-name (date)
  (let ((day (calendar-day-of-week date)))
    (concat "星期" (if (eq day 0) "日"
		     (aref chinese-number (1- day))))))

(defun wcy-calendar-display-form (date)
  (let* ((dayname (wcy-chinese-day-name date))
         (day     (extract-calendar-day date))
         (month   (extract-calendar-month date))
         (year    (extract-calendar-year date)))
    (format "%4d年%2d月%2d日 %s" year month day dayname)))

(setq chinese-date-diary-pattern
      '((year " *年" month " *月" day "日 *[^/年0-9]")
	(year "-" month "-" day "[^0-9]")
	(day "/" month "[^/0-9]")
	(day "/" month "/" year "[^0-9]")
	(backup day " *" monthname "\\W+\\<\\([^*0-9]\\|\\([0-9]+[:aApP]\\)\\)")
	(day " *" monthname " *" year "[^0-9]")
	(dayname "\\W")))

(setq calendar-date-display-form '((wcy-calendar-display-form date))
      diary-date-forms chinese-date-diary-pattern)

(defun wcy-count-chinese-character (string)
  (length (remq nil
                (mapcar 'multibyte-string-p
                        (mapcar 'char-to-string string)))))

(defun wcy-chinese-date-string (date)
  (let* ((a-date (calendar-absolute-from-gregorian date))
         (c-date (calendar-chinese-from-absolute a-date))
         (cycle (car c-date))
         (year (car (cdr c-date)))
         (month (car (cdr (cdr c-date))))
         (day (car (cdr (cdr (cdr c-date)))))
         (this-month (calendar-absolute-from-chinese
                      (list cycle year month 1)))
         (next-month (calendar-absolute-from-chinese
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            (calendar-mod (1+ (floor month)) 12)
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
    (format "农历%s年%s%s%s"
            ;;cycle
            ;;year
            (calendar-chinese-sexagesimal-name year)
            (if (not (integerp month))
                "润"
              (if (< 30 (- next-month this-month))
                  ""
                ""))
            (aref wcy-chinese-month-name (1- (floor month)))
            (aref wcy-chinese-day-name (1- day)))))

(defun wcy-calender-set-mode-line ()
  (let* ((date (calendar-cursor-to-date))
         (s1 (calendar-date-string date t))
         (s2 (wcy-chinese-date-string date))
         (x (list s1 s2))
         (y (make-string (apply '+  (mapcar 'wcy-count-chinese-character x )) ? )))
    (progn
      (setq calendar-mode-line-format
            (append x (list y)))
      (update-calendar-mode-line)
      (force-mode-line-update))))

;;; The following function is copied from calendar.el or cal-china.el
;;; because they don't conform with the chinese traditional presentation.

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s%s"
          (aref chinese-calendar-celestial-stem (% (1- n) 10))
          (aref chinese-calendar-terrestrial-branch (% (1- n) 12))))

(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (let* ((blank-days;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
	 (last (calendar-last-day-of-month month year)))
   (goto-char (point-min))
   (calendar-insert-indented
    (calendar-string-spread
     (list (format "%d年%d月" year month)) ?  20)
    indent t)
   (calendar-insert-indented "" indent);; Go to proper spot
   (calendar-for-loop i from 0 to 6 do
      (insert (calendar-day-name (mod (+ calendar-week-start-day i) 7)
                                 2 t))
      (insert " "))
   (calendar-insert-indented "" 0 t);; Force onto following line
   (calendar-insert-indented "" indent);; Go to proper spot
   ;; Add blank days before the first of the month
   (calendar-for-loop i from 1 to blank-days do (insert "   "))
   ;; Put in the days of the month
   (calendar-for-loop i from 1 to last do
      (insert (format "%2d " i))
      (add-text-properties
       (- (point) 3) (1- (point))
       '(mouse-face highlight
	 help-echo "mouse-2: menu of operations for this date"))
      (and (zerop (mod (+ i blank-days) 7))
           (/= i last)
           (calendar-insert-indented "" 0 t)    ;; Force onto following line
           (calendar-insert-indented "" indent)))));; Go to proper spot

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (chinese-year y)))))))
          (if (calendar-date-is-visible-p chinese-new-year)
	      (list (list chinese-new-year
			  (format "%s年春节"
				  (calendar-chinese-sexagesimal-name (+ y 57))))))))))


;; The followings are added by William Xu <william.xwl@gmail.com>
(defun wx-birthday-from-chinese (birthday-chinese)
  "Return wx's birthday date this year in Gregorian form.

BIRTHDAY-CHINESE is a list.  e.g.  '(5 11)"
  (let* ((current-chinese-date (calendar-chinese-from-absolute
				(calendar-absolute-from-gregorian
				 (calendar-current-date))))
	 (cycle (car current-chinese-date))
	 (year (cadr current-chinese-date))

	 (birthday-chinese-full `(,cycle ,year ,@birthday-chinese))
	 (birthday-gregorian-full (calendar-gregorian-from-absolute
				   (calendar-absolute-from-chinese
				    birthday-chinese-full))))

    (setq birthday-gregorian `(,(car birthday-gregorian-full)
			       ,(cadr birthday-gregorian-full)))))

(defun holiday-chinese (cmonth cday string)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).

If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
         (y displayed-year)
         (gdate (calendar-gregorian-from-absolute
                 (+ (cadr (assoc cmonth (chinese-year y))) (1- cday))))
         (gm (car gdate))
         (gd (cadr gdate))
         (gy (caddr gdate)))
    (increment-calendar-month m y (- 11 gm))
    (if (> m 9)
	(list (list (list gm gd gy) string)))))

(setq local-holidays
      '((holiday-fixed 1  1  "元旦")
        (holiday-chinese-new-year)
        (holiday-fixed 3  8  "妇女节")
        (holiday-fixed 3  12 "植树节")
        (holiday-fixed 5  1  "劳动节")
        (holiday-fixed 5  4  "青年节")
        (holiday-fixed 6  1  "儿童节")
        (holiday-fixed 9  10 "教师节")
        (holiday-fixed 10 1  "国庆节")
	(holiday-fixed 12 25 "圣诞节")

	(holiday-chinese 1 15 "元宵节")
	(holiday-chinese 5 5 "端午节")
	(holiday-chinese 5 11 "My Birthday !")
	(holiday-chinese 9 9 "重阳节")
	(holiday-chinese 8 15 "中秋节")))

(setq calendar-holidays (append general-holidays local-holidays))

(provide 'chinese-calendar)

;;; chinese-calendar.el ends here
