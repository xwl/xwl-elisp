;;; salary.el --- Calculate how much we get in our pocket

;; Copyright (C) 2010  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1
;; Url: http://github.com/xwl/xwl-elisp/blob/master/salary.el

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

;;; Code:

;; Ref: http://www.tekken.com.cn/m/money.html

(eval-when-compile (require 'cl))

(defgroup salary nil
  "Calculate how much we get in our pocket, taxes, insurances we paid..."
  :prefix "salary-"
  :group 'applications)

(defcustom salary-average 4037
  "北京人均工资:
  3322 RMB (2008)
  3726 RMB (2009)
  4037 RMB (2010)"
  :group 'salary)

(defcustom salary-tax-base 2000
  "个税起征点."
  :group 'salary)

(defcustom salary-pension-insurance-rate 0.08
  "养老保险百分比。"
  :group 'salary)

(defcustom salary-medical-insurance-rate 0.02
  "医疗保险百分比(每年4月更新)，另有额外 3 块钱 `salary-medical-insurance-extra'。"
  :group 'salary)

(defconst salary-medical-insurance-extra 3)

(defcustom salary-unemployment-insurance-rate 0.005
  "失业保险百分比。"
  :group 'salary)

(defcustom salary-housing-fund-rate 0.12
  "公积金百分比(每年7月更新)。"
  :group 'salary)

(defun salary-insurances (salary)
  (let* ((cap (* salary-average 3))
         ;; (round (+ (* (min salary cap) (+ 0.08 0.02 0.005 0.12)) 3))))
         (items
          (append (mapcar (lambda (i) (* (min salary cap) i))
                          (list
                           salary-pension-insurance-rate
                           salary-medical-insurance-rate
                           salary-unemployment-insurance-rate
                           salary-housing-fund-rate))
                  (list salary-medical-insurance-extra))))
    (list (round (apply '+ items))
          (mapcar* 'cons
                   (append
                    (mapcar* (lambda (fmt value) (format fmt (* value 100)))
                             '("养老 %.1f%%" "医疗 %.1f%%" "失业 %.1f%%" "公积金 %.1f%%")
                             (list
                              salary-pension-insurance-rate
                              salary-medical-insurance-rate
                              salary-unemployment-insurance-rate
                              salary-housing-fund-rate))
                    (list (format "医疗额外 %d 元" salary-medical-insurance-extra)))
                   items))))

(defconst salary-tax-table
  ;; from  to     rate 扣除额?
  '((0     500    0.10 0)               ; (0, 500]
    (500   2000   0.10 25)
    (2000  5000   0.15 125)
    (5000  20000  0.20 375)
    (20000 40000  0.25 1375)
    (40000 60000  0.30 3375)
    (60000 80000  0.35 6375)
    (80000 100000 0.40 10375)

    (10000 0.45 15375)))

(defun salary-tax (salary)
  (let ((table salary-tax-table)
        (i nil)
        (exceeded
         (- salary salary-tax-base (car (salary-insurances salary))))
        (rate 0)
        (to-substract 0))
    (while table
      (setq i (car table))
      (setq table (cdr table))
      (if (= (list-length i) 3)
          (when (> exceeded (car i))
            (setq rate (nth 1 i)
                  to-substract (nth 2 i))
            (setq table nil))
        (let ((low (nth 0 i))
              (high (nth 1 i)))
          (when (and (> exceeded low) (<= exceeded high))
            (setq rate (nth 2 i)
                  to-substract (nth 3 i))
            (setq table nil)))))
    (cons (round (- (* exceeded rate) to-substract))
          (format "%d%% base %d" (* rate 100) salary-tax-base))))

(defun salary-at-hand (salary)
  "除掉个税、保险后真正到手的钱。"
  (- salary (car (salary-insurances salary)) (car (salary-tax salary))))

;;;###autoload
(defun salary-show (salary)
  "Calculate how much we finally get in our pocket.
SALARY is before tax.  With prefix argument, insert result at point."
  (interactive "n税前：")
  (let ((s (format "税前(%d) - 个税%S - 四险一金%S = 最后到手(%d)"
                   salary
                   (salary-tax salary)
                   (salary-insurances salary)
                   (salary-at-hand salary))))
    (if current-prefix-arg
        (insert s)
      (message "%s" s))))

(provide 'salary)

;;; salary.el ends here
