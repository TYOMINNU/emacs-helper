;;; eh-fonts.el --- Tumashu's emacs configuation

;; Copyright (c) 2011-2013, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((eh-basic "0.0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'cl-lib)

(setq eh-font-size-conf "~/.emacs.d/emacs-font-size.conf")
(setq eh-main-fonts
      '(("PragmataPro" "Monaco" "Consolas" "DejaVu Sans Mono" "Monospace" "Courier New")
	("Microsoft Yahei" "Microsoft_Yahei" "微软雅黑" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体")))

(setq eh-italic-fonts
      '(("Courier New") ("Courier New")))

(setq eh-bold-italic-fonts
      '(("Courier New") ("Courier New")))

(setq eh-symbol-fonts
      '(("Courier New") ("Courier New")))

(defvar eh-english-font-size nil)
(defvar eh-english-font-size-steps '(9 10.5 11.5 12.5 14 16 18 20 22))

(setq eh-chinese-font-size-scale-alist
      (cond ((string-equal system-type "darwin")
	     '((9    . 1.05) (10.5 . 1.05) (11.5 . 1.10)
	       (12.5 . 1.10) (14   . 1.10) (16   . 1.05)
	       (18   . 1.00) (20   . 1.05) (22   . 1.05)))
	    ((string-equal system-type "windows-nt")
	     '((9    . 1.05) (10.5 . 1.05) (11.5 . 1.10)
	       (12.5 . 1.10) (14   . 1.10) (16   . 1.05)
	       (18   . 1.00) (20   . 1.05) (22   . 1.05)))
	    (t
	     '((9    . 1.05) (10.5 . 1.05) (11.5 . 1.10)
	       (12.5 . 1.10) (14   . 1.10) (16   . 1.05)
	       (18   . 1.00) (20   . 1.05) (22   . 1.05)))))

;; (eh-font-size-decrease)
;; (eh-font-size-increase)
;;
;; 调整eh-chinese-font-size-scale-alist后，
;; 运行上面两个命令，看下面的文字能否对齐.
;; +----------------------------------------+
;; | 一二三四五六七八九十九八七六五四三二一 |
;; | abcdefghijklmnopqrstuvwxyz1234567890,. |
;; +----------------------------------------+

(defun eh-font-exists-p (font)
  (if (null (x-list-fonts font))
      nil t))

(defun eh-make-font-string (font-name font-size)
  (if (and (stringp font-size)
	   (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))

(defun eh-save-font-size-to-file (font-size)
  (save-excursion
    (with-current-buffer (find-file-noselect eh-font-size-conf)
      (delete-region (point-min) (point-max))
      (insert (format "%s" font-size))
      (save-buffer)
      (kill-buffer))))

(defun eh-read-font-size-from-file ()
  (if (file-exists-p eh-font-size-conf)
      (save-excursion
	(find-file eh-font-size-conf)
	(goto-char (point-min))
	(let ((font-size (read (current-buffer))))
         (kill-buffer (current-buffer))
         font-size))
    12.5))

(defun eh-set-font (english-font-size &optional chinese-fonts-scale)
  (setq face-font-rescale-alist
	(mapcar (lambda (x)
		  (cons x chinese-fonts-scale))
		(car (cdr eh-main-fonts))))
  (eh-set-font-internal english-font-size (or chinese-fonts-scale 1.2)))

(defun eh-set-font-internal (english-font-size &optional chinese-fonts-scale)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (let ((english-main-font (eh-make-font-string
			    (find-if #'eh-font-exists-p (car eh-main-fonts))
			    english-font-size))
	(chinese-main-font
	 (font-spec :family
		    (find-if #'eh-font-exists-p
			     (car (cdr eh-main-fonts)))))
	(english-italic-font
	 (font-spec :family
		    (find-if #'eh-font-exists-p
			     (car eh-italic-fonts))))
	(english-bold-italic-font
	 (font-spec :slant 'italic :weight 'bold :size (+ 0.0 english-font-size)
		    :family
		    (find-if #'eh-font-exists-p
			     (car eh-bold-italic-fonts))))
	(english-symbol-font
	 (font-spec :family
		    (find-if #'eh-font-exists-p
			     (car eh-bold-italic-fonts)))))

    (set-face-attribute 'default nil :font english-main-font)
    (set-face-font 'italic english-italic-font)
    (set-face-font 'bold-italic english-bold-italic-font)
    (set-fontset-font t 'symbol english-symbol-font)
    ;; (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

    ;; Set Chinese font and don't not use 'unicode charset,
    ;; it will cause the english font setting invalid.
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset chinese-main-font))))

(defun eh-step-frame-font-size (step)
  (let ((steps eh-english-font-size-steps)
        next-size)
    (when (< step 0)
      (setq steps (reverse eh-english-font-size-steps)))
    (setq next-size
          (cadr (member eh-english-font-size steps)))
    (when next-size
      (eh-set-font next-size (cdr (assoc next-size eh-chinese-font-size-scale-alist)))
      (eh-save-font-size-to-file next-size)
      (setq eh-english-font-size next-size)
      (message "Your font size is set to %.1f" next-size))))

(defun eh-set-font-with-saved-size ()
  (let ((font-size (eh-read-font-size-from-file)))
    (eh-set-font font-size
		 (cdr (assoc font-size eh-chinese-font-size-scale-alist)))))

;; 正常启动emacs时设置字体
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (eh-set-font-with-saved-size))))
  (eh-set-font-with-saved-size))

(defun eh-font-size-decrease ()
  (interactive)
  (eh-step-frame-font-size -1))

(defun eh-font-size-increase ()
  (interactive)
  (eh-step-frame-font-size 1))

(global-set-key (kbd "C-x M--") 'eh-font-size-decrease)
(global-set-key (kbd "C-x M-+") 'eh-font-size-increase)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-fonts.el ends here
