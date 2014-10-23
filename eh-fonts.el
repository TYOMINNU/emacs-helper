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
(setq eh-fonts-alist
      '(("PragmataPro" "Monaco" "Consolas" "DejaVu Sans Mono" "Monospace" "Courier New")
	("文泉驿等宽微米黑" "Microsoft Yahei" "Microsoft_Yahei" "微软雅黑"  "黑体" "新宋体" "宋体")
	("Courier New")
	("Courier New")
	("Courier New")
	("Courier New")
	("Courier New")
	("Courier New")))

(defvar eh-english-font-size nil)
(defvar eh-english-font-size-steps '(9 10.5 11.5 12.5 14 16 18 20 22))

;; 调整系数中文字体的大小，从而实现中英文等宽。
;; 调整时，可以使用`eh-test-font-scale-at-point'
;; 来查看设置是否正确。
(setq eh-fonts-size-scale-alist
      '((1.05 1.05 1.10 1.10 1.10 1.05 1.00 0.98 0.85)
	(1.05 1.05 1.10 1.10 1.10 1.05 1.00 0.98 0.85)
	(1.05 1.05 1.10 1.10 1.10 1.05 1.00 0.98 0.85)))

(defun eh-get-font-size-scale (&optional size)
  (let* ((scale-list
	  (cond ((string-equal system-type "darwin")
		 (nth 0 eh-fonts-size-scale-alist))
		((string-equal system-type "windows-nt")
		 (nth 1 eh-fonts-size-scale-alist))
		(t (nth 2 eh-fonts-size-scale-alist))))
	 (index (position size eh-english-font-size-steps)))
    (or (nth index scale-list) 1)))

(defun eh-test-font-scale-at-point ()
  "Test scale list at point, which is usd to write font scale list"
  (interactive)
  (let* ((buffer-name "*Show-font-effect*")
	 (scale (sexp-at-point))
	 (index
	  (save-excursion
	    (let* ((point1 (point))
		   (point2 (progn (search-backward "(")
				  (point))))
	      (length (split-string
		       (buffer-substring-no-properties point1 point2)
		       " ")))))
	 (size (nth (1- index) eh-english-font-size-steps)))
    (eh-set-font size scale)
    (eh-show-font-effect size scale)))

(defun eh-show-font-effect (&optional size scale)
  "show font and its size in a new buffer"
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (set-buffer buffer-name)
    (when size
      (insert (format "# 英文字体大小设置为: %s\n" size)))
    (when scale
      (insert (format "# 中文字体调整系数(scale)设置为: %s\n" scale)))
    (insert
     (replace-regexp-in-string
      "\\^"  "\\\\"
      (replace-regexp-in-string
       "@@"  "   "
       "
# 请看下面中文和英文能否对齐.

+----------------------------------------+
| 一二三四五六七八九十九八七六五四三二一 |
| abcdefghijklmnopqrstuvwxyz,.1234567890 |
| ABCDEFGHIJKLMNOPQRSTUVWXYZ,.!@#$%^&*() |
+----------------------------------------+

  |^_/|            (^_/)
 / @ @ \\  @@     (='.'=)
( > º < )         (0)_(0)
`>>x<<´
/  O  \\  @@
")))))

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
  (setq eh-english-font-size english-font-size)
  (setq face-font-rescale-alist
	(mapcar (lambda (x)
		  (cons x chinese-fonts-scale))
		(nth 0 eh-fonts-alist)))
  (eh-set-font-internal english-font-size (or chinese-fonts-scale 1.2)))

(defun eh-set-font-internal (english-font-size &optional chinese-fonts-scale)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (let* ((english-main-font
	  (eh-make-font-string (find-if #'eh-font-exists-p (nth 0 eh-fonts-alist))
			       english-font-size))
	 (chinese-main-font
	  (font-spec :family (find-if #'eh-font-exists-p (nth 1 eh-fonts-alist))))
	 (english-italic-font
	  (font-spec :family (find-if #'eh-font-exists-p (nth 2 eh-fonts-alist))))
	 (english-bold-italic-font
	  (font-spec :slant 'italic :weight 'bold :size (+ 0.0 english-font-size)
		     :family (find-if #'eh-font-exists-p (nth 4 eh-fonts-alist))))
	 (english-symbol-font
	  (font-spec :family (find-if #'eh-font-exists-p (nth 6 eh-fonts-alist)))))
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
      (eh-set-font next-size (eh-get-font-size-scale next-size))
      (eh-save-font-size-to-file next-size)
      (setq eh-english-font-size next-size)
      (message "Your font size is set to %.1f" next-size))))

(defun eh-set-font-with-saved-size ()
  (let ((font-size (eh-read-font-size-from-file)))
    (eh-set-font font-size (eh-get-font-size-scale font-size))))

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
