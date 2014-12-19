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
(require 'cl)

(setq eh-fonts-info-conf "~/.emacs.d/eh-custom-fonts-info.el")
(setq eh-default-font-size-conf "~/.emacs.d/eh-custom-font-default-size.el")

;; 默认使用正常代替斜体。
(setq eh-fonts-ignore-italic t)

;; 默认使用粗体代替粗斜体。
(setq eh-fonts-ignore-bold-italic t)

(defconst eh-font-size-steps
  '(9 10.5 11.5 12.5 14 16 18 20 22))

(defconst eh-font-size-fallback 12.5)

(defconst eh-font-scale-fallback
  '(1.05 1.05 1.10 1.10 1.10 1.05 1.00 1.05 1.05))

(defconst eh-fonts-alist-fallback
  '(("PragmataPro" "Monaco" "Consolas" "Menlof" "DejaVu Sans Mono"
     "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro"
     "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter"
     "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono"
     "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New"
     "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono"
     "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows"
     "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
    ("黑体" "文泉驿等宽微米黑" "Microsoft Yahei" "Microsoft_Yahei" "微软雅黑"
     "Hiragino Sans GB" "文泉驿等宽正黑" "文泉驿正黑" "文泉驿点阵正黑"
     "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书"
     "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云"
     "华文新魏" "华文细黑" "华文行楷")
    ("PragmataPro" "Courier New")))

(defconst eh-font-test-string "
;; 请看下面中文和英文能否对齐.
;;                +------------------------------------------------+
;;   |^_/|        |  一二三四五六七八九十   /一二三四五六七八九十/ |      (^_/)
;;  / @ @ \\  @@  | *一二三四五六七八九十*  +一二三四五六七八九十+ |     (='.'=)
;; ( > º < )      | aaaaaaaaaaaaaaaaaaaaaa  /aaaaaaaaaaaaaaaaaaaa/ |     (0)_(0)
;;  `>>x<<´	  | *aaaaaaaaaaaaaaaaaaaa*  +aaaaaaaaaaaaaaaaaaaa+ |
;;  /  O  \\  @@  +------------------------------------------------+
")

(defun eh-font-dump-variable (variable-name value)
  "Insert a \"(setq VARIABLE value)\" in the current buffer."
  (cond ((atom value)
	 (insert (format "\n(setq %s %S)\n" variable-name value)))
	((atom (car value))
	 (insert (format "\n(setq %s\n       '%S)\n" variable-name value)))
	(t (insert (format "\n(setq %s\n      '(" variable-name))
	   (dolist (e value)
	     (insert (format "\n        %S" e)))
	   (insert "\n       ))\n"))))

;; (eh-save-fonts-info eh-fonts-alist-fallback eh-font-scale-fallback)
;; (eh-save-font-default-size 12)
;; (eh-get-font-default-size)
;; (eh-get-fonts-info)

(defun eh-save-default-font-size (size)
  "Save fonts default size to config."
  (interactive)
  (let ((variable-name "eh-custom-default-font-size"))
    (with-temp-buffer
      (erase-buffer)
      (insert ";;; Automatically generated by `eh-save-font-size'.")
      (eh-font-dump-variable variable-name size)
      (write-file (expand-file-name eh-default-font-size-conf)))))

(defun eh-get-default-font-size ()
  "Get a previously saved font size."
  (interactive)
  (let ((file (expand-file-name eh-default-font-size-conf)))
    (when (file-readable-p file)
      (load-file file))
    (if (boundp 'eh-custom-default-font-size)
	eh-custom-default-font-size
      eh-font-size-fallback)))

(defun eh-save-fonts-info (fonts-names fonts-scales)
  "Save fonts names and scales to config."
  (interactive)
  (let ((variable-fonts-names "eh-custom-fonts-names-alist")
	(variable-fonts-scales "eh-custom-fonts-scales-list"))
    (with-temp-buffer
      (erase-buffer)
      (insert ";;; 设置默认字体列表，按`C-c C-c'测试字体显示效果")
      (eh-font-dump-variable variable-fonts-names  fonts-names)
      (insert (format "\n;;; 为每个字号%s设置中文调整系数，使中英文等宽度。" eh-font-size-steps))
      (eh-font-dump-variable variable-fonts-scales fonts-scales)
      (write-file (expand-file-name eh-fonts-info-conf)))))

(defun eh-get-fonts-info ()
  "Get fonts names and scales of previously saved"
  (interactive)
  (let ((file (expand-file-name eh-fonts-info-conf)))
    (when (file-readable-p file)
      (load-file file))
    (list (if (boundp 'eh-custom-fonts-names-alist)
	      eh-custom-fonts-names-alist
	    eh-fonts-alist-fallback)
	  (if (boundp 'eh-custom-fonts-scales-list)
	      eh-custom-fonts-scales-list
	    eh-font-scale-fallback))))

(defun eh-font-exists-p (font)
  (if (null (x-list-fonts font))
      nil t))

(defun eh-get-valid-fonts ()
  (mapcar (lambda (x)
	    (find-if #'eh-font-exists-p x))
	  (car (eh-get-fonts-info))))

(defun eh-make-font-string (font-name font-size)
  (if (and (stringp font-size)
	   (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))

(defun eh-get-scale (&optional size)
  (let* ((scale-list (car (cdr (eh-get-fonts-info))))
	 (index (or (position size eh-font-size-steps) 1)))
    (unless (file-exists-p eh-fonts-info-conf)
      (message "如果中英文不能对齐，请运行`eh-fonts-setup'设置。"))
    (or (nth index scale-list) 1)))

(defun eh-set-font (font-size &optional font-scale)
  (setq face-font-rescale-alist
	(mapcar (lambda (x)
		  (cons x (or font-scale 1.25)))
		(nth 1 (car (eh-get-fonts-info)))))
  (eh-set-font-internal font-size))

(defun eh-set-font-internal (font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (let* ((valid-fonts (eh-get-valid-fonts))
	 (english-main-font (eh-make-font-string (nth 0 valid-fonts) font-size))
	 (chinese-main-font (font-spec :family (nth 1 valid-fonts)))
	 (english-bold-font
	  (font-spec :slant 'normal :weight 'bold
		     :size font-size
		     :family (nth 0 valid-fonts)))
	 (english-italic-font
	  (font-spec :slant 'italic :weight 'normal
		     :size font-size
		     :family (nth 0 valid-fonts)))
	 (english-bold-italic-font
	  (font-spec :slant 'italic :weight 'bold
		     :size font-size
		     :family (nth 0 valid-fonts)))
	 (english-symbol-font (font-spec :family (nth 3 valid-fonts))))
    (set-face-attribute 'default nil :font english-main-font)
    (set-face-font 'italic
		   (if eh-fonts-ignore-italic
		       english-main-font
		     english-italic-font))
    (set-face-font 'bold-italic
		   (if eh-fonts-ignore-bold-italic
		       english-bold-font
		     english-bold-italic-font))
    (set-fontset-font t 'symbol english-symbol-font)
    (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

    ;; Set Chinese font and don't not use 'unicode charset,
    ;; it will cause the english font setting invalid.
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset chinese-main-font))))

(defun eh-step-frame-font-size (step)
  (let ((steps eh-font-size-steps)
	next-size)
    (when (< step 0)
      (setq steps (reverse eh-font-size-steps)))
    (setq next-size
	  (cadr (member (eh-get-default-font-size) steps)))
    (when next-size
      (eh-set-font next-size (eh-get-scale next-size))
      (eh-save-default-font-size next-size)
      (message "Your font size is set to %.1f" next-size))))

(defun eh-set-font-with-saved-size ()
  (let* ((font-size (eh-get-default-font-size))
	 (font-size-scale (eh-get-scale font-size)))
    (eh-set-font font-size font-size-scale)))

;; 正常启动emacs时设置字体
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (eh-set-font-with-saved-size))))
  (when (display-graphic-p)
    (eh-set-font-with-saved-size)))

(defun eh-font-size-decrease ()
  (interactive)
  (eh-step-frame-font-size -1))

(defun eh-font-size-increase ()
  (interactive)
  (eh-step-frame-font-size 1))

(defvar eh-font-scale-setup-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'eh-test-scale-at-point)
    (define-key keymap (kbd "C-<up>") 'eh-increment-font-scale-at-point)
    (define-key keymap (kbd "C-<down>") 'eh-decrement-font-scale-at-point)
    (define-key keymap (kbd "C-<right>") 'eh-increment-font-scale-at-point)
    (define-key keymap (kbd "C-<left>") 'eh-decrement-font-scale-at-point)
    keymap)
  "Keymap for `eh-font-scale-setup-mode', a minor mode.
Use this map to set additional keybindings for setup chinese font scale")

(define-minor-mode eh-font-scale-setup-mode
  "Minor for edit chinese font scale"
  nil " Rem" eh-font-scale-setup-mode-map)

(defun eh-fonts-setup ()
  (interactive)
  (let ((file (expand-file-name eh-fonts-info-conf)))
    (unless (file-readable-p file)
      (eh-save-fonts-info eh-fonts-alist-fallback
			  eh-font-scale-fallback))
    (find-file file)
    (eh-font-scale-setup-mode 1)
    (goto-char (point-min))))

(defun eh-test-scale-at-point ()
  "Test scale list at point, which is usd to write font scale list"
  (interactive)
  (let (scale size index)
    (setq scale (sexp-at-point))
    (if (and scale (numberp scale))
	(progn
	  (setq index
		(save-excursion
		  (let* ((point1 (point))
			 (point2 (progn (search-backward "(")
					(point))))
		    (length (split-string
			     (buffer-substring-no-properties point1 point2)
			     " ")))))
	  (setq size (nth (1- index) eh-font-size-steps))
	  (eh-set-font size scale)
	  (eh-show-font-effect size scale))
      (eh-set-font 14 1.25)
      (eh-show-font-effect 14 1.25 t))))

(defun eh-change-font-scale-at-point (step)
  (interactive)
  (skip-chars-backward "0123456789\\.")
  (or (looking-at "[0123456789.]+")
      (error "No number at point"))
  (replace-match
   (format "%.5s"
	   (number-to-string
	    (+ step (string-to-number (match-string 0))))))
  (backward-char 1)
  (eh-test-scale-at-point))

(defun eh-increment-font-scale-at-point ()
  (interactive)
  (eh-change-font-scale-at-point 0.01))

(defun eh-decrement-font-scale-at-point ()
  (interactive)
  (eh-change-font-scale-at-point -0.01))

(defun eh-show-font-effect (&optional size scale info)
  "show font and its size in a new buffer"
  (interactive)
  (let ((buffer-name "*Show-font-effect*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
	(org-mode))
      (setq truncate-lines 1)
      (when size
	(insert (format "# 英文字体大小设置为: %s ; " size)))
      (when scale
	(insert (format "中文字体调整系数(scale)设置为: %s 。\n" scale)))
      (when info
	(insert
	 (concat
	  "# 将光标移动到`eh-custom-fonts-scales-list‘中各个数字上，"
	  "C-<up> 增大 scale 的值，C-<down> 减小 scale 的值。")))
      (insert
       (replace-regexp-in-string
	"\\^"  "\\\\"
	(replace-regexp-in-string
	 "@@"  "   "
	 eh-font-test-string)))
      (when (and size scale)
	(eh-set-font size scale)))))

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-fonts.el ends here
