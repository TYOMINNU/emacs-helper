;;; eh-basic.el --- Tumashu's basic emacs configuation 

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.3
;; Package-Requires: ((starter-kit "2.0.2"))

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

;; Charset设置
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; 默认显示菜单栏
(menu-bar-mode t)

;; ;; Emacs中英文字体混排对齐问题
;; Emacs的设计里，假设一个双字节字符和两个半角英文字符是等宽的。所有的表格对齐之类的问题，
;; 无论table-insert还是org-mode都是基于这个假设之上。可是实际上，这个假设不成立。
;; 这又是字体的问题，我们如何使一个中文字符的宽度与两个英文字符的宽度相等？

;; 1. 文泉驿等宽正黑12号字体可以实现中英文对齐，但显示效果没有微米黑好看！
;; 2. 一位大牛编写了一个可以实现中英文对齐的补丁，但是现在似乎不维护了！
;; 3. 找出一对中英字体搭配（固定一个试另一个）,已知的字体对有:
;; 　　{"Dejavu Sans Mono 10" 与 "WenQuanyi Micro Hei Mono 12"}
;; 　　{"Consolas 11" 与 "Microsoft Yahei 16"}
;; 　　{"Liberation Mono 12" 与 "WenQuanYi Micro Hei Mono 15"}

;; ;;这句指定默认英文字体为Dejavu Sans Mono，大小10
;; (set-frame-font "Dejavu Sans Mono 10")

;; ;;前面一串“(if...lambda...(with-select-frame frame ())...)"是个很好的
;; ;;函数框架，意思是frame创建后载入，用这个框架可以解决--daemon启动的问题
;; ;;只有set-frame-font一句指定修改字符集'unicode的字体为文泉驿等宽微米黑，大小为12
;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (set-fontset-font "fontset-default" 
;; 				    'unicode "WenQuanyi Micro Hei Mono 12"))))
;;   (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12"))



;; 设置字体的函数
(defun eh-default-font ()
  (interactive)
  (when window-system
    (set-frame-font "M+ 1m:style=Regular")
    (set-fontset-font (frame-parameter nil 'font)
                      'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))))

;; 设置默认字体
(eh-default-font)
;; 使用daemon模式时的字体设置
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
                  (eh-default-font)))))

;;;###autoload(require 'eh-basic)

(provide 'eh-basic)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
