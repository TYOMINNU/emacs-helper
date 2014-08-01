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
(menu-bar-mode 1)
(tool-bar-mode -1)

;; 高亮配对的括号
(show-paren-mode 1)

(defvar eh-default-fonts-list '("PragmataPro 15" "文泉驿等宽微米黑")
  "Emacs的设计里，假设一个双字节字符和两个半角英文字符是等宽的。
所有的表格对齐之类的问题，无论table-insert还是org-mode都是基于这个假设之上。
可是实际上，这个假设不成立(字体问题)。我们一般找一对中英字体搭配使用，
从而实现中英文对齐。

下面四对字体组合可以实现中英文对齐:
   '(\"M+ 1m 12\" \"文泉驿等宽微米黑 12\")
   '(\"Dejavu Sans Mono 10\"  \"文泉驿等宽微米黑 12\")
   '(\"Consolas 11\"  \"微软雅黑 16\")
   '(\"Liberation Mono 12\"  \"文泉驿等宽微米黑 15\")
   '(\"M+ 1m 12\" \"文泉驿等宽微米黑 12\")

如果当前运行的emacs已经应用了CJK等宽补丁, 那么只需要设置
英文等宽字体的字号，中文字号不需要设置。(选择一个字母宽度较
小的英文等宽字体,否则中文显示会非常散)，比如：
   '(\"PragmataPro 16\" \"文泉驿等宽微米黑\")

注1: 文泉驿等宽正黑12号字体本身可以实现中英文对齐，
     不过显示效果没有文泉驿等宽微米黑好
注2: WindowXP用户可以安装MacType软件，字体渲染效果较好！
注3: Debian 6字体显示效果很差，可以安装infinality补丁，
     显示效果有明显的提高。")

;; 设置字体的函数
(defun eh-default-font ()
  (interactive)
  (let ((default-font-name (car eh-default-fonts-list))
	(cjk-font-name (car (cdr eh-default-fonts-list))))
    (when window-system
      (when default-font-name (set-frame-font default-font-name))
      (when cjk-font-name
	(dolist (charset '(kana han symbol cjk-misc bopomofo unicode))
	  (set-fontset-font t charset cjk-font-name))))))

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
