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

;; Theme设置
(add-to-list 'custom-theme-load-path
             (file-name-directory
              (locate-library "cyberpunk-theme.el")))
;; (load-theme 'cyberpunk)

;; Charset设置
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'gbk-dos)
  (set-next-selection-coding-system 'gbk-dos)
  (set-clipboard-coding-system 'gbk-dos))

;; 默认显示菜单栏
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 高亮配对的括号
(show-paren-mode 1)

;; 使用空格缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; 默认不显示 *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              '("\\*Async Shell Command\\*.*"  display-buffer-no-window nil))


;; 保存文件之前，删除无用的空格
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Basic keybinding
(global-unset-key (kbd "C-x C-x"))
(global-set-key (kbd "C-x <SPC>") 'set-mark-command)
(global-set-key (kbd "C-x C-x <SPC>") 'rectangle-mark-mode)
(global-set-key (kbd "C-x C-x C-x") 'exchange-point-and-mark)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x c") 'eshell)


;;;###autoload(require 'eh-basic)
(provide 'eh-basic)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
