;;; eh-functions.el --- Tumashu's  emacs functions

;; Copyright (c) 2011-2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.2
;; Package-Requires: ((starter-kit "2.0.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的emacs函数，emacs中文用户可以参考。

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
;; require
;; (require 'starter-kit-defuns)

;; 常用函数  
(defun eh-dos2unix () 
  "将dos换行方式转换为unix的换行方式,用于去除^M"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun eh-unix2dos ()
  "将unix换行方式转换为dos的换行方式"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun eh-remove-white-space ()
  "删除多余空格"
  (interactive)
  ;; 删除行首空格
  (goto-char (point-min))
  (while (re-search-forward "^ +\\(\\cc+\\)" nil t)
    (replace-match "\\1"))
  ;; 删除汉字之间的空格
  (goto-char (point-min))
  (while (re-search-forward "\\(\\cc+\\) +" nil t)
    (replace-match "\\1")))

(defun eh-utf8-language-environment ()
  "设置utf-8语言环境"
  (interactive)
  (set-language-environment "UTF-8")
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-next-selection-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix))

(defun eh-gbk-language-environment ()
  "设置gbk语言环境"
  (interactive)
  (set-language-environment "Chinese-GBK")
  (set-buffer-file-coding-system 'gbk-dos)
  (set-clipboard-coding-system 'gbk-dos)
  (set-file-name-coding-system 'gbk-dos)
  (set-keyboard-coding-system 'gbk-dos)
  (set-next-selection-coding-system 'gbk-dos)
  (set-selection-coding-system 'gbk-dos)
  (set-terminal-coding-system 'gbk-dos))

;;;###autoload(require 'eh-functions)

(provide 'eh-functions)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-functions.el ends here







