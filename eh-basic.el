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
(require 'use-package)
(if (eq system-type 'windows-nt)
    (setq use-package-always-ensure t)
  (setq use-package-always-ensure nil))

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

;; eshell
(use-package eshell
  :ensure nil
  :config
  (require 'em-term)
  (require 'em-unix)
  (setq eshell-visual-commands
        (append '("aptitude" "mutt" "nano" "crontab" "vim" "less")
                eshell-visual-commands))
  (setq eshell-visual-subcommands
        (list (append '("sudo") eshell-visual-commands)
              '("git" "log" "diff" "show")))
  (setq eshell-visual-options
        '(("git" "--help")))

  (defun eh-eshell (&optional arg)
    (interactive)
    ;; 使用eshell-exec-visual第一次打开term时，
    ;; 不能使用multi-term的键盘绑定，原因不知，
    ;; 首先运行一下less, 从而让multi-term的键盘绑定生效。
    (eshell-command "less")
    (eshell arg)))

;; (setenv "PATH"
;;         (concat "/usr/local/texlive/2014/bin/i386-linux:"
;;                 (getenv "PATH")))
;; (setenv "PYTHONPATH"
;;         (concat "~/project/emacs-packages/emacs-helper/doc/configs:"
;;                 (getenv "PYTHONPATH")))
;; (setq exec-path
;;       (append '("/usr/local/texlive/2014/bin/i386-linux")
;;               exec-path))

;; eww
(use-package eww
  :ensure nil
  :config
  (setq shr-width 90)
  ;; 搜狗:  http://www.sogou.com/sogou?query=
  ;; 百度:  http://m.baidu.com/ssid=0/s?word=
  ;; 必应:  http://cn.bing.com/search?q=
  (setq eww-search-prefix "http://www.sogou.com/sogou?query="))

;; Pinyin Input Method
(use-package chinese-pyim
  :ensure t
  :config
  (setq default-input-method "chinese-pyim")
  :bind
  (("C-<SPC>" . toggle-input-method)
   ("C-;" . pyim-toggle-input-ascii)
   ("C-:" . pyim-delete-word-from-personal-buffer)))

;; Chinese fonts setup
(use-package chinese-fonts-setup
  :ensure t
  :bind (("C--" . cfs-decrease-fontsize)
         ("C-=" . cfs-increase-fontsize)
         ("C-+" . cfs-next-profile)))

;; recentf
(use-package recentf
  :ensure nil
  :bind (("C-x f" . recentf-open-files))
  :config
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1)
  (setq recentf-max-saved-items 99)
  (setq recentf-max-menu-items 99)
  (setq recentf-exclude
        '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"
          ".recentf" "emacs-font-size.conf"))
  (setq recentf-menu-filter 'eh-recentf-buffer-filter)
  (setq recentf-show-file-shortcuts-flag nil)

  (defun eh-recentf-buffer-filter (l)
    (let ((index 0)
          filtered-list element list name recentf-string)
      (dolist (elt l (nreverse filtered-list))
        (setq index (1+ index)
              element (recentf-menu-element-value elt)
              list (reverse (split-string element "/"))
              name (if (> (length (nth 0 list)) 0)
                       (format "%s" (nth 0 list))
                     (format "%s/" (nth 1 list)))
              recentf-string (format "[%2s]:  %-30s (%s)" index name element))
        (push (recentf-make-menu-element recentf-string element) filtered-list))))

  ;; 自动保存recentf文件。
  (add-hook 'find-file-hook 'recentf-save-list))

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
