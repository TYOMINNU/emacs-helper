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

;; Full name and email
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; Startup screen
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-scratch-message ";; This is *scratch* buffer.\n\n")

;; 使用空格缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; Don't delete *scratch* buffer
(defun eh-unkillable-scratch-buffer ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions 'eh-unkillable-scratch-buffer)

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(setq package-unsigned-archives '("gnu"))

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; load-path
(defvar eh-enable-load-path-hack t)

(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (delq nil (mapcar
                   #'(lambda (p)
                       (unless (or (string-match "lisp/org$" p))
                         p))
                   load-path)))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms #'(lambda (s)
                (let ((sn (symbol-name s)))
                  (when (string-match "^\\(org\\|ob\\|ox\\)-?" sn)
                    (setplist s nil)
                    (when (autoloadp s)
                      (unintern s)))))))

(let* ((output nil)
       (dirs (progn
               (dolist (x '("~" "c:" "d:" "e:" "f:" "g:"))
                 (push (file-name-as-directory
                        (concat x "/projects"))
                       output)
                 (push (file-name-as-directory
                        (concat x "/project"))
                       output))
               (reverse output))))
  (dolist (directory dirs)
    (let* ((dir (expand-file-name directory))
           (default-directory dir))
      (when eh-enable-load-path-hack
        (eh-hack-load-path))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 默认不显示 *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              '("\\*Async Shell Command\\*.*"  display-buffer-no-window nil))

;; Theme 设置
(use-package custom
  :ensure nil
  :config
  (add-to-list 'custom-theme-load-path
               (file-name-directory
                (locate-library "cyberpunk-theme.el")))
  ;; (load-theme 'cyberpunk)
  )

;; Charset 设置
(use-package mule
  :ensure nil
  :config

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
    (set-clipboard-coding-system 'gbk-dos)))

;; 保存文件之前，删除无用的空格
(use-package files
  :ensure nil
  :config
  (use-package whitespace
    :ensure nil
    :config
    (add-hook 'before-save-hook 'whitespace-cleanup))
  (use-package simple
    :ensure nil
    :config
    (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))))

;; eshell
(use-package eshell
  :bind (("C-x c" . eshell))
  :ensure nil
  :config

  (use-package em-term
    :ensure nil)
  (use-package em-unix
    :ensure nil)

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
  :config
  (setq default-input-method "chinese-pyim")
  (when (eq system-type 'windows-nt)
    (setq pyim-use-tooltip nil)
    (setq pyim-page-length 7))
  :bind
  (("C-<SPC>" . toggle-input-method)
   ("C-;" . pyim-toggle-input-ascii)
   ("C-:" . pyim-delete-word-from-personal-buffer)))

;; Chinese fonts setup
(use-package chinese-fonts-setup
  :demand t
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

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x b" . ibuffer)))

(use-package simple
  :ensure nil
  :init (global-unset-key (kbd "C-x C-x"))
  :bind
  (("C-x <SPC>" . set-mark-command)
   ("C-x C-x C-x" . exchange-point-and-mark)))

(use-package rect
  :ensure nil
  :bind (("C-x C-x <SPC>" . rectangle-mark-mode)))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1)
  :bind (("C-x k" . kill-this-buffer)))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode 1)
  :bind (("C-x k" . kill-this-buffer)))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package paren
  :ensure nil
  :config
  ;; 高亮配对的括号
  (show-paren-mode 1))


;;;###autoload(require 'eh-basic)
(provide 'eh-basic)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
