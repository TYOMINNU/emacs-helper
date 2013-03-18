;;; eh-complete.el --- Tumashu's emacs complete configuation

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.1

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


(require 'auto-complete)
(require 'auto-complete-config)

;; 打开auto-complete-mode模式
(global-auto-complete-mode 1)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'message-mode)
;; 不要自动激活，用快捷键激活。
(setq ac-auto-start nil)
(setq ac-dwim t)
;; 设置菜单长度
(setq ac-candidate-menu-height 20)
(setq ac-candidate-max ac-candidate-menu-height)
(setq ac-use-menu-map t)
(setq-default ac-sources '(ac-source-words-in-all-buffer
			   ac-source-filename
			   ac-source-functions
			   ac-source-variables
			   ac-source-symbols
			   ac-source-features
			   ac-source-abbrev
			   ac-source-words-in-same-mode-buffers
			   ac-source-dictionary
			   ))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-files-in-current-dir
			       ac-source-words-in-buffer
			       ac-source-abbrev
			       ac-source-symbols))))
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-files-in-current-dir
			       ac-source-words-in-buffer
			       ac-source-abbrev
			       ac-source-symbols))))

(add-hook 'text-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-abbrev
			       ac-source-words-in-buffer
			       ac-source-words-in-all-buffer
			       ac-source-imenu
			       ac-source-symbols))))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
	try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
        try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))


(require 'yasnippet)

(add-to-list 'yas/root-directory (file-name-as-directory 
                                  (concat  (file-name-directory
                                            (locate-library "eh-complete.el")) "snippets")))
(mapc 'yas/load-directory yas/root-directory)
(yas/global-mode 1)
(setq yas/trigger-key nil )

;; 使org-contact,gnus,yasnippet在一起时正常工作.
(defun org-contacts-yas/fallback-behavior ()
    (setq yas/fallback-behavior '(apply completion-at-point)))

(add-hook 'message-mode-hook 'org-contacts-yas/fallback-behavior)


;; completion keybindings
(global-set-key (kbd "M-i") 'yas/expand)
(global-set-key (kbd "M-/") 'auto-complete)
;; (global-set-key (kbd "M-i") 'hippie-expand)
(define-key ac-menu-map (kbd "M-i") 'ac-complete)
(define-key ac-menu-map (kbd "M-n") 'ac-next)
(define-key ac-menu-map (kbd "M-p")'ac-previous)

;;;autoload(require 'eh-complete)
(provide 'eh-complete)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-complete.el ends here


