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

;; Completion for M-x
(require 'smex)
(smex-initialize)
(global-set-key "\M-x" 'smex)

;; IDO & filecache: smart file name completion
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido) ;; Improved flex matching
(require 'ido-vertical-mode) ;; Vertical completion menu

(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".org" ".R" ".el" ".java" ".js" ".el" ".xml")
      ido-use-filename-at-point 'guess
      ido-use-faces nil
      flx-ido-use-faces t)

(ido-mode 1)
(ido-ubiquitous)
(flx-ido-mode 1)
(ido-vertical-mode)

;; ido keybindings
(add-hook 'ido-setup-hook 'eh-ido-keybinding)
(defun eh-ido-keybinding ()
   (define-key ido-completion-map (kbd "C-SPC") nil)
   (define-key ido-completion-map (kbd "C-@") nil)
   (define-key ido-completion-map (kbd "C-i") 'ido-edit-input)
   (define-key ido-completion-map (kbd "C-l") 'ido-delete-backward-updir))


;; 打开auto-complete-mode模式
(require 'auto-complete)
(require 'auto-complete-config)


(global-auto-complete-mode 1)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'message-mode)

(setq ac-auto-show-menu t)
(setq ac-auto-start 3)
(setq ac-dwim t)

;; 设置菜单长度
(setq ac-menu-height 5)
(setq ac-use-menu-map t)
(setq-default ac-sources '(ac-source-words-in-all-buffer
			   ac-source-filename
			   ac-source-abbrev
			   ac-source-words-in-same-mode-buffers
			   ac-source-dictionary))

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
			       ac-source-dictionary
			       ac-source-functions
			       ac-source-variables
			       ac-source-symbols
			       ac-source-features
			       ac-source-symbols))))

;; hippie-expand
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
(add-to-list 'yas-snippet-dirs (file-name-as-directory 
                                (concat  (file-name-directory
                                          (locate-library "eh-complete.el")) "snippets")))
(yas-reload-all)
(yas-global-mode 1)
(setq yas-trigger-key nil )


;; completion keybindings
(global-set-key (kbd "M-i") 'yas-expand)
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


