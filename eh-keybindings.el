;;; eh-keybindings.el --- Tumashu's  emacs keybindings

;; Copyright (c) 2011, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.1
;; Package-Requires: ((eh-functions "0.0.1")(starter-kit-bindings "2.0.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的emacs快捷键设置，emacs中文用户可以参考。

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
;; mark相关命令的键盘绑定
(global-unset-key (kbd "C-x C-x"))
(global-set-key (kbd "C-x <SPC>") 'set-mark-command)
(global-set-key (kbd "C-x C-x <SPC>") 'rectangle-mark-mode)
(global-set-key (kbd "C-x C-x C-x") 'exchange-point-and-mark)

;; 设置键盘绑定,使用Ctrl-x b激活ibuffer函数
(global-set-key (kbd "C-x b") 'ibuffer)

;; 设置键盘绑定,使用Ctrl-x k删除当前缓存
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; ido
(global-set-key (kbd "C-x C-b") 'ido-display-buffer)

;; 放大字体，缩小字体
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; magit status
(global-set-key (kbd "C-c g") 'magit-status)

;; 设置键盘绑定,发送邮件
(global-set-key (kbd "C-x m") 'gnus-msg-mail)

;; ebib 相关快捷键
(global-set-key (kbd "C-c b") 'eh-ebib)

;; org-mode 相关快捷键
(global-set-key (kbd "<f1>") '(lambda () (interactive) (progn (org-agenda nil "m") (org-agenda nil "l"))))
(global-set-key (kbd "<f2>") '(lambda () (interactive) (org-agenda nil "s")))
(global-set-key (kbd "<f3>") '(lambda () (interactive) (org-agenda nil "p")))
(global-set-key (kbd "<f4>") '(lambda () (interactive) (org-agenda nil "t")))

(global-set-key (kbd "<f7>") 'org-screenshot-take)
(global-set-key (kbd "<f8>") 'org-capture)
(global-set-key (kbd "<f9>") 'org-store-link)
(global-set-key (kbd "<f10>") 'eh-org-fill-paragraph)
(global-set-key (kbd "<f11>") 'org-contacts)
(global-set-key (kbd "<f12>") 'eh-org-mobile-sync-with-adb)

;; (global-set-key (kbd "<f12>") 
;;                 '(lambda () 
;;                    (interactive)
;;                    (progn (org-mobile-push)
;;                           (org-contacts-export-as-vcard)
;;                           ;;  (shell-command "scp ~/Documents/org-mobile/* root@192.168.1.234:/sdcard/org-mobile/")
;;                           ;;  (start-process "org-mobile-push with scp" 
;;                           ;;  (get-buffer-create "*scp-to-android*")              
;;                           ;;  "/bin/bash"
;;                           ;;  "-c" "scp ~/documents/org-mobile/* root@192.168.1.234:/sdcard/org-mobile/")
;;                                                           )))


;;;###autoload(require 'eh-keybindings)
(provide 'eh-keybindings)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-keybindings.el ends here
