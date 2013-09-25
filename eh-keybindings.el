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
;; require
(require 'vi)

;; 设置键盘绑定,使用Ctrl-x <SPC>设置mark
(global-set-key (kbd "C-x <SPC>") 'set-mark-command)

;; 设置键盘绑定,使用Ctrl-x b激活ibuffer函数
(global-set-key (kbd "C-x b") 'ibuffer)

;; 设置键盘绑定,使用Ctrl-x k删除当前缓存
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; 设置键盘绑定,使用Ctrl-c k激活browese-kill-ring函数
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; ido
(global-set-key (kbd "C-x C-b") 'ido-display-buffer)

;; 放大字体，缩小字体
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; 设置键盘绑定,使用Meta-w激活eh-kill-ring-save
;; 设置键盘绑定,使用Ctrl-w激活eh-kill-region
(global-set-key (kbd "M-w") 'eh-kill-ring-save)
(global-set-key (kbd "C-w") 'eh-kill-region)

;; magit status
(global-set-key (kbd "C-c g") 'magit-status)

;; 设置键盘绑定,使用Ctrl-;快速切换窗口
(global-set-key (kbd "C-;") 'other-window)

;; 设置键盘绑定,发送邮件
(global-set-key (kbd "C-x m") 'gnus-msg-mail)

;; 设置键盘绑定,在当前行上下插入空白行
(global-set-key (kbd "C-S-<return>") 'vi-open-above)
(global-set-key (kbd "S-<return>") 'vi-open-below)
(global-set-key (kbd "M-O") 'vi-open-above)
(global-set-key (kbd "M-o") 'vi-open-below)

;; org-mode 相关快捷键
(global-set-key (kbd "<f1>") '(lambda () (interactive) (progn (org-agenda nil "m") (org-agenda nil "l"))))
(global-set-key (kbd "<f2>") '(lambda () (interactive) (org-agenda nil "s")))
(global-set-key (kbd "<f3>") '(lambda () (interactive) (org-agenda nil "p")))
(global-set-key (kbd "<f4>") '(lambda () (interactive) (org-agenda nil "t")))

(global-set-key (kbd "<f7>") 'org-screenshot-take)
(global-set-key (kbd "<f8>") 'org-capture)
(global-set-key (kbd "<f9>") 'org-store-link)
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


(define-key global-map "\C-ct"
  (lambda () (interactive) (org-capture nil "t")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))
(define-key global-map "\C-cs"
  (lambda () (interactive) (org-capture nil "s")))
(define-key global-map "\C-cl"
  (lambda () (interactive) (org-capture nil "l")))
(define-key global-map "\C-cw"
  (lambda () (interactive) (org-capture nil "w")))
(define-key global-map "\C-cx"
  (lambda () (interactive) (org-capture nil "e")))
(define-key global-map "\C-cv"
  (lambda () (interactive) (org-capture nil "v")))
(define-key global-map "\C-cc"
  (lambda () (interactive) (org-capture nil "c")))
(define-key global-map "\C-cm"
  (lambda () (interactive) (org-capture nil "m")))


;;;###autoload(require 'eh-keybindings)
(provide 'eh-keybindings)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-keybindings.el ends here
