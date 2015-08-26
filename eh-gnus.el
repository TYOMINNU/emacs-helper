;;; eh-gnus.el --- Tumashu's gnus configuation file

;; Copyright (c) 2011-2015, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.6
;; Keywords: gnus

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的gnus配置文件，中文gnus用户可以参考。

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

;;; Commentary:
;;
;; 在~/.gnus.el文件中插入一行：
;;
;; ```
;; (require 'eh-gnus)
;; (eh-gnus-load)
;; ```

;;; Code:
(defcustom eh-gnus-personal-file "~/Gnus/eh-gnus-personal.el"
  "eh-gnus用于存储个人帐号信息的文件")

(defun eh-gnus-load ()
  (interactive)
  (if (file-exists-p (expand-file-name eh-gnus-personal-file))
      (progn
        ;; 加载个人帐号信息。
        (load eh-gnus-personal-file)
        ;; bbdb
        (use-package eh-bbdb3
          :ensure nil)
        ;; 加载 gnus 可共享的配置
        (use-package eh-gnus-common
          :ensure nil)
        ;; rss2email同步脚本
        (use-package eh-rss2email
          :ensure nil
          :config
          (eh-rss2email-cron)))
    (message "eh-gnus个人帐号文件不存在，eh-gnus启动失败!!!")))

(global-set-key (kbd "C-x m")
                '(lambda ()
                   (interactive)
                   (unless (gnus-alive-p)
                     (gnus))
                   (gnus-msg-mail)))

(provide 'eh-gnus)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-gnus.el ends here
