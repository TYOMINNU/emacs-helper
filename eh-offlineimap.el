;;; eh-offlineimap.el --- Run Offlineimap from Emacs

;; Copyright (c) 2011-2014, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.6
;; Keywords: gnus offlineimap

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

;;; Code:

(setq eh-offlineimap-buffer-name "*eh-offlineimap*")
(setq eh-offlineimap-command "offlineimap -u MachineUI")
(defvar eh-offlineimap-timer nil)

;;;###autoload
(defun eh-offlineimap ()
  "Start Offlineimap."
  (interactive)
  (let* ((buffer (get-buffer-create eh-offlineimap-buffer-name)))
    (if (get-buffer-process buffer)
	(message "offlineimap is running")
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
	  (remove-overlays)
	  (erase-buffer)
	  (insert "###### offlineimap verbose log #####\n")))
      (start-process-shell-command
       "offlineimap"
       buffer
       eh-offlineimap-command))))

(defun eh-offlineimap-cron ()
  "5分钟运行一次`eh-offlineimap', 延迟启动30秒, 等待gpg-agent密码输入."
  (interactive)
  (when eh-offlineimap-timer
    (cancel-timer eh-offlineimap-timer))
  (setq eh-offlineimap-timer
	(run-with-timer
	 30 (* 5 60)
	 '(lambda ()
	    (message "Sync email with eh-offlineimap ...")
	    (eh-offlineimap)))))

(provide 'eh-offlineimap)

;;; eh-offlineimap.el ends here
