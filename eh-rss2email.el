;;; eh-rss2email.el --- Run Rss2email from Emacs

;; Copyright (c) 2011-2014, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.6
;; Keywords: gnus rss2email

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

(setq eh-rss2email-buffer-name "*eh-rss2email*")
(setq eh-rss2email-command "export PYTHONPATH=~/project/emacs-packages/emacs-helper/doc/configs; r2e -V run")
(defvar eh-rss2email-timer nil)

;;;###autoload
(defun eh-rss2email ()
  "Start Rss2email."
  (interactive)
  (let* ((buffer (get-buffer-create eh-rss2email-buffer-name)))
    (if (get-buffer-process buffer)
        (message "rss2email is running")
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (remove-overlays)
          (erase-buffer)
          (insert "###### rss2email verbose log #####\n")))
      (start-process-shell-command
       "rss2email"
       buffer
       eh-rss2email-command))))

(defun eh-rss2email-cron ()
  (interactive)
  (when eh-rss2email-timer
    (cancel-timer eh-rss2email-timer))
  (setq eh-rss2email-timer
        (run-with-timer
         nil (* 120 60)
         '(lambda ()
            (message "Download rss with eh-rss2email ...")
            (eh-rss2email)))))

(provide 'eh-rss2email)

;;; eh-rss2email.el ends here
