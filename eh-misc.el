;;; eh-misc.el --- Tumashu's emacs configuation

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
;; 在emacs中使用ibus
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
;; Change cursor color depending on IBus status
(setq ibus-cursor-color "red")
;; daemon模式下使用ibus
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
                  (or ibus-mode (ibus-mode-on))))))

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; recentf
(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x f") 'recentf-open-files)

;; wdired
(require 'wdired)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-x m") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'multiple-cursors-mode-enabled-hook
          (lambda()
	    (require 'phi-search)
            (local-set-key (kbd "C-s") 'phi-search)
            (local-set-key (kbd "C-r") 'phi-search)))

;; ace-jump
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-j") 'ace-jump-mode)

;; expand-region
(require 'expand-region)
(define-key global-map (kbd "C-c =") 'er/expand-region)

;; browse-kill-ring
(require' browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-separator (concat "\n" (make-string 70 ?=) "\n"))
(add-hook 'browse-kill-ring-hook 'eh-browse-kill-ring-settings)

(defun eh-browse-kill-ring-settings ()
  (interactive)
  (setq browse-kill-ring-show-preview nil)
  (define-key browse-kill-ring-mode-map (kbd "C-c C-k") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "C-k") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "k") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "C-/") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "C-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "C-p") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "C-c C-c") 'browse-kill-ring-insert-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert-and-quit))

(defun eh-browse-kill-ring ()
  (interactive)
  (let ((clipboard-output (x-get-clipboard)))
    (unless 
	(string= (car kill-ring) clipboard-output)
      (kill-new clipboard-output))
    (if (car kill-ring)
	(browse-kill-ring)
      (message "kill ring is empty"))))

(global-set-key (kbd "C-c y") 'eh-browse-kill-ring)

;; General project support
(require 'projectile)
(require 'wgrep)
(projectile-global-mode)
(setq projectile-enable-caching nil)
(global-set-key (kbd "C-x F") 'projectile-find-file)
(global-set-key (kbd "C-S-s") 'projectile-grep)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-c /") 'undo-tree-visualize)
(add-hook 'undo-tree-visualizer-mode-hook 'eh-undo-tree-visualizer-settings)
(defun eh-undo-tree-visualizer-settings ()
  (interactive)
  (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-k") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-abort))

;; 查字典
(global-set-key (kbd "C-c d") 'kid-sdcv-to-buffer)
(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((string (if mark-active
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (current-word nil t))))
    (setq old-buffer (current-buffer))
    (setq old-point (point))
    (setq word
          (if string string
            (read-string
             (format "查字典 (默认 %s): " string)
             nil nil nil)))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
    ;; 在没有创建 *sdcv* windows 之前检查是否有分屏(是否为一个window)
    ;; 缺憾就是不能自动开出一个小屏幕，自己注销
    (if (null (cdr (window-list)))
        (setq onewindow t)
      (setq onewindow nil))
    (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n" word)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (goto-char 1)
             (while (search-forward-regexp "\\(-->.*\\)" nil t)
               (replace-match "" t nil))
             (goto-char 1)
             (while (search-forward-regexp "\\(^int\\)\. *\\|\\(^n\\)\. *\\|\\(^vt\\)\. *\\|\\(^v\\)\. *\\|\\(^prep\\)\. *" nil t)
               (replace-match "" nil t))
             (goto-char 1)
             (while (search-forward-regexp "\\(\\[.*\\]\\)" nil t)
               (replace-match "" nil t))
             (goto-char 1)
             (while (search-forward-regexp "\n\\{3,\\}" nil t)
               (replace-match "\n\n" nil t))
             (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
             (local-set-key (kbd "i") (lambda ()
                                         (interactive)
                                         (let ((sdcv-word 
                                                (if mark-active
                                                    (buffer-substring-no-properties 
                                                     (region-beginning)
                                                     (region-end))
                                                  (current-word nil t))))
                                           (set-buffer (get-buffer-create old-buffer))
                                           (goto-char old-point)
                                           (insert (concat "(" sdcv-word ")")))))
             (local-set-key (kbd "n") 'next-line)
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "p") 'previous-line)
             (local-set-key (kbd "k") 'previous-line)
             (local-set-key (kbd "SPC") 'scroll-up)
             (local-set-key (kbd "DEL") 'scroll-down)
             (local-set-key (kbd "q") (lambda ()
                                        (interactive)
                                        (if (eq onewindow t)
                                            (delete-window)
                                          (progn (bury-buffer) (other-window 1))))))
           (goto-char (point-min))))))))

;;;autoload(require 'eh-misc)
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-misc.el ends here











