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

;; elisp setting
(defun eh-elisp-setup ()
  ;; 跟踪行尾空格
  (setq show-trailing-whitespace t)
  ;; 高亮TAB
  (setq highlight-tabs t)
  ;; 自动缩进
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook
	  #'eh-elisp-setup)

;; aggressive-indent
(require 'aggressive-indent)

;; autopair
(require 'autopair)
(autopair-global-mode)

;; visual-regexp
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq multi-term-buffer-name "term")
(setq term-scroll-show-maximum-output nil)
(setq term-scroll-to-bottom-on-output nil)
(setq multi-term-dedicated-select-after-open-p t)
(setq term-bind-key-alist
      (append '(("C-c C-x" . eh-term-send-ctrl-x)
		("C-c C-h" . eh-term-send-ctrl-h))
	      term-bind-key-alist))

(defun eh-term-send-ctrl-x ()
  "Send C-x in term mode."
  (interactive)
  (term-send-raw-string "\C-x"))

(defun eh-term-send-ctrl-z ()
  "Send C-z in term mode."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun eh-term-send-ctrl-h ()
  "Send C-h in term mode."
  (interactive)
  (term-send-raw-string "\C-h"))

(defun eh-term-setup ()
  (setq truncate-lines t)
  (setq term-buffer-maximum-size 0)
  (setq show-trailing-whitespace nil)
  (multi-term-handle-close))

(remove-hook 'term-mode-hook 'eh-term-setup)
(remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
(remove-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)

(add-hook 'term-mode-hook 'eh-term-setup)
(add-hook 'term-mode-hook 'multi-term-keystroke-setup)
(add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)

;; eshell
(require 'eshell)
(require 'em-term)
(require 'em-unix)
(require 'esh-help)

(setq eshell-visual-commands
      (append '("aptitude" "mutt" "nano" "crontab" "vim" "less")
	      eshell-visual-commands))

(setq eshell-visual-subcommands
      (list (append '("sudo") eshell-visual-commands)
	    '("git" "log" "diff" "show")))

(setq eshell-visual-options
      '(("git" "--help")))

(defun eh-eshell-setup ()
  ;; (setup-esh-help-eldoc)
  ;; (eldoc-mode)
  (local-set-key (kbd "M-q") 'eshell-push-command))

(add-hook 'eshell-mode-hook 'eh-eshell-setup)

(defun eh-eshell (&optional arg)
  (interactive)
  ;; 使用eshell-exec-visual第一次打开term时，
  ;; 不能使用multi-term的键盘绑定，原因不知，
  ;; 首先运行一下less, 从而让multi-term的键盘绑定生效。
  (eshell-command "less")
  (eshell arg))

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

;; recentf
(require 'recentf)
(require 'recentf-ext)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-saved-items 99)
(setq recentf-max-menu-items 99)
(setq recentf-exclude '("/adb:" "COMMIT" "autoloads"
			"archive-contents" "eld" "newsrc"
			".recentf" "emacs-font-size.conf"))
(setq recentf-menu-filter 'eh-recentf-buffer-filter)
(setq recentf-show-file-shortcuts-flag nil)

(defun eh-recentf-buffer-filter (l)
  (let (filtered-names filtered-list  full name counters sufx (index 0))
    (dolist (elt l (nreverse filtered-list))
      (setq index (1+ index)
	    element (recentf-menu-element-value elt)
	    full (directory-file-name element)
	    directory  (file-name-directory full)
	    name (if (file-directory-p element)
		     (concat (file-name-nondirectory full) "/")
		   (file-name-nondirectory full))
	    recentf-string (format "[%2s]:  %-30s (%s)" index name (abbreviate-file-name directory)))
      (push (recentf-make-menu-element recentf-string full) filtered-list))))


;; 自动保存recentf文件。
(add-hook 'find-file-hook 'recentf-save-list)

(global-set-key (kbd "C-x f") 'recentf-open-files)

;; magit
(require 'magit)

;; wdired
(require 'wdired)

;; dired-ranger
(require 'dired-ranger)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-x m") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ace-jump
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-j") 'ace-jump-mode)

;; switch window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(setq  switch-window-shortcut-style 'qwerty)

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

;; slime and stumpwm
(require 'slime)
(setq inferior-lisp-program "sbcl")
(slime-setup)

(defun eh-stumpwm-swank-connect ()
  (interactive)
  (shell-command "stumpish swank")
  (slime-connect "127.0.0.1" "4005"))

;; calfw
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)
(require 'cal-china-x)


(setq eh-calendar-holidays
      '(;;公历节日
	(holiday-fixed 1 1 "元旦")
	(holiday-fixed 2 14 "情人节")
	(holiday-fixed 3 8 "妇女节")
	(holiday-fixed 3 14 "白色情人节")
	(holiday-fixed 4 1 "愚人节")
	(holiday-fixed 5 1 "劳动节")
	(holiday-fixed 5 4 "青年节")
	(holiday-float 5 0 2 "母亲节")
	(holiday-fixed 6 1 "儿童节")
	(holiday-float 6 0 3 "父亲节")
	(holiday-fixed 9 10 "教师节")
	(holiday-fixed 10 1 "国庆节")
	(holiday-fixed 12 25 "圣诞节")
	;; 农历节日
	(holiday-lunar 1 1 "春节" 0)
	(holiday-lunar 1 2 "春节" 0)
	(holiday-lunar 1 3 "春节" 0)
	(holiday-lunar 1 15 "元宵节" 0)
	(holiday-solar-term "清明" "清明节")
	(holiday-solar-term "小寒" "小寒" )
	(holiday-solar-term "大寒" "大寒" )
	(holiday-solar-term "立春" "立春" )
	(holiday-solar-term "雨水" "雨水" )
	(holiday-solar-term "惊蛰" "惊蛰" )
	(holiday-solar-term "春分" "春分" )
	(holiday-solar-term "谷雨" "谷雨" )
	(holiday-solar-term "立夏" "立夏" )
	(holiday-solar-term "小满" "小满" )
	(holiday-solar-term "芒种" "芒种" )
	(holiday-solar-term "夏至" "夏至" )
	(holiday-solar-term "小暑" "小暑" )
	(holiday-solar-term "大暑" "大暑" )
	(holiday-solar-term "立秋" "立秋" )
	(holiday-solar-term "处暑" "处暑" )
	(holiday-solar-term "白露" "白露" )
	(holiday-solar-term "秋分" "秋分" )
	(holiday-solar-term "寒露" "寒露" )
	(holiday-solar-term "霜降" "霜降" )
	(holiday-solar-term "立冬" "立冬" )
	(holiday-solar-term "小雪" "小雪" )
	(holiday-solar-term "大雪" "大雪" )
	(holiday-solar-term "冬至" "冬至" )
	(holiday-lunar 5 5 "端午节" 0)
	(holiday-lunar 8 15 "中秋节" 0)
	(holiday-lunar 7 7 "七夕情人节" 0)
	(holiday-lunar 12 8 "腊八节" 0)
	(holiday-lunar 9 9 "重阳节" 0)
	(holiday-lunar 12 22 "冬至" 0)))

(setq calendar-holidays eh-calendar-holidays)

(setq calendar-month-name-array
      ["一月" "二月" "三月" "四月" "五月" "六月"
       "七月" "八月" "九月" "十月" "十一月" "十二月"])
(setq calendar-day-name-array
      ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])

;; 一周第一天，0表示星期天, 1表示星期一
(setq calendar-week-start-day 0)

;; 为calfw设置一个capture模板并添加到org-capture-templates
(setq cfw:org-capture-template
      '("calfw2org" "calfw2org" entry (file+headline eh-org-schedule-file "Schedule")
	"* %?\n %(cfw:org-capture-day)\n %a"))

(setq org-capture-templates
      (append org-capture-templates (list cfw:org-capture-template)))

;; 日历表格边框设置
(setq cfw:fchar-junction ?+
      cfw:fchar-vertical-line ?|
      cfw:fchar-horizontal-line ?-
      cfw:fchar-left-junction ?+
      cfw:fchar-right-junction ?+
      cfw:fchar-top-junction ?+
      cfw:fchar-top-left-corner ?+
      cfw:fchar-top-right-corner ?+ )

(defun eh-cfw-render-toolbar (width current-view prev-cmd next-cmd)
  "Translate words: 'Month', 'Week', 'Day' and 'Two day' to Chinese"
  (let* ((prev (cfw:render-button " < " prev-cmd))
	 (today (cfw:render-button "今天" 'cfw:navi-goto-today-command))
	 (next (cfw:render-button " > " next-cmd))
	 (month (cfw:render-button
		 "显示一月" 'cfw:change-view-month
		 (eq current-view 'month)))
	 (tweek (cfw:render-button
		 "显示两周" 'cfw:change-view-two-weeks
		 (eq current-view 'two-weeks)))
	 (week (cfw:render-button
		"显示一周" 'cfw:change-view-week
		(eq current-view 'week)))
	 (day (cfw:render-button
	       "显示一天" 'cfw:change-view-day
	       (eq current-view 'day)))
	 (sp  " ")
	 (toolbar-text
	  (cfw:render-add-right
	   width (concat sp prev sp next sp today sp)
	   (concat day sp week sp tweek sp month sp))))
    (cfw:render-default-content-face toolbar-text 'cfw:face-toolbar)))

(advice-add 'cfw:render-toolbar :override #'eh-cfw-render-toolbar)

(defun eh-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'month
   :contents-sources
   (list
    ;; orgmode source
    (cfw:org-create-source "Green"))))


;;;autoload(require 'eh-misc)
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-misc.el ends here
