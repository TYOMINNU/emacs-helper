;;; eh-gnus.el --- Tumashu's gnus configuation file

;; Copyright (c) 2008-2009, Andy Stewart
;;               2011-2012, Feng Shu

;; Author: Andy Stewartf <lazycat.manatee@gmail.com>
;;         Feng Shu <tumashu@gmail.com>
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

;;; Code:


;; require
(require 'gnus)
(require 'mm-encode)
(require 'mm-decode)
(require 'rfc2047)
(require 'nnir)
(require 'gnus-demon)
(require 'eww)

;; 新闻组地址
;; 添加几个著名的新闻组地址，方便测试
(setq gnus-select-method
      '(nnimap "OfflineImap"
	       (nnimap-address "localhost")
	       (nnimap-stream shell)
               (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Maildir")))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "localhost"))

(add-to-list 'gnus-secondary-select-methods
	     '(nnmaildir "RSS" 
			 (directory "~/RSS/")))

;; (setq gnus-select-method '(nnimap "gmail"
;; 				  (nnimap-address "imap.gmail.com")
;; 				  (nnimap-stream ssl)))

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nntp "news.gmane.org"))
;; (add-to-list 'gnus-secondary-select-methods
;;               '(nntp "news.newsfan.net"))



;; 存储设置
(setq gnus-startup-file "~/Gnus/.newsrc")                  ;初始文件
(setq gnus-init-file "~/Gnus/.gnus")                       ;.gnus位置
(setq gnus-default-directory nil)                          ;默认目录
(setq gnus-home-directory "~/")                            ;主目录
(setq gnus-dribble-directory "~/Gnus/")                    ;恢复目录
(setq gnus-directory "~/Gnus/News/")                       ;新闻组的存储目录
(setq gnus-article-save-directory "~/Gnus/News/")          ;文章保存目录
(setq gnus-kill-files-directory "~/Gnus/News/trash/")      ;文件删除目录
(setq gnus-agent-directory "~/Gnus/News/agent/")           ;代理目录
(setq gnus-cache-directory "~/Gnus/News/cache/")           ;缓存目录
(setq gnus-cache-active-file "~/Gnus/News/cache/active")   ;缓存激活文件
(setq message-directory "~/Gnus/Mail/")                    ;邮件的存储目录
(setq message-auto-save-directory "~/Gnus/Mail/drafts")    ;自动保存的目录
(setq mail-source-directory "~/Gnus/Mail/incoming")        ;邮件的源目录
(setq nnmail-message-id-cache-file "~/Gnus/.nnmail-cache") ;nnmail的消息ID缓存
(setq nnml-newsgroups-file "~/Gnus/Mail/newsgroup")        ;邮件新闻组解释文件
(setq nntp-marks-directory "~/Gnus/News/marks")            ;nntp组存储目录
(setq mml-default-directory "~/")                          ;附件的存储位置
(setq gnus-notifications-minimum-level 1)                  ;桌面提醒功能
(setq gnus-notifications-use-google-contacts nil)
(setq gnus-notifications-use-gravatar nil)

;; 默认禁用nnfolder
(setq gnus-message-archive-group nil)
;; 发送信件程序设置
;; (setq sendmail-program "msmtp")                             ;设置发送程序
;; 当使用message-mode时的发信方式.
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; (setq message-send-mail-function 'message-send-mail-with-mailclient)

;; (setq message-sendmail-extra-arguments '("-a" "default"))
;; (setq message-sendmail-envelope-from 'header)

;; (defun eh-send-mail-with-msmtp ()
;;   (if (message-mail-p)
;;       (save-excursion
;; 	(let* ((from
;; 		(save-restriction
;; 		  (message-narrow-to-headers)
;; 		  (message-fetch-field "from")))
;; 	       (account
;; 		(cond
;; 		 ((string-match "tumashu@gmail.com" from) "tumashu@gmail.com"))))
;; 	  (setq message-sendmail-extra-arguments (list "-a" account))))))
;; (add-hook 'message-send-mail-hook 'eh-send-mail-with-msmtp)

;; 编码设置
;; gnus默认使用的编码。如果常与国外联系，设置为utf-8如果只在本国使用，设置为本地
;; 编码,比如gbk
(setq gnus-default-charset 'gbk)
;;根据我们选择的 method确定编码
(setq gnus-group-name-charset-method-alist
      '(((nntp "news.newsfan.net") . gbk)
        ((nntp "news.cn99.com") . gbk)))
;;根据组名确定组名采用的编码
(setq gnus-group-name-charset-group-alist
      '((".*" . gbk)))
;; 正则表达式匹配的组默认使用的编码。
(setq gnus-group-charset-alist
      '((".*" . gbk)))
;; 如果还有乱码，手动调整
(setq gnus-summary-show-article-charset-alist
      '((1 . gbk)
        (2 . utf-8)
        (3 . big5)
        (4 . utf-7)))
;; 邮件没有指定正确的MIME类型的时候的处理方式
(setq gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown x-gbk ))

;; gnus-posting-styles设置
;; 1. 发送编码设置.
;; 2. 用什么程序发送邮件.
(setq gnus-posting-styles
      '(("\(^INBOX\)\|\(\\[Gmail\\].*\)"
         ("X-Message-SMTP-Method" "sendmail"))
        (message-mail-p
         ("X-Message-SMTP-Method" "sendmail"))
        (".*"
         (signature "")
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1  utf-8  gb2312  gbk  utf-8 gb18030))))
        (".*newsfan.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1   gb2312  gbk   gb18030  utf-8))))
        (".*cn99.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1    gb2312    gbk    gb18030  utf-8))))))


;; 指定附件文件名和subject的编码方式
(defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)
(add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
(add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))

;; 常规设置
(setq gnus-agent t)                                 ;开启agent
(setq read-mail-command 'gnus)                      ;使用gnus阅读邮件
(setq mail-user-agent 'gnus-user-agent)             ;使用gnus发送邮件
(setq gnus-inhibit-startup-message t)               ;关闭启动时的画面
(setq gnus-novice-user nil)                         ;关闭新手设置, 不进行确认
(setq gnus-expert-user t)                           ;不询问用户
(setq gnus-show-threads t)                          ;显示邮件线索
(setq gnus-interactive-exit t)                      ;退出时进行交互式询问
(setq gnus-use-dribble-file t)                      ;创建恢复文件
(setq gnus-always-read-dribble-file t)              ;读取恢复文件
(setq gnus-asynchronous t)                          ;异步操作
(setq gnus-large-newsgroup 2000)                    ;设置大容量的新闻组默认显示的大小
(setq gnus-read-active-file 'some)
(setq gnus-nov-is-evil nil)
(setq gnus-large-ephemeral-newsgroup nil)           ;和上面的变量一样, 只不过对于短暂的新闻组
(setq gnus-summary-ignore-duplicates t)             ;忽略具有相同ID的消息
(setq gnus-treat-fill-long-lines t)                 ;如果有很长的行, 不提示
(setq message-confirm-send t)                       ;防止误发邮件, 发邮件前需要确认
(setq message-kill-buffer-on-exit t)                ;设置发送邮件后删除buffer
(setq message-from-style 'angles)                   ;`From' 头的显示风格
(setq message-syntax-checks '((sender . disabled))) ;语法检查
(setq nnmail-expiry-wait 7)                         ;邮件自动删除的期限 (单位: 天)
(setq nnmairix-allowfast-default t)                 ;加快进入搜索结果的组
(setq gnus-use-correct-string-widths t)             ;使用正确的字体宽度
(setq gc-cons-threshold 3500000)                    ;加快gnus的速度
(setq gnus-use-cross-reference t)                   ;交叉索引
(setq gnus-summary-display-while-building 50)       ;在生成summary时,每50封显示一下
;; 进入summer模式时，禁止自动选择第一个article,
;; 这样设置主要是因为有些article下载速度极慢，
;; 会降低响应速度
(setq gnus-auto-select-first nil)
(setq gnus-auto-select-next nil)                  

;; 设置gnus启动时,组级别大于3的不自动更新。
;; 当你添加了许多速度慢的组时，比如rss,imap等，启动速度会相当慢。这时你
;; 可以把它们的组级别设置为大于3的值，这样启动时就不自动更新了。
;; 当你需要更新这些组的时候，使用 "4-g" "5-g" 等快捷键
(setq gnus-activate-level 3) 

;; 双窗口布局(垂直)
;; (gnus-add-configuration '(article
;;			  (horizontal 1.0
;;				      (summary 0.50 point)
;;				      (article 1.0))))

;; 双窗口布局(水平)
(gnus-add-configuration '(article
                          (vertical 1.0
				    (summary 0.25 point)
				    (article 1.0))))

;; 三窗口布局
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;; 	       (vertical 25
;; 			 (group 1.0))
;; 	       (vertical 1.0
;; 			 (summary 0.25 point)
;; 			 (article 1.0)))))
;; (gnus-add-configuration
;;  '(summary
;;    (horizontal 1.0
;; 	       (vertical 25
;; 			 (group 1.0))
;; 	       (vertical 1.0
;; 			 (summary 1.0 point)))))

;; 显示设置
(setq mm-inline-large-images t)                       ;显示内置图片
(add-to-list 'mm-attachment-override-types "image/*") ;附件显示图片

;; 概要显示设置
;; 设置summer缓冲区的显示格式
(setq gnus-extra-headers
      '(To From))
(setq nnmail-extra-headers gnus-extra-headers)
(setq gnus-summary-gather-subject-limit 'fuzzy) ;聚集题目用模糊算法
(setq gnus-summary-make-false-root 'adopt)
(setq gnus-summary-line-format (concat 
                                "%U%R |"
                                "%ua"
                                "%2{%ub%}"
                                "%uc"
                                "%B"
                                "%I"
                                "%2{%ud%}"
                                "%ue"
                                "%3{%uf%}"
                                "\n"))

(copy-face 'default 'eh-gnus-face-2)
(set-face-foreground 'eh-gnus-face-2 "orange")
(setq gnus-face-2 'eh-gnus-face-2)

(defun eh-gnus-find-invisible-foreground ()
  (let ((candidates (remove
		     "unspecified-bg"
		     (nconc
		      (list (face-background 'default))
		      (mapcar
		       (lambda (alist)
			 (when (boundp alist)
			   (cdr (assoc 'background-color (symbol-value alist)))))
		       '(default-frame-alist initial-frame-alist window-system-default-frame-alist))
		      (list (face-foreground 'eh-gnus-face-3))))))
    (car (remove nil candidates))))

(copy-face 'default 'eh-gnus-face-3)
(set-face-foreground 'eh-gnus-face-3 (eh-gnus-find-invisible-foreground))
(setq gnus-face-3 'eh-gnus-face-3)


;; 显示箭头设置
(defun gnus-user-format-function-a (header)
  (let ((date (mail-header-date header)))
    (if (zerop gnus-tmp-level)
	"-> " "")))

;; 显示时间设置
(defun gnus-user-format-function-b (header)
  (let ((date (mail-header-date header)))
    (if (zerop gnus-tmp-level)
	"" (concat "     " (concat (gnus-user-date date) "  ")))))

;; 显示主题设置
(defun gnus-user-format-function-c (header)
  (let ((date (mail-header-date header))
        (subject (mail-header-subject header)))
    (if (zerop gnus-tmp-level)
        (concat subject
                " ("
                (gnus-user-date date)")") "")))

;; 提取From名字
(defun eh-mail-header-from-name (from)
  (cond
   ((string-match "<[^>]+> *$" from)
    (let ((beg (match-beginning 0)))
      (or (and (string-match "^\".+\"" from)
               (substring from 1 (1- (match-end 0))))
          (substring from 0 beg))))
   ((string-match "(.+)" from)
    (substring from
               (1+ (match-beginning 0)) (1- (match-end 0))))
   (t from)))

;; 显示发件人设置
(defun gnus-user-format-function-d (header)
  (let ((from (mail-header-from header)))
    (if (zerop gnus-tmp-level)
	"" (eh-mail-header-from-name from))))

;; 显示箭头设置
(defun gnus-user-format-function-e (header)
  (if (zerop gnus-tmp-level)
      "" "----> "))

;; 显示隐藏Subject, 用于搜索
(defun gnus-user-format-function-f (header)
  (let ((date (mail-header-date header))
        (subject (mail-header-subject header)))
    (if (zerop gnus-tmp-level)
        ""
      subject)))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((+ (* 24 3600)    (gnus-seconds-today)) . "YD   ")
        ((- (gnus-seconds-month) (* 72 3600)) . "%dth ")
        ((- (gnus-seconds-month) (* 48 3600)) . "%drd ")
        ((- (gnus-seconds-month) (* 24 3600)) . "%dnd ")
        ((gnus-seconds-month) . "%dst ")
        ((gnus-seconds-year)  . "%m-%d")
        (t . "%Y ")))

(setq gnus-thread-indent-level 0)
;; 线程的可视化外观, `%B'
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "    ")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-false-root "")
(setq gnus-sum-thread-tree-vertical "|")
(setq gnus-sum-thread-tree-leaf-with-other "|----")
(setq gnus-sum-thread-tree-single-leaf " `----")

;; Highlight当前行
(defface gnus-hl-line
  '((t :inherit hl-line))
  "Face for highlighting the current line with `gnus-hl-line'."
  :group 'hl-line)

(defun eh-gnus-hl-line ()
  (hl-line-mode 1)
  (set (make-local-variable 'hl-line-face) 'gnus-hl-line)
  (setq cursor-type nil))

(add-hook 'gnus-summary-mode-hook 'eh-gnus-hl-line)
(add-hook 'gnus-group-mode-hook 'eh-gnus-hl-line)

;; 将邮件的发出时间转换为本地时间
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)

;; 跟踪组的时间轴
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; 将新闻组分组
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Open X-RSS-URL with eww
(setq eh-gnus-article-url-field '("X-RSS-URL"))
(setq eh-gnus-current-article-url nil)
(setq eh-gnus-current-article-subject nil)
(setq eh-gnus-current-article-from nil)

(setq eh-eww-buffer-ignore-wash-regexp "lwn\\|phoronix")

(setq eh-eww-buffer-narrow-boundary-1 "")
(setq eh-eww-buffer-narrow-boundary-2
      (mapconcat 'regexp-quote
		 '("编辑" "作者" "责编" "关键字" "标签" "更多相关消息"
		   "相关新闻" "频道精选" "最新评论" "相关资讯"
		   "相关阅读" "相关文章" "看过本文的人还看过" "更多评论"
		   "分享编辑：" "查看所有评论" "我来说两句" "我要发言"
		   "点击可以复制本篇文章的标题和链接" "查看所有收藏过的文章"
		   "延伸阅读:" "您对这篇文章的评价" "焦点阅读" "相关链接"
		   "点击可以复制本篇文章的标题和链接" "发表评论" "查看全部评论"
		   "热门推荐" "复制本网址推荐" "延伸阅读" "热门排行")
		 "\\|"))

(defun eh-gnus-view-article-with-eww (&optional force)
  (interactive)
  (gnus-summary-scroll-up 1)
  (when (member "*eww*" (mapcar (function buffer-name) (buffer-list)))
    (kill-buffer "*eww*"))
  (gnus-eval-in-buffer-window gnus-article-buffer
    (setq eh-gnus-current-article-subject
	  (progn
	    (message-narrow-to-headers)
	    (message-fetch-field "Subject")))
    (setq eh-gnus-current-article-from
	  (progn
	    (message-narrow-to-headers)
	    (message-fetch-field "From")))
    (setq eh-gnus-current-article-url
	  (progn
	    (message-narrow-to-headers)
	    (eval (cons 'or (mapcar 'message-fetch-field
				    eh-gnus-article-url-field)))))
    (setq eh-eww-buffer-narrow-boundary-1
	  (progn
	    (message-goto-body)
	    ;; 前进四行,提取一个字符串，在eww buffer中搜索这个
	    ;; 字符串,可以获得正文的大概位置
	    (forward-line 4)
	    (set-mark (point))
	    (forward-char 10)
	    (replace-regexp-in-string
	     "^ +" ""
	     (buffer-substring-no-properties
	      (region-beginning) (region-end))))))
  (when (and (or force (string-match-p "\\cc" (or eh-gnus-current-article-subject "")))
	     eh-gnus-current-article-url)
    (eww eh-gnus-current-article-url)
    (delete-other-windows)))

(defun eh-eww-narrow-and-wash-buffer (&optional num1 num2)
  (interactive)
  (when (string= (buffer-name) "*eww*")
    (save-excursion
      (goto-char (point-min))
      ;; find first narrow boundary
      (if (re-search-forward eh-eww-buffer-narrow-boundary-1 nil t)
	  (progn
	    (backward-paragraph (or num1 1))
	    (set-mark (point)))
	(set-mark (point-min)))
      ;; find second narrow boundary
      (setq boundary-search-p t)
      (while (and (< (abs (- (point) (region-beginning)))
		     (/ (abs (- (point) (point-max))) 10))
		  boundary-search-p)
	(unless (re-search-forward eh-eww-buffer-narrow-boundary-2 nil t)
	  (goto-char (point-max))
	  (setq boundary-search-p nil)))
      (previous-line (or num2 1))
      
      ;; narrow to two boundary
      (narrow-to-region (region-beginning) (point))
      (goto-char (point-min))

      ;; wash the context
      (when (not
	     (or (string-match-p eh-eww-buffer-ignore-wash-regexp
				 eh-gnus-current-article-url)
		 (string-match-p eh-eww-buffer-ignore-wash-regexp
				 eh-gnus-current-article-from)
		 (string-match-p eh-eww-buffer-ignore-wash-regexp
				 eh-gnus-current-article-subject)))
	;; 自动断行
	(fill-region (point-min) (point-max))
	;; 行距设置为0.2
	(setq line-spacing 0.2)
	;; 设置字号
	(let ((text-scale-mode-amount 1.2))
	  (text-scale-mode))))))

(defun eh-eww-scroll-up ()
  (interactive)
  (if (and (< (point) 300)
	   (not (eh-narrow-p)))
      (eh-eww-narrow-and-wash-buffer)
    (scroll-up-command)))

(defun eh-eww-next-line ()
  (interactive)
  (if (and (< (point) 300)
	   (not (eh-narrow-p)))
      (eh-eww-narrow-and-wash-buffer)
    (next-line)))

(defun eh-eww-toggle-narrow ()
  (interactive)
  (if (eh-narrow-p)
      (progn (widen)
	     (message "Un-narrowing."))
    (progn (eh-eww-narrow-and-wash-buffer)
	   (message "Narrowing eww buffer"))))

(defun eh-narrow-p ()
  "Whether narrow is in effect for the current buffer"
  (let (real-point-min real-point-max)
    (save-excursion
      (save-restriction
	(widen)
	(setq real-point-min (point-min)
	      real-point-max (point-max))))
    (or (/= real-point-min (point-min))
	(/= real-point-max (point-max)))))

(define-key eww-mode-map (kbd "C-c C-c") 'eh-eww-toggle-narrow)
(define-key eww-mode-map (kbd "SPC") 'eh-eww-scroll-up)
(define-key eww-mode-map (kbd "<down>") 'eh-eww-next-line)

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            ;; summary buffer行距设置
            (setq line-spacing 3)
            ;; 设置一个face,用来隐藏不需要显示的文字
            (set-face-foreground 'eh-gnus-face-3 (eh-gnus-find-invisible-foreground))
            ;; summary buffer不显示右fringe
            (set-fringe-style  '(nil . 0))
            ;; 重新定义键盘绑定
            (local-set-key (kbd "SPC") (lambda ()
                                         (interactive)
                                         (gnus-summary-next-page)
                                         (move-beginning-of-line 1)))
	    (local-set-key (kbd "C-p") (lambda ()
					 (interactive)
                                         (delete-other-windows)
                                         (previous-line 1)))
	    (local-set-key (kbd "C-n") (lambda ()
                                         (interactive)
                                         (delete-other-windows)
                                         (next-line 1)))
	    (local-set-key (kbd "<up>") (lambda ()
					  (interactive)
					  (delete-other-windows)
					  (previous-line 1)))
	    (local-set-key (kbd "<down>") (lambda ()
					    (interactive)
					    (delete-other-windows)
					    (next-line 1)))
            (local-set-key (kbd "<return>") (lambda ()
					      (interactive)
					      (eh-gnus-view-article-with-eww)
					      (move-beginning-of-line 1)))
	    (local-set-key (kbd "C-<return>") (lambda ()
						(interactive)
						(eh-gnus-view-article-with-eww t)
						(move-beginning-of-line 1)))
            (local-set-key (kbd "<f1>") 'gnus-uu-mark-all)
            (local-set-key (kbd "<f2>") 'gnus-uu-unmark-thread)
            (local-set-key (kbd "<f3>") 'gnus-uu-mark-thread)))

;; visual
(setq gnus-treat-emphasize t
      gnus-treat-buttonize t
      gnus-treat-buttonize-head 'head
      gnus-treat-unsplit-urls 'last
      gnus-treat-leading-whitespace 'head
      gnus-treat-highlight-citation t
      gnus-treat-highlight-signature t
      gnus-treat-date-lapsed 'head
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-strip-cr t
      gnus-treat-overstrike nil
      gnus-treat-display-x-face t
      gnus-treat-display-face t
      gnus-treat-display-smileys nil
      gnus-treat-x-pgp-sig 'head)

;; 设置邮件报头显示的信息
(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic" "X-RSS-URL")
                 "\\|"))

;; 设置邮件日期显示格式,使用两行日期，一行具体日期时间，另一行显示article
;; 距现在多长时间
(setq gnus-article-date-headers '(user-defined))
(setq gnus-article-time-format
      (lambda (time)
	(concat "X-Sent:   "
                (format-time-string "%Y年%m月%d日 星期%u %R" time)
                "\n"
                "X-Lasped: "
                (article-lapsed-string time)
                )))


;; 用 Supercite 显示多种多样的引文形式
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)

;; 线程设置
(setq
 gnus-use-trees t                                                       ;联系老的标题
 gnus-tree-minimize-window nil                                          ;用最小窗口显示
 ;; gnus-fetch-old-headers 'some                                           ;抓取老的标题以联系线程,速度极慢
 gnus-generate-tree-function 'gnus-generate-horizontal-tree             ;生成水平树
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;聚集函数根据标题聚集
 )
;; Thread root排序
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-number
        gnus-thread-sort-by-most-recent-date))

;; Subthread排序
(setq gnus-subthread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))

;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group) ;gnus切换时
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group)    ;退出Summary时

;; 设置message hook
(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)


;; 每隔10分钟刷新一下
(add-hook 'gnus-startup-hook
          '(lambda () (progn
			(setq gnus-use-demon t)
			(gnus-demon-add-handler 'gnus-demon-scan-news 10 nil))))


;; 启用桌面提醒功能
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)



;;;###autoload
(add-hook 'gnus-before-startup-hook
	  '(lambda () (require 'eh-gnus)))

(provide 'eh-gnus)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-gnus.el ends here
