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

;; 新闻组地址
;; 添加几个著名的新闻组地址，方便测试
(setq gnus-select-method
      '(nnimap "OfflineImap"
	       (nnimap-address "localhost")
	       (nnimap-stream shell)
               (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Maildir")))

(add-to-list 'gnus-secondary-select-methods
               '(nntp "localhost"))

;; (setq gnus-select-method '(nnimap "gmail"
;; 				  (nnimap-address "imap.gmail.com")
;; 				  (nnimap-stream ssl)))

;; (add-to-list 'gnus-secondary-select-methods
;;               '(nntp "news.gmane.org"))
;; (add-to-list 'gnus-secondary-select-methods
;;               '(nntp "news.newsfan.net"))



;; 存储设置
(setq gnus-startup-file "~/Gnus/.newsrc")                  ;初始文件
(setq gnus-init-file "~/Gnus/.gnus")                       ;.gnus位置
(setq gnus-default-directory "~/Gnus/")                    ;默认目录
(setq gnus-home-directory "~/Gnus/")                       ;主目录
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

;; 双窗口布局
(gnus-add-configuration '(article 
                          (horizontal 1.0 
                                      (summary 0.50 point)
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
;; (setq gnus-summary-line-format (concat 
;;                                 "%4P "
;;                                 "%("
;;                                 "%U%R%z  "
;;                                 "%4&user-date;  "
;;                                 "%-12,12n  "
;;                                 "%B " 
;;                                 "%I "
;;                                 "%-50,50s "
;;                                 "%)"
;;                                 "\n"))

;; (setq gnus-summary-make-false-root 'dummy)
;; (setq gnus-summary-make-false-root-always nil)
;; (setq gnus-summary-dummy-line-format "    |->%-62,62S\n")

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
                                "\n"))

(copy-face 'default 'eh-gnus-face-2)
(set-face-foreground 'eh-gnus-face-2 "orange")
(setq gnus-face-2 'eh-gnus-face-2)

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
      "" "---->"))


;; 设置user-date变量，自定义日期时间的显示格式
;; (setq gnus-user-date-format-alist
;;       '(((gnus-seconds-today) . "今天%d号")
;;         ((+ (* 24 3600)    (gnus-seconds-today)) . "昨天%d号") 
;;         ((+ (* 2  24 3600) (gnus-seconds-today)) . "前天%d号")
;;         ((gnus-seconds-month) . "本月%d号")
;;         ((gnus-seconds-year)  . "%m月%d号")
;;         (t . "%y-%m-%d")))

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


;; 时间显示
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ;将邮件的发出时间转换为本地时间
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ;跟踪组的时间轴
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)              ;新闻组分组
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (setq line-spacing 3)
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
                   "X-Attachments" "X-Diagnostic")
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
;; 排序
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)
        (not gnus-thread-sort-by-date)))

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
