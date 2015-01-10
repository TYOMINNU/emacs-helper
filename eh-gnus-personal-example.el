;; 从authinfo文件中解析密码，避免密码明文保存
(defun eh-gnus-get-password (host port)
  (let* ((authinfo (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (hostentry (netrc-machine authinfo host (format "%s" port) port)))
    (when hostentry (netrc-get hostentry "password"))))

;; Gnus邮件设置。
(setq gnus-select-method '(nnml ""))
(setq mail-sources
      `((pop :server "pop.163.com"
             :user "myname@163.com"
             :port 995
             :password ,(eh-gnus-get-password "pop.163.com" 995)
             :stream ssl
             :leave t)
        (imap :server "imap.qq.com"
              :user "myname@qq.com"
              :port 993
              :password ,(eh-gnus-get-password "imap.qq.com" 993)
              :stream ssl
              :fetchflag "\\Seen")))

;; 使用offlineimap, leafnode以及rss2email
;; 将邮件，新闻组和rss订阅同步到本地dovecot服务器
;; 然后使用gnus访问。

(add-to-list 'gnus-secondary-select-methods
             '(nntp "localhost"))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "rss"
                      (nnimap-address "localhost")
                      (nnimap-stream network)
                      (nnimap-inbox "INBOX")
                      (nnimap-split-methods 'nnmail-split-fancy)))

;; gnus-posting-styles设置
;; 1. 邮件发送时字符编码设置.
;; 2. 发送邮件使用的方法.
(setq gnus-posting-styles
      '((".*"
         (signature "")
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 utf-8 gb2312 gbk utf-8 gb18030))))
        (message-mail-p
         (name    "My Name")
         (address "myname@163.com")
         ("Cc" "My Name <myname@163.com>")
         ("X-Message-SMTP-Method" "smtp smtp.163.com 465")
         (eval (setq smtpmail-stream-type 'ssl)
               (setq mm-coding-system-priorities
                     '(iso-8859-1 utf-8 gb2312 gbk utf-8 gb18030))))
        (".*newsfan.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8))))
        (".*cn99.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8))))))


;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "gmail"
;;                       (nnimap-address "localhost")
;;                       (nnimap-stream network)))
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "163mail"
;;                       (nnimap-address "localhost")
;;                       (nnimap-stream network)))
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "qqmail"
;;                       (nnimap-address "localhost")
;;                       (nnimap-stream network)))

;;; 其他一些常见的配置例子
;;
;; (add-to-list 'gnus-secondary-select-methods
;;       '(nnimap "RSS"
;;            (nnimap-address "localhost")
;;            (nnimap-stream shell)
;;            (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Maildir/rss:LAYOUT=fs")))
;;
;; (setq gnus-select-method
;;       '(nnimap "gmail"
;;         (nnimap-address "imap.gmail.com")
;;         (nnimap-stream ssl)))
;;
;; (add-to-list 'gnus-secondary-select-methods
;;       '(nntp "news.gmane.org"))
;;
;; (add-to-list 'gnus-secondary-select-methods
;;       '(nntp "news.newsfan.net"))
