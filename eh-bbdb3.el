;;; eh-bbdb3.el --- Tumashu's bbdb version 3 configure

;; Copyright (c) 2011-2013, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
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
(require 'gnus)
(require 'message)
(require 'bbdb)
(require 'bbdb-gnus)
(require 'bbdb-vcard)
(require 'bbdb-csv-import)

;; Variables
(setq bbdb-file "~/contacts/contacts.bbdb"
      bbdb-phone-style nil
      bbdb-pop-up-window-size 0.3
      bbdb-mua-pop-up-window-size 1.0
      bbdb-mua-update-interactive-p '(query . create)  ;; Invoking bbdb interactively
      bbdb-message-all-addresses t
      bbdb-mua-summary-mark nil
      bbdb-completion-list t
      bbdb-complete-mail-allow-cycling t
      bbdb-layout 'multi-line
      bbdb-pop-up-layout 'multi-line
      bbdb-mua-pop-up nil
      bbdb-default-country "China"
      bbdb-dial-function 'eh-bbdb-dia-with-adb
      bbdb-string-match-function 'eh-bbdb-string-match)

(setq bbdb-vcard-name-imported-priority '(formated-name first-last bbdb-vcard-generate-bbdb-name)
      bbdb-vcard-skip-on-import '("^X-GSM-" "^X-RADICALE-" "^X-CONTACTSYNC-" "^PRODID" "^UID")
      bbdb-vcard-skip-on-export '("^pinyin-abbrev")
      bbdb-vcard-import-translation-table '(("CELL\\|CAR" . "cell")
                                            ("WORK\\|pref" . "work")
                                            ("DOM\\|HOME" . "home")))

;; initialization
;; (bbdb-initialize 'gnus 'message)
;; (bbdb-mua-auto-update-init 'gnus 'message)
(bbdb-initialize)

;; BBDB setting for gnus
(defun eh-bbdb-insinuate-gnus ()
  "BBDB setting for gnus, See `bbdb-insinuate-gnus' for details."
  (define-key gnus-summary-mode-map ":" 'bbdb-mua-display-sender)
  (define-key gnus-article-mode-map ":" 'bbdb-mua-display-sender)
  (define-key gnus-summary-mode-map ";" 'bbdb-mua-edit-field)
  (define-key gnus-article-mode-map ";" 'bbdb-mua-edit-field))

(add-hook 'gnus-startup-hook 'eh-bbdb-insinuate-gnus)

;; Push email to message-mode
(defvar eh-bbdb-push-buffer nil)

(defun eh-bbdb-push-mail (records &optional subject n verbose)
  (interactive (list (bbdb-do-records) nil
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (if verbose (message "No records"))
    (let ((to (bbdb-mail-address records n nil verbose))
          (buffer eh-bbdb-push-buffer))
      (when (and buffer (not (string= "" to)))
        (with-current-buffer buffer
          (insert (concat to ", ")))
        (message "%s, will be push to buffer: %s" to buffer)))))

(defun eh-bbdb-quit-window ()
  (interactive)
  (setq eh-bbdb-push-buffer nil)
  (with-current-buffer bbdb-buffer-name
    (setq header-line-format nil))
  (quit-window))

(defun eh-bbdb-push-mail-and-quit-window ()
  (interactive)
  (if eh-bbdb-push-buffer
      (progn (call-interactively 'eh-bbdb-push-mail)
             (eh-bbdb-quit-window))
    (message "Can't find `eh-bbdb-push-buffer', Do Nothing!!")))

(defun eh-bbdb-grab-word ()
  (buffer-substring
   (point)
   (save-excursion
     (skip-syntax-backward "w")
     (point))))

(defun eh-bbdb ()
  (interactive)
  (let ((buffer (current-buffer))
        (bbdb-pop-up-window-size 1.0)
        prefix)
    ;; Update `eh-bbdb-push-buffer'
    (if (string= mode-name "Message")
        (progn
          (setq eh-bbdb-push-buffer buffer)
          (setq prefix (eh-bbdb-grab-word)))
      (setq eh-bbdb-push-buffer nil)
      (setq prefix nil))

    ;; Call bbdb
    (if prefix
        (progn
          (delete-char (- 0 (length prefix)))
          (bbdb prefix))
      (bbdb ""))

    ;; Update `header-line-format'
    (when (string= mode-name "Message")
      (with-current-buffer bbdb-buffer-name
        (setq header-line-format
              (format "## Type `C-c C-c' or `p' to push email to buffer %s. ##"
                      (buffer-name buffer)))))))

(defun eh-bbdb-message-tab ()
  (interactive)
  (cond
   ;; 在 header 中, 按 TAB 键调用 eh-bbdb.
   ((and (save-excursion
           (let ((point (point)))
             (message-goto-body)
             (> (point) point)))
         (not (looking-back "^\\(Subject\\|From\\): *.*"
                            (line-beginning-position)))
         (not (looking-back "^" (line-beginning-position))))
    (eh-bbdb))
   (message-tab-body-function (funcall message-tab-body-function))
   (t (funcall (or (lookup-key text-mode-map "\t")
                   (lookup-key global-map "\t")
                   'indent-relative)))))

(defun eh-bbdb-create ()
  (interactive)
  (let ((name (bbdb-read-string "联系人名称: "))
        (mail (bbdb-split 'mail (bbdb-read-string "电子邮件: ")))
        (phone (list (vector "work" (bbdb-read-string "电话号码: ")))))
    (bbdb-create-internal name nil nil nil mail phone)
    (bbdb name)))

(defun eh-bbdb-search-records ()
  (interactive)
  (call-interactively 'bbdb)
  (message "Type `g', show all contacts records"))

(defun eh-bbdb-display-all-records ()
  (interactive)
  (bbdb-display-all-records)
  (message "Show all contacts records ..."))

(defun eh-bbdb-keybinding ()
  (define-key bbdb-mode-map "g" 'eh-bbdb-display-all-records)
  (define-key bbdb-mode-map "q" 'eh-bbdb-quit-window)
  (define-key bbdb-mode-map "p" 'eh-bbdb-push-mail)
  (define-key bbdb-mode-map "c" 'eh-bbdb-create)
  (define-key bbdb-mode-map "M" 'bbdb-merge-records)
  (define-key bbdb-mode-map "\C-s" 'eh-bbdb-search-records)
  (define-key bbdb-mode-map "b" 'eh-bbdb-search-records)
  (define-key bbdb-mode-map "\C-c\C-c" 'eh-bbdb-push-mail)
  (define-key bbdb-mode-map (kbd "x e") 'eh-bbdb-export-vcard-file-to-android)
  (define-key bbdb-mode-map (kbd "x i") 'eh-bbdb-import-vcard-file-from-android)
  (define-key bbdb-mode-map (kbd "x r") 'eh-bbdb-import-vcard-file-from-radicale)
  (define-key bbdb-mode-map (kbd "RET") 'eh-bbdb-push-mail-and-quit-window))

(add-hook 'bbdb-mode-hook 'eh-bbdb-keybinding)

(define-key message-mode-map "\C-cb" 'eh-bbdb)
(define-key message-mode-map "\t" 'eh-bbdb-message-tab)

(defun eh-bbdb-string-match (regexp string)
  (let ((string-list
         `(,string
           ,@(when (and string (featurep 'chinese-pyim))
               (pyim-hanzi2pinyin string t nil t))
           ,@(when (and string (featurep 'chinese-pyim))
               (pyim-hanzi2pinyin string nil nil t)))))
    (cl-some #'(lambda (x)
                 (string-match regexp x))
             string-list)))

(defun eh-bbdb-puthash (orig-fun key record)
  (funcall orig-fun key record)
  (when (and key (not (string= "" key))
             (string-match-p "\\cc" key)
             (featurep 'chinese-pyim))
    (let ((key-pinyin-1  (pyim-hanzi2pinyin key))
          (key-pinyin-2  (pyim-hanzi2pinyin key t)))
      (funcall orig-fun key-pinyin-1 record)
      (funcall orig-fun key-pinyin-2 record))))

(defun eh-bbdb-remhash (orig-fun key record)
  (funcall orig-fun key record)
  (when (and key (not (string= "" key))
             (string-match-p "\\cc" key)
             (featurep 'chinese-pyim))
    (let ((key-pinyin-1  (pyim-hanzi2pinyin key))
          (key-pinyin-2  (pyim-hanzi2pinyin key t)))
      (funcall orig-fun key-pinyin-1 record)
      (funcall orig-fun key-pinyin-2 record))))

(advice-add 'bbdb-puthash :around #'eh-bbdb-puthash)
(advice-add 'bbdb-remhash :around #'eh-bbdb-remhash)

;; Add pinyin alias for gnus
(defun eh-bbdb-add-pinyin-abbreviation (record)
  (when (featurep 'chinese-pyim)
    (let* ((bbdb-allow-duplicates t)
           (first-name (eh-bbdb-return-chinese-string
                        (bbdb-record-firstname record)))
           (last-name (eh-bbdb-return-chinese-string
                       (bbdb-record-lastname record)))
           pinyin-list)
      (setq pinyin-list
            (delete-dups
             `(,@(when first-name (pyim-hanzi2pinyin first-name nil nil t))
               ,@(when last-name (pyim-hanzi2pinyin last-name nil nil t))
               ,@(when first-name (pyim-hanzi2pinyin first-name t nil t))
               ,@(when last-name (pyim-hanzi2pinyin last-name t nil t)))))
      (bbdb-record-set-xfield
       record 'pinyin-abbrev (mapconcat 'identity pinyin-list ", ")))))

(defun eh-bbdb-return-chinese-string (str)
  (when (and str (string-match-p "\\cc" str))
    str))

;; Pinyin abbreviation make search chinese easily.
;; (add-hook 'bbdb-change-hook 'eh-bbdb-add-pinyin-abbreviation)

(defun eh-bbdb-import-vcard-file-from-radicale ()
  "Import Radicale vcard with bbdb-vcard."
  (interactive)
  (let* ((directory (concat "~/.config/radicale/collections/"
                            (user-real-login-name) "/"))
         (files (directory-files directory t ".vcf$"))
         (file (completing-read "Import vcard file: " files)))
    (when (yes-or-no-p (format "Really import vcard file: %s? " file))
      (bbdb-vcard-import-file file))))

(defun eh-bbdb-adb-connect-p ()
  (not (= 1 (shell-command "adb devices | grep HC2CXW101748"))))

(defun eh-bbdb-import-vcard-file-from-android ()
  "Copy vcard file from android phone with adb, then import to bbdb."
  (interactive)
  (let* ((time-string (format-time-string "%Y%m%d" nil t))
         (contacts-db-file-name
          (concat "android-contacts-" time-string ".db"))
         (vcard-file-name
          (concat "android-contacts-" time-string ".vcf"))
         (temp-contacts-db-1
          (concat (file-name-as-directory "/sdcard/BBDB/") contacts-db-file-name))
         (temp-contacts-db-2
          (concat (file-name-as-directory "~/BBDB/") contacts-db-file-name))
         (vcard-file
          (concat (file-name-as-directory "~/BBDB/") vcard-file-name)))
    (if (and (eh-bbdb-adb-connect-p)
             (yes-or-no-p "Do you want to import from android phone? "))
        (progn
          ;; Copy contacts database file to /sdcard directory
          ;; NOTE: This require your android *rooted*.
          (shell-command
           (format "adb shell \"su -c 'cat %s > %s'\""
                   "/data/data/com.android.providers.contacts/databases/contacts2.db"
                   temp-contacts-db-1))
          ;; Pull contacts db file to computer
          (shell-command
           (format "adb pull %s %s"
                   temp-contacts-db-1
                   temp-contacts-db-2))

          ;; Import vcard file to BBDB
          (eh-bbdb-import-android-contacts-db temp-contacts-db-2))
      (message "Can't connect android device by adb command."))))

(defun eh-bbdb-export-vcard-file-to-android ()
  "Export bbdb to vcard file and save to android with adb command."
  (interactive)
  (let* ((records (bbdb-records))
         (vcard-file-name
          (concat "bbdb-contacts-"
                  (format-time-string "%Y%m%d" nil t) ".vcf"))
         (temp-vcard-file
          (concat (file-name-as-directory
                   "~/BBDB/") vcard-file-name))
         (remote-vcard-file
          (concat (file-name-as-directory
                   "/sdcard/BBDB/") vcard-file-name))
         (remote-vcard-file-2
          (concat (file-name-as-directory
                   "file:///sdcard/BBDB/") vcard-file-name)))
    (with-temp-buffer
      (dolist (record records)
        (insert (bbdb-vcard-from record)))
      (bbdb-vcard-write-buffer temp-vcard-file t))
    (if (not (eh-bbdb-adb-connect-p))
        (message "Can't connect android device by adb command.")
      (when (yes-or-no-p (format "Push temp vcard file to \"%s\" ? " remote-vcard-file))
        (shell-command (format "adb push %s %s" temp-vcard-file remote-vcard-file)))
      (when (yes-or-no-p "Delete all android contacts ? ")
        (shell-command "adb shell pm clear com.android.providers.contacts"))
      (when (yes-or-no-p (format "Import vcard file: \"%s\" ? " remote-vcard-file))
        (shell-command (format "adb shell am start -t \"%s\" -d \"%s\" -a android.intent.action.VIEW"
                               "text/x-vcard"
                               remote-vcard-file-2))))))

(defun eh-bbdb-dia-with-adb (phone-number)
  (if (eh-bbdb-adb-connect-p)
      (progn
        (shell-command
         (format "adb shell am start -a android.intent.action.CALL -d tel:%s"
                 phone-number))
        (message "Dia phone number: %s ..." phone-number))
    (message "Can't connect android device by adb command.")))


(defun eh-bbdb-import-android-contacts-db (android-db-file)
  (let* ((command
          (format "sqlite3 %s \"%s\""
                  android-db-file
                  (concat "SELECT raw_contacts._id, raw_contacts.display_name, "
                          "raw_contacts.display_name_alt, mimetypes.mimetype, "
                          "REPLACE(REPLACE(data.data1, '\r\n', '\n'), '\n', '\n'), "
                          "data.data2, "
                          "REPLACE(REPLACE(data.data4, '\r\n', '\n'), '\n', '\n'), "
                          "data.data5, data.data6, data.data7, "
                          "data.data8, data.data9, data.data10, "
                          "quote(data.data15)||'::::' " ;; Insert "::::" as split-string separator
                          "FROM raw_contacts, data, mimetypes "
                          "WHERE raw_contacts.deleted = 0 "
                          "AND raw_contacts._id = data.raw_contact_id "
                          "AND data.mimetype_id = mimetypes._id "
                          "ORDER BY raw_contacts._id, mimetypes._id, data.data2")))
         (sqlite3-output (shell-command-to-string command))
         contacts-list contacts-list2 scards-list)

    ;; Convert string to lisp, like the example:
    ;; "1|a|b::::         (("1" "a" "b")
    ;;  1|c|d::::    -->   ("1" "c" "d")
    ;;  1|e|f::::"         ("1" "e" "f"))
    (setq contacts-list
          (mapcar #'(lambda (str)
                      (split-string str "\|"))
                  ;; String "::::" at the end of data15 as separator.
                  (split-string sqlite3-output "::::\n")))

    ;; Convert list to alist, like the example:
    ;; (("1" "a" "b")       ("1" (("a" "b")
    ;;  ("1" "c" "d")   ->        ("c" "d")
    ;;  ("1" "e" "f"))            ("e" "f")))
    (dolist (contact contacts-list)
      (let* ((key (car contact))
             (value (cdr contact)))
        (if (assoc key contacts-list2)
            (push value (car (cdr (assoc key contacts-list2))))
          (push `(,key (,value)) contacts-list2))))

    ;; convert to bbdb-vcard scard format.
    (setq scards-list
          (delq 'nil (mapcar
                      #'(lambda (x)
                          (eh-bbdb-android-contact-scardize (car (cdr x))))
                      contacts-list2)))

    ;; import scards to bbdb database
    (mapc #'bbdb-vcard-import-vcard-internal scards-list)
    (message "Import android contacts finished.")))

(defun eh-bbdb-android-contact-scardize (list)
  "Convert list to scard"
  ;; (("FN" ((("content" "Hello World"))))
  ;;  ("N" ((("content" ("Word" "Hello" nil nil nil)))))
  ;;  ("ORG" ((("content" ("Example.com Inc." nil)))))
  ;;  ("EMAIL" ((("type" ("pref" "work" "internet"))
  ;;             ("content" "helloworld@example.org"))))
  ;;  ("TEL" ((("type" ("pref" "work"))
  ;;           ("content" "123456789"))
  ;;          (("type" "work")
  ;;           ("content" "234567890"))
  ;;          (("type" "cell")
  ;;           ("content" "345678901"))
  ;;          (("type" "home")
  ;;           ("content" "456789012"))))
  ;;  ("ADR" ((("type" "work")
  ;;           ("content" (nil nil
  ;;                           "2 Enterprise Avenue"
  ;;                           "Worktown"
  ;;                           "NY"
  ;;                         "01111"
  ;;                         "USA")))
  ;;          (("type" ("pref" "home"))
  ;;           ("content" (nil nil
  ;;                           "3 Acacia Avenue"
  ;;                            "Hoemtown"
  ;;                            "MA"
  ;;                            "02222"
  ;;                            "USA")))))
  ;;  ("URL" ((("type" "pref")
  ;;           ("content" "http://www.helloworld1.com/"))
  ;;          (("content" "http://www.helloworld2.com/"))))
  ;;  ("NOTE" ((("content" "This is a note.")))))
  (let (scard)
    (dolist (x list)
      (let* ((name (nth 0 x))
             (alt-name-string (nth 1 x))
             (alt-name
              (when (stringp alt-name-string)
                (split-string alt-name-string ", *")))
             (mimetype (nth 2 x))
             (data1 (nth 3 x))
             (data2 (nth 4 x))
             (data4 (nth 5 x))
             (data5 (nth 6 x))
             (data6 (nth 7 x))
             (data7 (nth 8 x))
             (data8 (nth 9 x))
             (data9 (nth 10 x))
             (data10 (nth 11 x))
             (data15 (nth 12 x)))
        (when (and name (string= mimetype "vnd.android.cursor.item/name"))
          (push `("FN" ((("content" ,name)))) scard))

        (when (and alt-name (string= mimetype "vnd.android.cursor.item/name"))
          (push `("N" ((("content" ,alt-name)))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/email_v2"))
          (let ((key "EMAIL")
                (value `(("content" ,data1))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/website"))
          (let ((key "URL")
                (value `(("content" ,data1))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/note"))
          (push `("NOTE" ((("content" ,data1)))) scard))

        ;; TODO: Write a elisp function which do job like below command.
        ;;
        ;;   "perl -ne 's/([0-9a-f]{2})/print chr hex $1/gie' | base64 --wrap=0"
        ;;
        ;; (when (and data1 (string= mimetype "vnd.android.cursor.item/photo"))
        ;;   (push `("PHOTO" ((("content" ,data15)))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/organization"))
          (push `("ORG" ((("content" (,data1))))) scard))

        (when (and data4 (string= mimetype "vnd.android.cursor.item/organization"))
          (push `("TITLE" ((("content" (,data4))))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/nickname"))
          (push `("NICKNAME" ((("content" ,(split-string data1 ", *"))))) scard))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/phone_v2"))
          (let* ((list '(("1" "HOME" "VOICE") ("2" "CELL" "VOICE" "PREF")
                         ("3" "WORK" "VOICE") ("4" "WORK" "FAX")
                         ("5" "HOME" "FAX") ("6" "PAGER") ("7" "OTHER")
                         ("8" "CUSTOM") ("9" "CAR" "VOICE")))
                 (key "TEL")
                 (value `(("type" ,(cdr (assoc data2 list)))
                          ("content" ,data1))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/postal-address_v2"))
          (let* ((list '(("1"  "HOME") ("2"  "WORK")))
                 (key "ADR")
                 (value (if (string= data1 "United States of America")
                            `(("type" ,(cdr (assoc data2 list)))
                              ("content" ,data1))
                          `(("type" ,(cdr (assoc data2 list)))
                            ("content" (,data4 ,data7 ,data8 ,data9 ,data10))))))
            (if (assoc key scard)
                (push value (car (cdr (assoc key scard))))
              (push `(,key (,value)) scard))))

        (when (and data1 (string= mimetype "vnd.android.cursor.item/im"))
          (let* ((list `(("-1"  ,(concat "-IM-Custom-" data6))
                         ("1"  "X-MSN")
                         ("2"  "X-YAHOO")
                         ("3"  "X-SKYPE-USERNAME")
                         ("4"  "X-QQ")
                         ("5"  "X-GOOGLE-TALK")
                         ("6"  "X-ICQ")
                         ("7"  "X-JABBER")))
                 (key (or (car (cdr (assoc data2 list)))
                          "IM")))
            (push `(,key ((("content" ,data1)))) scard)))
        ))
    scard))

(provide 'eh-bbdb3)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bbdb3.el ends here
