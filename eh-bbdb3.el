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
      bbdb-default-country "China")

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
        (message "Pushed [ %s ] to buffer %s" to buffer)))))

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

(defun eh-bbdb ()
  (interactive)
  (let ((buffer (current-buffer))
        (bbdb-pop-up-window-size 1.0))
    ;; Update `eh-bbdb-push-buffer'
    (if (string= mode-name "Message")
        (setq eh-bbdb-push-buffer buffer)
      (setq eh-bbdb-push-buffer nil))
    ;; Call bbdb
    (bbdb "")
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
   ((save-excursion
      (let ((point (point)))
        (message-goto-body)
        (> (point) point)))
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
  (define-key bbdb-mode-map (kbd "RET") 'eh-bbdb-push-mail-and-quit-window))

(add-hook 'bbdb-mode-hook 'eh-bbdb-keybinding)

(define-key message-mode-map "\C-cb" 'eh-bbdb)
(define-key message-mode-map "\t" 'eh-bbdb-message-tab)

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
             `(,@(when first-name (pyim-hanzi2pinyin first-name t nil t))
               ,@(when last-name (pyim-hanzi2pinyin last-name t nil t)))))
      (bbdb-record-set-xfield
       record 'pinyin-abbrev (mapconcat 'identity pinyin-list ", ")))))

(defun eh-bbdb-add-pinyin-aka (record)
  (when (featurep 'chinese-pyim)
    (let* ((bbdb-allow-duplicates t)
           (first-name (eh-bbdb-return-chinese-string
                        (bbdb-record-firstname record)))
           (last-name (eh-bbdb-return-chinese-string
                       (bbdb-record-lastname record)))
           (aka (bbdb-record-aka record))
           pinyin-alias)
      (setq pinyin-alias
            (delete-dups
             `(,@aka
               ,@(when first-name (pyim-hanzi2pinyin first-name nil nil t))
               ,@(when last-name (pyim-hanzi2pinyin last-name nil nil t)))))
      (bbdb-record-set-field record 'aka pinyin-alias))))

(defun eh-bbdb-return-chinese-string (str)
  (when (and str (string-match-p "\\cc" str))
    str))

(add-hook 'bbdb-change-hook 'eh-bbdb-add-pinyin-aka)
(add-hook 'bbdb-change-hook 'eh-bbdb-add-pinyin-abbreviation)


(provide 'eh-bbdb3)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bbdb3.el ends here
