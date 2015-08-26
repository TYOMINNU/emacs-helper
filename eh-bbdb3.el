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
(use-package bbdb
  :config

  (use-package gnus
    :ensure nil)

  (use-package message
    :ensure nil)

  (use-package bbdb-gnus
    :ensure bbdb
    :config
    (defun eh-bbdb-insinuate-gnus ()
      "BBDB setting for gnus, See `bbdb-insinuate-gnus' for details."
      (define-key gnus-summary-mode-map ":" 'bbdb-mua-display-sender)
      (define-key gnus-article-mode-map ":" 'bbdb-mua-display-sender)
      (define-key gnus-summary-mode-map ";" 'bbdb-mua-edit-field)
      (define-key gnus-article-mode-map ";" 'bbdb-mua-edit-field))

    (add-hook 'gnus-startup-hook 'eh-bbdb-insinuate-gnus))

  (use-package bbdb-vcard)
  (use-package bbdb-csv-import)
  (use-package bbdb-china)

  (use-package bbdb-android
    :config
    (defun eh-bbdb-keybinding ()
      (bbdb-handy-keybinding-setup)
      (define-key bbdb-mode-map "c" 'eh-bbdb-create)
      (define-key bbdb-mode-map "M" 'bbdb-merge-records)
      (define-key bbdb-mode-map (kbd "x e") 'bbdb-android-export)
      (define-key bbdb-mode-map (kbd "x i") 'bbdb-android-import)
      (define-key bbdb-mode-map (kbd "x r") 'bbdb-android-import-from-radicale))
    (add-hook 'bbdb-mode-hook 'eh-bbdb-keybinding))

  (use-package bbdb-handy
    :config
    (define-key message-mode-map "\C-cb" 'bbdb-handy)
    (define-key message-mode-map "\t" 'bbdb-handy-message-tab))

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
        bbdb-dial-function 'bbdb-android-dia-with-adb)

  (setq bbdb-vcard-name-imported-priority '(formated-name first-last bbdb-vcard-generate-bbdb-name)
        bbdb-vcard-skip-on-import '("^X-GSM-" "^X-RADICALE-" "^X-CONTACTSYNC-" "^PRODID" "^UID")
        bbdb-vcard-import-translation-table '(("CELL\\|CAR" . "cell")
                                              ("WORK\\|pref" . "work")
                                              ("DOM\\|HOME" . "home")))

  ;; initialization
  ;; (bbdb-initialize 'gnus 'message)
  ;; (bbdb-mua-auto-update-init 'gnus 'message)
  (bbdb-initialize)

  ;; Push email to message-mode
  (defun eh-bbdb-create ()
    (interactive)
    (let ((name (bbdb-read-string "联系人名称: "))
          (mail (bbdb-split 'mail (bbdb-read-string "电子邮件: ")))
          (phone (list (vector "work" (bbdb-read-string "电话号码: ")))))
      (bbdb-create-internal name nil nil nil mail phone)
      (bbdb name))))

(provide 'eh-bbdb3)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bbdb3.el ends here
