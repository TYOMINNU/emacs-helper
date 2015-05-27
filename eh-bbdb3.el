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
(require 'bbdb-ext)

;; Variables
(setq bbdb-file "~/contacts/contacts.bbdb"
      bbdb-message-all-addresses t
      bbdb-phone-style nil
      bbdb-pop-up-window-size 0.5
      bbdb-mua-update-interactive-p '(query . create)  ;; Invoking bbdb interactively
      bbdb-message-all-addresses t
      bbdb-mua-summary-mark nil
      bbdb-completion-list t
      bbdb-complete-mail-allow-cycling t)

;; initialization
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)

;; BBDB setting for gnus
(defun eh-bbdb-insinuate-gnus ()
  "BBDB setting for gnus, See `bbdb-insinuate-gnus' for details."
  (define-key gnus-summary-mode-map ":" 'bbdb-mua-display-sender)
  (define-key gnus-article-mode-map ":" 'bbdb-mua-display-sender)
  (define-key gnus-summary-mode-map ";" 'bbdb-mua-edit-field)
  (define-key gnus-article-mode-map ";" 'bbdb-mua-edit-field))

(add-hook 'gnus-startup-hook 'eh-bbdb-insinuate-gnus)

;; Add pinyin alias for gnus
(defun eh-bbdb-add-pinyin-aka (record)
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
             ,@(when first-name (pyim-hanzi2pinyin first-name t nil t))
             ,@(when first-name (pyim-hanzi2pinyin first-name nil nil t))
             ,@(when last-name (pyim-hanzi2pinyin last-name t nil t))
             ,@(when last-name (pyim-hanzi2pinyin last-name nil nil t)))))
    (bbdb-record-set-field record 'aka pinyin-alias)))

(defun eh-bbdb-return-chinese-string (str)
  (when (and str (string-match-p "\\cc" str))
    str))

(add-hook 'bbdb-change-hook 'eh-bbdb-add-pinyin-aka)

(provide 'eh-bbdb3)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bbdb3.el ends here
