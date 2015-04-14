;;; eh-bibtex.el --- Tumashu's emacs configuation

;; Copyright (c) 2011-2013, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((eh-basic "0.0.1"))

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

;;; Codes
(require 'bibtex)

(defvar eh-bibtex-abstact-fill-column 80
  "default column when wash bib file")

(defun eh-bibtex-chinese-autokey-setup ()
  "Bibtex 中文文献 autokey 生成规则：

    <第一作者拼音><年份><标题前两个汉字字符拼音>

比如：[3] 徐琳玲. P公司标准成本制度研究[D]. 华东理工大学, 2013.
将生成：xulinling2013pgong

注：bibtex开启了词法绑定。"
  (setq bibtex-autokey-names 1
        bibtex-autokey-name-separator ""
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titlewords 2
        bibtex-autokey-titleword-length 2
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titleword-ignore nil
        bibtex-autokey-before-presentation-function
        '(lambda (x) (downcase (pyim-hanzi2pinyin-simple x)))))

(defun eh-bibtex-english-autokey-setup ()
  "Bibtex 英文文献 autokey 生成规则。"
  (setq bibtex-autokey-names 1
        bibtex-autokey-name-separator ""
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titlewords 3
        bibtex-autokey-titleword-length 5
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titleword-ignore
        '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
          "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")))

(defun eh-bibtex-autokey-get-title (orig-fun &rest args)
  (let ((case-fold-search t)
        (titlestring (bibtex-autokey-get-field "title")))
    (if (string-match-p "\\cc" titlestring)
        (eh-bibtex-chinese-autokey-setup)
      (eh-bibtex-english-autokey-setup))
    (apply orig-fun args)))

(advice-add 'bibtex-autokey-get-title :around #'eh-bibtex-autokey-get-title)

(defun eh-bibtex-wash-field (field)
  "Wash the content of field"
  (goto-char begin)
  (let ((field-content (bibtex-autokey-get-field field))
        (field-position (bibtex-search-forward-field field t)))
    (when field-position
      (goto-char (car (cdr field-position)))
      (bibtex-kill-field))
    (bibtex-make-field
     (list field nil
           (eh-wash-text
            field-content
            eh-bibtex-abstact-fill-column
            (+ bibtex-text-indentation 1 )) nil) t)))

(defun eh-bibtex-reformat ()
  (interactive)
  (goto-char (point-min))
  ;; Clean elide blank lines of entries,
  ;; which make abstrack field look beautiful
  (save-restriction
    (bibtex-map-entries
     (lambda (key begin end)
       (let ((case-fold-search t)
             (entry-type (bibtex-type-in-head)))
         (save-excursion
           ;; Add language field
           (goto-char begin)
           (let ((language-field (bibtex-search-forward-field "language" t)))
             (when language-field
               (goto-char (car (cdr language-field)))
               (bibtex-kill-field)))
           (when (string-match-p "\\cc+" (bibtex-autokey-get-field "title"))
             (bibtex-make-field '("language" nil "Chinese" nil) t))

           ;; Add alias field
           (goto-char begin)
           (let ((alias-field (bibtex-search-forward-field "alias" t))
                 (title (bibtex-autokey-get-field "title"))
                 (author (bibtex-autokey-get-field "author")))
             (when alias-field
               (goto-char (car (cdr alias-field)))
               (bibtex-kill-field))
             (bibtex-make-field
              (list "alias" nil
                    (replace-regexp-in-string
                     "\n" ""
                     (pyim-hanzi2pinyin-simple
                      (concat author ", " title) t)) nil) t))

           ;; Wash abstract field
           (eh-bibtex-wash-field "abstract")

           ;; Add autokey
           (goto-char begin)
           (let (auto-key)
             (re-search-forward (if (bibtex-string= entry-type "string")
                                    bbibtex-string-maybe-empty-head
                                  bibtex-entry-maybe-empty-head))
             (if (match-beginning bibtex-key-in-head)
                 (delete-region (match-beginning bibtex-key-in-head)
                                (match-end bibtex-key-in-head)))
             (setq auto-key (bibtex-generate-autokey))

             ;; Sometimes `bibtex-generate-autokey' returns an empty string
             (if (string= "" auto-key)
                 (setq auto-key "!NEED_EDIT"))
             (insert auto-key)
             (let ((bibtex-entry-format
                    ;; Don't add `realign' to this list
                    '(opts-or-alts  delimiters
                                    last-comma page-dashes unify-case inherit-booktitle
                                    braces strings sort-fields whitespace
                                    ;; numerical-fields
                                    )))
               (bibtex-clean-entry nil t))

             ;; Add keyhistory field
             (let ((keyhistory-field (bibtex-search-forward-field "keyhistory" t))
                   (keyhistory (bibtex-autokey-get-field "keyhistory")))
               (when keyhistory-field
                 (goto-char (car (cdr keyhistory-field)))
                 (bibtex-kill-field))
               (bibtex-make-field
                (list "keyhistory" nil
                      (mapconcat #'identity
                                 (delq "" (delete-dups
                                           (cons auto-key (split-string (or keyhistory "") "; "))))
                                 "; ") nil) t)))))))))

(provide 'eh-bibtex)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bibtex.el ends here
