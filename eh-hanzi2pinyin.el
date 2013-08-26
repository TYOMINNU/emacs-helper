;;; eh-hanzi2pinyin.el --- Tumashu's emacs configuation

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
(require 'cl)

(defun eh-build-hanzi2pinyin-hash-table ()
  "Build a hanzi to pinyin hashtable with the quail date file: quail/PY.el"
  (let ((hanzi2pinyin-hash-table (make-hash-table)))
    (with-temp-buffer
      (insert-file-contents (locate-library "quail/PY.el"))
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*([[:space:]]*\"\\([a-z]+\\)\"[[:space:]]*\"\\([^\"]+\\)\"[[:space:]]*)[[:space:]]*$" nil t)
        (let ((pinyin (match-string 1))
              (hanzi-string (match-string 2)))
          (loop for hanzi across hanzi-string
                do (progn
                     (let ((existing (gethash hanzi hanzi2pinyin-hash-table)))
                       (puthash hanzi
                                (if existing
                                    (cons pinyin existing)
                                  (list pinyin))
                                hanzi2pinyin-hash-table)))))))
    hanzi2pinyin-hash-table))

(defvar eh-hanzi2pinyin-table (eh-build-hanzi2pinyin-hash-table)
  "hash table used to convert hanzi string to pinyin string")

(defun eh-hanzi2pinyin (string &optional shou-zi-mu)
  "Convert hanzi string to pinyin, if set `shou-zi-mu' to t,only
get pinyin shouzimu string"
  (let* ((pinyin-list (mapcar
                       (lambda (x) (if (gethash x eh-hanzi2pinyin-table)
                                   (gethash x eh-hanzi2pinyin-table)
                                 (list (char-to-string x)))) string)))
    (mapconcat 'identity
               (remove-duplicates
                (let ((result '("")))
                 (loop for i in pinyin-list
                       do (setq result
                                (loop for j in i
                                      append (loop for k in result
                                                   collect (concat k (if shou-zi-mu (substring j 0 1) j)))))) result)
                :test (lambda (x y) (or (null y) (equal x y)))
                :from-end t) " ")))

(defun org-contacts-add-pinyin-alias ()
  "Add pinyin alias to all head of current buffer"
  (interactive)
  (require 'org)
  (org-map-entries '(lambda () (let ((pinyin-alias (eh-hanzi2pinyin (org-get-heading 1 1) t)))
                            (org-set-property org-contacts-alias-property pinyin-alias)))))

(provide 'eh-hanzi2pinyin)


;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-hanzi2pinyin.el ends here
