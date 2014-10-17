;;; eh-translate.el --- Tumashu's emacs configuation

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

;;; Code:

;;; google-translate
(require 'google-translate)
(require 'google-translate-smooth-ui)

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "zh-CN")
(setq google-translate-show-phonetic nil)
(setq google-translate-base-url
      "http://translate.google.cn/translate_a/t")
(setq google-translate-listen-url
      "http://translate.google.cn/translate_tts")
(setq google-translate-output-destination nil)
(setq google-translate-translation-directions-alist '(("en" . "zh-CN")))

;;; stardict
(setq eh-sdcv-chinese2english-command "sdcv --utf8-output --utf8-input -n -u XDICT汉英辞典")
(setq eh-sdcv-english2chinese-command "sdcv -n -u XDICT英汉辞典")
;; Chinese word split system command
(setq eh-scws-command "/usr/local/scws/bin/scws -c utf-8 -N -A -I -d /usr/local/scws/etc/dict.utf8.xdb -i")

(defun eh-current-word ()
  "Get English word or Chinese word at point"
  (let ((word (current-word t t))
	(current-char (string (preceding-char))))
    (or (car (remove-if-not
	      #'(lambda (x) (string-match-p current-char x))
	      (split-string
	       (replace-regexp-in-string
		"/[a-zA-z]+ +" " "
		(shell-command-to-string
		 (concat eh-scws-command " " (or word ",")))))))
	word "")))

(defun eh-sdcv-get-translate (word)
  "Return a translations list of `word'"
  (let* ((translate
	  (if (string-match-p "\\cc" word)
	      (shell-command-to-string (concat eh-sdcv-chinese2english-command " " word))
	    (shell-command-to-string (concat eh-sdcv-english2chinese-command " " word))))
	 (string-regexp (concat "-->" word)))
    (when (string-match-p string-regexp translate)
      (remove-duplicates
       (mapcar #'(lambda (x) (replace-regexp-in-string "^ +\\| +$" "" x))
	       (split-string (eh-wash-sdcv-output translate) "[,;]"))
       :test (lambda (x y) (or (= 0 (length y)) (equal x y)))
       :from-end t))))

(defun eh-wash-sdcv-output (str)
  "Wash sdcv output"
  (replace-regexp-in-string
   "^ +\\| +$" ""
   (replace-regexp-in-string
    ";+" "; "
    (replace-regexp-in-string
     " +" " "
     (replace-regexp-in-string
      "^;+\\|^ +" ""
      (replace-regexp-in-string
       "[\nˊ，。；：！？“]+\\|[a-zA-Z]+\\." ";"
       (replace-regexp-in-string
	".*\n*-->.+\\|\\[.+\\]" ""
	str)))))))

(defun eh-replace-word-with-translate ()
  (interactive)
  (let* ((word (or (if mark-active
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (eh-current-word)) ""))
	 (translate (eh-sdcv-get-translate word)))
    (if translate
	(let ((string (popup-menu* translate)))
	  (if (and (not mark-active) (string-match-p "\\cc" word))
	      (let ((length (length word)))
		(backward-char length)
		(re-search-forward word nil t)
		(replace-match (concat string " ")))
	    (progn  (eh-mark-word)
		    (delete-region (region-beginning) (region-end))
		    (insert string))))
      (message "Can't translate the word: %s" word))))

(defun eh-mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp (line-beginning-position)))
      (skip-syntax-forward "w")
      (set-mark (point))
      (skip-syntax-backward "w"))))

(global-set-key (kbd "C-c d") 'google-translate-at-point)
(global-set-key (kbd "C-c D") 'eh-replace-word-with-translate)

(provide 'eh-translate)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-translate.el ends here

