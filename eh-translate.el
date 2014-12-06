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

;;; bing-translate
(setq eh-bing-url-format "http://cn.bing.com/dict/search?q=%s")
(setq eh-bing-translate-timer nil)

(defun eh-bing-translate-at-point ()
  "Translate current word at point with sdcv"
  (interactive)
  (save-restriction
    (let* ((word (or (if mark-active
			 (buffer-substring-no-properties
			  (region-beginning) (region-end))
		       (eh-current-word)) ""))
	   (url (format eh-bing-url-format word)))
      (set-buffer (get-buffer-create "*bing-eww*"))
      (let ((inhibit-read-only t))
	(remove-overlays)
	(erase-buffer))
      (url-retrieve url 'eww-render
		    (list url nil (current-buffer)))
      (unless (eq major-mode 'eww-mode)
	(eww-mode))
      (when eh-bing-translate-timer
	(cancel-timer eh-bing-translate-timer))
      (setq eh-bing-translate-timer
	    (run-with-timer
	     2 nil
	     '(lambda ()
		(switch-to-buffer-other-window "*bing-eww*")
		(setq header-line-format nil)
		(goto-char (point-min))
		(forward-line 18)
		(eh-eww-narrow-to-region (point) (point-max))))))))

;;; google-translate
(require 'google-translate)
(require 'google-translate-smooth-ui)

(setq google-translate-default-source-language "auto")
(setq google-translate-default-target-language "zh-CN")
(setq google-translate-show-phonetic nil)
(setq google-translate-base-url
      "http://translate.google.cn/translate_a/t")
(setq google-translate-listen-url
      "http://translate.google.cn/translate_tts")
(setq google-translate-output-destination nil)
(setq google-translate-translation-directions-alist '(("en" . "zh-CN")))

(defun eh-google-translate-to-string (source-language target-language text &optional phonetic details)
  (let* ((json (google-translate-request source-language
					 target-language
					 text)))
    (if (null json)
	(message "Nothing to translate.")
      (let* ((text-phonetic (google-translate-json-text-phonetic json))
	     (translation (google-translate-json-translation json))
	     (detailed-translation (google-translate-json-detailed-translation json))
	     (detailed-translation (google-translate--detailed-translation
				    detailed-translation
				    translation
				    "\n* %s\n" " %2d.%s")))
	(cond ((org-not-nil phonetic) text-phonetic)
	      ((org-not-nil details)
	       (if (string= detailed-translation "")
		   translation
		 detailed-translation))
	      (t translation))))))

;;; stardict
(require 'org)

;; stardict command
(setq eh-sdcv-command "sdcv --non-interactive --utf8-output --utf8-input --use-dict \"%dict\" \"%word\"")
;; Chinese word split system command
(setq eh-scws-command "/usr/local/scws/bin/scws -c utf-8 -N -A -I -d /usr/local/scws/etc/dict.utf8.xdb -i %word")

(setq eh-sdcv-dicts
      '(
	("jianminghy" "简明汉英词典" eh-sdcv-output-clean-powerword2007 nil)
	("jianmingyh" "简明英汉词典" eh-sdcv-output-clean-powerword2007 nil)

	("lanconghy"  "懒虫简明汉英词典" nil nil)
	("lancongyh"  "懒虫简明英汉词典" nil nil)


	("xdictyh"    "XDICT英汉辞典" nil t)
	("xdicthy"    "XDICT汉英辞典" nil t)

	("xiandai"    "现代英汉综合大辞典" eh-sdcv-output-clean-powerword2007 t)

	("niujing"    "牛津高阶英汉双解"  eh-sdcv-output-clean-oald nil)
	(""           "英文相关词典" eh-sdcv-output-clean-powerword2007 nil)

	("langdaohy"  "朗道汉英字典5.0" eh-sdcv-output-clean-langdao nil)
	("langdaoyh"  "朗道英汉字典5.0" eh-sdcv-output-clean-langdao nil)

	("21shiji"    "21世纪英汉汉英双向词典" eh-sdcv-output-clean-21cen nil)
	("21shjikj"   "21世纪双语科技词典" nil nil)

	(""           "新世纪英汉科技大词典" nil nil)
	(""           "新世纪汉英科技大词典" nil nil)

	(""           "现代商务汉英大词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "英汉双解计算机词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "汉语成语词典" eh-sdcv-output-clean-chengyu t)
	(""           "高级汉语大词典" nil nil)
	(""           "现代汉语词典" nil nil)

	(""           "Cantonese Simp-English" nil nil)
	(""           "英汉进出口商品词汇大全" nil nil)

	(""           "中国大百科全书2.0版" nil t)
	(""           "CEDICT汉英辞典" nil nil)
	(""           "英文字根字典" nil t)

	(""           "湘雅医学专业词典" nil nil)

	(""           "[七国语言]英汉化学大词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[七国语言]英汉数学大词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[七国语言]英汉公共大词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[七国语言]英汉医学大词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[七国语言]英汉信息大词典" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[七国语言]英汉生物学大词典" eh-sdcv-output-clean-powerword2007 nil)

	(""           "[名词委审定]英汉铁道科技名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英细胞生物学名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英数学名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(七, 整形、美容、皮肤、康复)" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(四, 心血管病学等)" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(一, 妇产科学)" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英生物化学名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]英汉生物化学名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(二, 口腔学)" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(六, 外科)" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英人体解剖学名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英药学名词" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(三, 遗传学等)" eh-sdcv-output-clean-powerword2007 nil)
	(""           "[名词委审定]汉英医学名词(五, 眼科学)" eh-sdcv-output-clean-powerword2007 nil)))

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
		 (replace-regexp-in-string
		  "%word" (or word ",") eh-scws-command))))))
	word "")))

(defun eh-sdcv-output-clean-powerword2007 ()
  "清理现代英汉综合大辞典的输出"
  (goto-char (point-min))
  (while (re-search-forward "<\\([^><]+\\)><!\\[CDATA\\[\\([^><]+\\)\\]\\]><\\([^><]+\\)>" nil t)
    (replace-match "\\1: \\2"))

  (goto-char (point-min))
  (while (re-search-forward "\n+例句原型: *\\([^><]+\\)\n例句解释: *\\([^><]+\\)\n+" nil t)
    (replace-match "- \\1 \\2"))

  (goto-char (point-min))
  (while (re-search-forward " +索引类型='.+'" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "<例句s +" nil t)
    (replace-match "- "))

  (goto-char (point-min))
  (while (re-search-forward "<[^><]+>\\|^ *]" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "词典音标.*\n" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "单词原型: +" nil t)
    (replace-match "** "))

  (goto-char (point-min))
  (while (re-search-forward "&L{\\(.+\\)}" nil t)
    (replace-match "\\1"))

  (goto-char (point-min))
  (while (search-forward "}&L{" nil t)
    (replace-match ", "))

  (mapcar (lambda (x)
	    (goto-char (point-min))
	    (while (search-forward x nil t)
	      (replace-match "")))
	  '("单词词性: " "解释项: "
	    "[.]]>" "}]]>" "ly]]>" "[F]]>"
	    "]]>" "<>" "<![CDATA[" "\n[")))

(defun eh-sdcv-output-clean-oald ()
  "清理牛津高阶英汉双解词典的输出"
  (goto-char (point-min))

  (goto-char (point-min))
  (while (re-search-forward "^\\* *" nil t)
    (replace-match "**** "))

  (goto-char (point-min))
  (while (re-search-forward "/\\(.+\\)/\n+\\(.+\\)" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "\\(^[0-9]\\) +\\(([a-z])\\) +" nil t)
    (replace-match "\\1 Good good study, day day up ......\n\\2"))

  (goto-char (point-min))
  (while (re-search-forward "^[0-9] *" nil t)
    (replace-match "** "))

  (goto-char (point-min))
  (while (re-search-forward "^([a-z]) *" nil t)
    (replace-match "*** ")))

(defun eh-sdcv-output-clean-langdao ()
  "清理朗道英汉字典的输出"
  (goto-char (point-min))
  (while (search-forward "*" nil t)
    (replace-match "")))

(defun eh-sdcv-output-clean-chengyu ()
  "清理成语大词典的输出"
  (goto-char (point-min))
  (while (re-search-forward "<[^><]+>\\|\\[[^><]+>" nil t)
    (replace-match "")))

(defun eh-sdcv-output-clean-21cen ()
  "清理21世纪英汉汉英双向词典的输出"
  (goto-char (point-min))
  (while (re-search-forward "<<\\([^><]+\\)>>" nil t)
    (replace-match "** \\1"))

  (goto-char (point-min))
  (while (re-search-forward "\\(^[0-9]\\) +\\([a-z]\\.\\) +" nil t)
    (replace-match "\\1 Good good study, day day up ......\n\\2"))

  (goto-char (point-min))
  (while (re-search-forward "^[0-9] *" nil t)
    (replace-match "*** "))

  (goto-char (point-min))
  (while (re-search-forward "^[a-z]\\. *" nil t)
    (replace-match "**** ")))

(defun eh-sdcv-output-clean-common ()
  (goto-char (point-min))
  (while (re-search-forward "-->\\(.*\\)\n-->\\(.*\\)" nil t)
    (replace-match "* \\1 (\\2)"))

  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))

  (goto-char (point-min))
  (kill-line 1))

(defun eh-sdcv-get-sdcv-output (word dict &optional force)
  "Get sdcv translate output using dict"
  (let* ((dict-name (nth 1 dict))
	 (filter (nth 2 dict))
	 (enable (nth 3 dict))
	 (command  (replace-regexp-in-string
		    "%dict" dict-name
		    (replace-regexp-in-string
		     "%word" word eh-sdcv-command))))
    (when (or enable force)
      (with-temp-buffer
	(insert (shell-command-to-string command))
	(when filter (funcall filter))
	(funcall 'eh-sdcv-output-clean-common)
	(goto-char (point-min))
	(when (re-search-forward word nil t)
	  (buffer-string))))))

(defun eh-sdcv-get-translate (word &optional dict-key indent)
  "Return sdcv translate string of `word'"
  (with-temp-buffer
    (insert (mapconcat
	     (lambda (dict)
	       (cond ((or (not dict-key)
			  (=  (length dict-key) 0))
		      (eh-sdcv-get-sdcv-output word dict))
		     ((or (string= dict-key (nth 0 dict))
			  (string-match-p dict-key (nth 1 dict)))
		      (eh-sdcv-get-sdcv-output word dict t))))
	     eh-sdcv-dicts ""))
    (when (and indent (featurep 'org))
      (org-mode)
      (org-indent-region (point-min) (point-max)))
    (buffer-string)))

(defun eh-sdcv-translate-at-point ()
  "Translate current word at point with sdcv"
  (interactive)
  (let* ((word (or (if mark-active
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (eh-current-word)) ""))
	 (translate (eh-sdcv-get-translate word)))
    (if (or (not translate) (string= translate ""))
	(message "Can't translate the word: %s" word)
      (eh-sdcv-buffer-output-translation translate))))

(defun eh-sdcv-buffer-output-translation (translate-text)
  "Output sdcv translation to the temp buffer."
  (let ((buffer-name "*Stardict Output*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
	(org-mode)
	(org-indent-mode))
      (insert translate-text))))

;;; ob-gtranslate
(require 'org)
(require 'ob)

(defun org-babel-execute:translate (text params)
  "org-babel translation hook."
  (let ((src (or (cdr (assoc :from params))
		 google-translate-default-source-language))
	(dest (or (cdr (assoc :to params))
		  google-translate-default-target-language))
	(phonetic (cdr (assoc :phonetic params)))
	(details (or (cdr (assoc :details params)) t))
	(dict (or (cdr (assoc :dict params)) ""))
	(method (or (cdr (assoc :method params)) "google")))
    (cond ((string= method "google")
	   (if (string-match "," dest)
	       (mapcar (lambda (subdest)
			 (list subdest
			       (eh-google-translate-to-string src subdest text phonetic details)))
		       (split-string dest ","))
	     (eh-google-translate-to-string src dest text phonetic details)))
	  ((string= method "stardict")
	   (mapconcat (lambda (x)
			(replace-regexp-in-string
			 "^" " "
			 (eh-sdcv-get-translate text x t)))
		      (split-string dict ",") "\n")))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("translate" . text)))

(global-set-key (kbd "C-c d d") 'eh-sdcv-translate-at-point)
(global-set-key (kbd "C-c d g") 'google-translate-at-point)
(global-set-key (kbd "C-c d b") 'eh-bing-translate-at-point)

(provide 'eh-translate)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-translate.el ends here
