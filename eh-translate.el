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
				    "\n* %s\n" " %2d. %s ")))
	(cond ((org-not-nil phonetic) text-phonetic)
	      ((org-not-nil details)
	       (if (string= detailed-translation "")
		   translation
		 detailed-translation))
	      (t translation))))))

;;; stardict
(require 'org)

;; stardict command
(setq eh-sdcv-command "sdcv --utf8-output --utf8-input -n %word")
;; Chinese word split system command
(setq eh-scws-command "/usr/local/scws/bin/scws -c utf-8 -N -A -I -d /usr/local/scws/etc/dict.utf8.xdb -i %word")

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

(defun eh-sdcv-get-translate (word)
  "Return sdcv translate string of `word'"
  (let* ((translate
	  (shell-command-to-string
	   (replace-regexp-in-string
	    "%word" word eh-sdcv-command))))
    (with-temp-buffer
      (insert translate)
      (goto-char (point-min))
      (while (re-search-forward "-->\\(.*\\)\n-->" nil t)
	(replace-match "* \\1\n** "))
      (goto-char (point-min))
      (while (re-search-forward "-->" nil t)
	(replace-match "** "))
      (goto-char (point-min))
      (kill-line 1)
      (when (featurep 'org)
	(org-mode)
	(org-indent-region (point-min) (point-max)))
      (buffer-string))))

(defun eh-sdcv-translate-at-point ()
  "Translate current word at point with sdcv"
  (interactive)
  (let* ((word (or (if mark-active
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (eh-current-word)) ""))
	 (translate (eh-sdcv-get-translate word)))
    (if translate
	(eh-sdcv-buffer-output-translation translate)
      (message "Can't translate the word: %s" word))))


(defun eh-sdcv-buffer-output-translation (translate-text)
  "Output sdcv translation to the temp buffer."
  (let ((buffer-name "*Stardict Output*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
	(org-mode))
      (insert translate-text))))

;;; ob-gtranslate
(require 'org)
(require 'ob)
(require 'ob-translate)

(defun org-babel-execute:translate (text params)
  "org-babel translation hook."
  (let ((src (or (cdr (assoc :from params))
		 google-translate-default-source-language))
	(dest (or (cdr (assoc :to params))
		  google-translate-default-target-language))
	(phonetic (cdr (assoc :phonetic params)))
	(details (or (cdr (assoc :details params)) t))
	(dict (or (cdr (assoc :dict params)) "google")))
    (cond ((string= dict "google")
	   (if (string-match "," dest)
	       (mapcar (lambda (subdest)
			 (list subdest
			       (eh-google-translate-to-string src subdest text phonetic details)))
		       (split-string dest ","))
	     (eh-google-translate-to-string src dest text phonetic details)))
	  ((string= dict "stardict")
	   (eh-sdcv-get-translate text)))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("translate" . text)))

(global-set-key (kbd "C-c d") 'google-translate-at-point)
(global-set-key (kbd "C-c D") 'eh-sdcv-translate-at-point)

(provide 'eh-translate)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-translate.el ends here

