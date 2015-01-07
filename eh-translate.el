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
(require 'chinese-yasdcv)
(setq yasdcv-chinese-wordsplit-command
      "echo %string | python -m jieba -q -d ' '")

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
                         (yasdcv--get-translate text x t)))
                      (split-string dict ",") "\n")))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("translate" . text)))

(global-set-key (kbd "C-c d d") 'yasdcv-translate-at-point)
(global-set-key (kbd "C-c d g") 'google-translate-at-point)
(global-set-key (kbd "C-c d b") 'eh-bing-translate-at-point)

(provide 'eh-translate)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-translate.el ends here
