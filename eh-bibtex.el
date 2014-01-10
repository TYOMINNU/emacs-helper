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

;;; Code:
(require 'bibtex)
(require 'ebib)
(require 'eh-hanzi2pinyin)

;; bibtex autokey setting
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-titleword-length 0)
(setq bibtex-autokey-titleword-separator "")
(setq bibtex-autokey-name-year-separator "")
(setq bibtex-autokey-year-title-separator "")
(setq bibtex-autokey-before-presentation-function '(lambda (x) (downcase (eh-hanzi2pinyin-simple x))))


;; ebib window setting
(setq ebib-layout 'full)
(setq ebib-window-vertical-split nil)
(setq ebib-width 80)
(setq ebib-index-window-size 10)
(setq ebib-uniquify-keys nil)

;; ebib index buffer display
(defun ebib-display-entry (entry-key)
  "Display ENTRY-KEY in the index buffer at POINT."
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    (with-ebib-buffer-writable
      (insert (format "%-20s %-15s %s\n"
                      entry-key
		      (car (split-string
			    (or (ebib-db-get-field-value 'author entry-key ebib-cur-db 'noerror 'unbraced)
				"    ") "[ \t\n]+and[ \t\n]+\\|," ))
		      (let ((title (ebib-db-get-field-value 'title entry-key ebib-cur-db 'noerror 'unbraced)))
			(if (> (length title) 60)
			    (concat (substring title 0 60) "...")
			  title))
		      "")))))

(defun eh-ebib-generate-all-entries-autokeys ()
  "generate autokeys for all entries and overwrite the exists."
  (interactive)
  (ebib-execute-when
    ((entries)
     (ebib-goto-first-entry)
     (while (ebib-next-elem (ebib-cur-entry-key) ebib-cur-keys-list)
       (ebib-generate-autokey)
       (ebib-next-entry)))))

(defun eh-add-bibtex-language-field ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\( +\\)language += +{\\([^}{]+\\)}, *\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\( +\\)title += +{[^}{]+}," nil t)
    (let ((title (match-string 0))
	  (space (match-string 1)))
      (if (string-match-p "\\cc+" title)
	  (replace-match (concat title "\n" space "language  = {zh},")) t))))

(defun eh-convert-bibtex-key-to-pinyin ()
  "Convert bibtex key to pinyin"
  (interactive)
  (if (featurep 'eh-hanzi2pinyin)
      (progn
	(goto-char (point-min))
	(while (re-search-forward "\\@\\([a-zA-Z]+\\){\\([^,]+\\)," nil t)
	  (let ((bibtype (match-string 1))
		(string (match-string 2)))
	    (replace-match (concat "@" bibtype "{" (replace-regexp-in-string " +" "" (downcase (eh-hanzi2pinyin-simple string))) ",") t))))
    (message "Can't find eh-hanzi2pinyin")))

(defun eh-convert-cite-key-to-pinyin ()
  "Convert bibtex key to pinyin"
  (interactive)
  (if (featurep 'eh-hanzi2pinyin)
      (progn
	(goto-char (point-min))
	(while (re-search-forward "\\\\cite{\\([^{}]+\\)}" nil t)
	  (let ((string (match-string 1)))
	    (replace-match (concat "\\\\cite{" (downcase (eh-hanzi2pinyin-simple string)) "}") t))))
    (message "Can't find eh-hanzi2pinyin")))

;; ebib mode keybinding
(ebib-key index "\C-xb" (lambda ()
			  (interactive)
			  (ebib-leave-ebib-windows)
			  (ibuffer)))
(ebib-key index "\C-xk" ebib-leave-ebib-windows)
(ebib-key index "\C-xq" ebib-quit)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bibtex.el ends here
