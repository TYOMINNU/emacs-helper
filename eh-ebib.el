;;; eh-ebib.el --- Tumashu's emacs configuation

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
(require 'reftex)
(require 'ebib)
(require 'eh-hanzi2pinyin)
(require 'phi-search)

;; bibtex autokey rule
;; the below will generate a auto named : xulinling2013
;; [3] 徐琳玲. P公司标准成本制度研究[D]. 华东理工大学, 2013.
(setq bibtex-autokey-names 1)
(setq bibtex-autokey-name-separator "")
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-titleword-length 0)
(setq bibtex-autokey-titleword-separator "")
(setq bibtex-autokey-name-year-separator "")
(setq bibtex-autokey-year-title-separator "")
(setq bibtex-autokey-before-presentation-function
      '(lambda (x) (downcase (eh-hanzi2pinyin-simple x))))

;; ebib window setting
(setq ebib-layout 'full)
(setq ebib-window-vertical-split nil)
(setq ebib-width 80)
(setq ebib-index-window-size 15)

;; scale the font height in entry buffer
(setq eh-ebib-entry-buffer-text-scale-amount 2.0)
(setq ebib-uniquify-keys nil)

;; only show abstract in entry buffer
(setq eh-ebib-entry-buffer-only-show-abstact t)
(setq eh-ebib-entry-buffer-abstact-fill-column 80)

;; record the last opened bibfile name
(setq eh-ebib-recently-opened-bibfile nil)

;; If this varible is `t', index buffer will highlight lines instead of autokey word
;; setting this varible to *a list of field* is *useless* in my configure
(setq ebib-index-display-fields t)

(defun eh-ebib-select-and-popup-entry ()
  (interactive)
  (setq eh-ebib-entry-buffer-only-show-abstact
	(not eh-ebib-entry-buffer-only-show-abstact))
  (ebib-select-and-popup-entry))

(defun eh-ebib-get-abstract-field  (field key &optional match-str db)
  "Get abstract field of the entry"
  (or db (setq db ebib-cur-db))
  (let* ((case-fold-search t)
         (value (ebib-db-get-field-value field key db 'noerror nil 'xref))
         (abstract-string (if (car value)
			      (copy-sequence (car value)))))
    (eh-wash-text (or abstract-string "")
		  eh-ebib-entry-buffer-abstact-fill-column 0)))

;; ebib entry buffer format setting
(defadvice ebib-format-fields (around eh-ebib-format-fields
				      (key &optional match-str db) activate)
  ;; show cursor in entry buffer
  (setq cursor-type t)
  ;; reset font size to default
  (text-scale-mode 0)
  (if eh-ebib-entry-buffer-only-show-abstact
      (let ((text-scale-mode-amount eh-ebib-entry-buffer-text-scale-amount))
	;; add additional 0.2 pixels between two lines
	(setq line-spacing 0.2)
	;; increase the text size of the entry buffer
	(text-scale-mode)
	(visual-line-mode t)
	(insert (eh-ebib-get-abstract-field 'abstract key match-str)))
    (progn (toggle-truncate-lines t)
	   ad-do-it)))

(defadvice ebib-edit-entry (around eh-ebib-edit-entry
				   () activate)
  "Edits the current BibTeX entry."
  (when eh-ebib-entry-buffer-only-show-abstact
    (eh-ebib-select-and-popup-entry))
  ad-do-it)

(defadvice ebib-edit-field (around eh-ebib-edit-field
				   (&optional pfx) activate)
  "Edits the current BibTeX entry."
  (when (not eh-ebib-entry-buffer-only-show-abstact)
    ad-do-it))

;; ebib index buffer format setting
(defun ebib-display-entry (entry-key)
  "Display ENTRY-KEY in the index buffer at POINT."
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    (with-ebib-buffer-writable
      (setq cursor-type t)
      (insert (format "%-20s %-15s %-45s %s\n"
                      entry-key
		      ;; author
		      (car (split-string
			    (or (ebib-db-get-field-value 'author entry-key ebib-cur-db 'noerror 'unbraced)
				"  ") "[ \t\n]+and[ \t\n]+\\|," ))
		      ;; title
		      (let ((title (ebib-db-get-field-value 'title entry-key ebib-cur-db 'noerror 'unbraced)))
			(if (> (string-width title) 40)
			    (if (string-match-p "\\cc+" title)
				(concat (substring title 0 20) "...")
			      (concat (substring title 0 40) "..."))
			  title))
		      ;; journal publisher or school
		      (or (ebib-db-get-field-value 'journal entry-key ebib-cur-db 'noerror 'unbraced)
			  (ebib-db-get-field-value 'publisher entry-key ebib-cur-db 'noerror 'unbraced)
			  (ebib-db-get-field-value 'school entry-key ebib-cur-db 'noerror 'unbraced)
			  "——————————")
		      "")))))

(defun eh-ebib-get-matched-files (files-list match-str)
  (let ((match-string (replace-regexp-in-string "[ +-=_]+" "" match-str)))
    (if (string= match-string "")
	""
      (delete-if
       (lambda (s)
	 (let ((case-fold-search t)
	       (string (replace-regexp-in-string "[ +-=_]+" "" s)))
	   (not (or (string-match match-string string)
		    (if (featurep 'eh-hanzi2pinyin)
			(string-match match-string (eh-hanzi2pinyin string)))))))
       files-list))))

(defun eh-ebib-view-file ()
  (interactive)
  (ebib-execute-when
    ((entries)
     (let* ((name-string
	     (car (split-string
		   (or (car (ebib-db-get-field-value 'author (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced 'xref))
		       "  ") "[ \t\n]+and[ \t\n]+\\|," )))
	    (all-files (eh-directory-files-recursively (file-name-directory (ebib-db-get-filename ebib-cur-db)) t))
	    (files-matched (eh-ebib-get-matched-files all-files name-string)))
       (cond
	((> (length files-matched) 1)
	 (start-process "" nil "xdg-open" (ido-completing-read "Open file:" files-matched)))
	((= (length files-matched) 1)
	 (message "Opening file: %s" (car files-matched))
	 (start-process "" nil "xdg-open" (car files-matched)))
	((< (length files-matched) 1)
	 (message "Can't find the corresponding file")))))))

(defun eh-ebib ()
  "Open ebib then search the marked string"
  (interactive)
  (let* ((files-list (eh-directory-files-recursively "." t ".bib$"))
	 (file
	  (or
	   ;; (if (buffer-file-name)
	   ;;     (car (eh-reftex-get-bibfile-list)))
	   (when (and eh-ebib-recently-opened-bibfile
		      (y-or-n-p (format "Load recently opened bibfile (%s)?  "
					eh-ebib-recently-opened-bibfile)))
	     eh-ebib-recently-opened-bibfile)
	   (when files-list
	     (ido-completing-read "Open bibfile:" files-list))
	     (ido-read-file-name "Open bibfile:" (car ebib-file-search-dirs))))
	 (word (or (current-word nil t) ""))
	 (length (length word))
	 (key (if mark-active
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(if (and (string-match-p "\\cc+" word) (> length 3))
		    (buffer-substring-no-properties (- (point) 2) (point))
		  word))))
    (setq eh-ebib-push-buffer (current-buffer))
    (ebib file)
    (setq eh-ebib-recently-opened-bibfile file)
    (when key
      (phi-search)
      (insert key)
      (phi-search-complete)
      (ebib-select-and-popup-entry))))

(defun eh-reftex-get-bibfile-list ()
  "Return list of bibfiles for current document.
When using the chapterbib or bibunits package you should either
use the same database files everywhere, or separate parts using
different databases into different files (included into the mater file).
Then this function will return the applicable database files."

  ;; Ensure access to scanning info
  (reftex-access-scan-info)
  (or
   ;; Try inside this file (and its includes)
   (cdr (reftex-last-assoc-before-elt
         'bib (list 'eof (buffer-file-name))
         (member (list 'bof (buffer-file-name))
                 (symbol-value reftex-docstruct-symbol))))
   ;; Try after the beginning of this file
   (cdr (assq 'bib (member (list 'bof (buffer-file-name))
                           (symbol-value reftex-docstruct-symbol))))
   ;; Anywhere in the entire document
   (cdr (assq 'bib (symbol-value reftex-docstruct-symbol)))))

(defun eh-ebib-push-bibtex-key ()
  "Pushes the current entry to a LaTeX buffer.
The user is prompted for the buffer to push the entry into."
  (interactive)
  (let ((called-with-prefix (ebib-called-with-prefix)))
    (ebib-execute-when
      ((entries)
       (let ((citation-string
	      ;; 获取当前的entry-key
	      (if (ebib-db-marked-entries-p ebib-cur-db)
		  (mapconcat #'(lambda (key)
				 key)
			     (ebib-db-list-marked-entries ebib-cur-db)
			     ", ")
		(ebib-cur-entry-key))))
	 ;; 将citation-string插入到eh-ebib-push-buffer变量所
	 ;; 对应的buffer, (调用eh-ebib命令时,会设置eh-ebib-push-buffer变量)
	 (when citation-string
	   (with-current-buffer eh-ebib-push-buffer
	     (let* ((current-point (point))
		    (point1
		     (progn
		       (goto-char current-point)
		       (search-forward "{" nil t)))
		    (point1 (if point1 point1 (+ 1 (point-max))))
		    (point2
		     (progn
		       (goto-char current-point)
		       (search-forward "}" nil t)))
		    (point3
		     (progn
		       (goto-char current-point)
		       (search-backward "{" nil t)))
		    (point4
		     (progn
		       (goto-char current-point)
		       (search-backward "}" nil t)))
		    (point4 (if point4 point4 -1)))
	       (goto-char current-point)
	       (if (and point2 point3 (> point1 point2) (> point3 point4))
		   (let ((old-cite (buffer-substring-no-properties (+ point3 1) (- point2 1))))
		     (goto-char (- point2 1))
		     (insert
		      (if (> (length (replace-regexp-in-string " " "" old-cite)) 0)
			  (concat ", " citation-string)
			citation-string)))
		 (insert (format "\\cite{%s}" citation-string)))))
	   (message "Pushed entries to buffer %s" eh-ebib-push-buffer)
	   (setq eh-ebib-push-buffer nil)
	   ;; 隐藏ebib窗口
	   (ebib-leave-ebib-windows))))
      ((default)
       (beep)))))


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
	  eh-ebib-entry-buffer-abstact-fill-column
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
		     (eh-hanzi2pinyin-simple
		      (concat author ", " title) t)) nil) t))

	   ;; Wash abstract field
	   (eh-bibtex-wash-field "abstract")
	   
	   ;; Add autokey
	   (goto-char begin)
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
		  '(opts-or-alts numerical-fields delimiters
				 last-comma page-dashes unify-case inherit-booktitle
				 braces strings sort-fields whitespace)))
	     (bibtex-clean-entry nil t))))))))

(defun eh-ebib-reformat-all-entries ()
  "1. Add language field to all entries.
   2. Add alias field to all entries.
   3. reformat all the entries"
  (interactive)
  (let* ((current-bib-file (ebib-db-get-filename ebib-cur-db)))
    (ebib-execute-when
      ((entries)
       (when (yes-or-no-p (concat (format "Reformat bibfile: %s  " current-bib-file)))
	 (ebib-save-current-database)
	 (message "Reformat ... ")
	 (with-current-buffer (find-file-noselect current-bib-file)
	   (goto-char (point-min))
	   (eh-bibtex-reformat)
	   (save-buffer)
	   (kill-buffer))
	 ;; reload the current database
	 (ebib-reload-database ebib-cur-db)
	 (ebib-set-modified nil)
	 (ebib-redisplay)
	 (message "Reformat complete")))
      ((default)
       (beep)))))

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
(ebib-key index "q" ebib-quit)
(ebib-key index "f" eh-ebib-view-file)
(ebib-key index "\C-c\C-c" eh-ebib-push-bibtex-key)
(ebib-key index [(control k)] eh-ebib-reformat-all-entries)
(ebib-key index [(return)] eh-ebib-select-and-popup-entry)

(provide 'eh-ebib)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-ebib.el ends here
