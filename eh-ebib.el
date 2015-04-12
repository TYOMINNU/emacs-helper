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
(require 'chinese-pyim)

;; org cite link setting
(org-add-link-type "cite" 'eh-ebib)

;; bibtex autokey rule
;; the below will generate a auto named : xulinling2013
;; [3] 徐琳玲. P公司标准成本制度研究[D]. 华东理工大学, 2013.

(setq bibtex-autokey-names 1)
(setq bibtex-autokey-name-separator "")

(setq bibtex-autokey-year-length 4)

(setq bibtex-autokey-name-year-separator "")
(setq bibtex-autokey-year-title-separator "")

(defun eh-bibtex-chinese-autokey-setup ()
  (setq bibtex-autokey-titlewords 2)
  (setq bibtex-autokey-titleword-length 2)
  (setq bibtex-autokey-titlewords-stretch 0)
  (setq bibtex-autokey-titleword-separator "_")
  (setq bibtex-autokey-titleword-ignore nil)
  (setq bibtex-autokey-before-presentation-function
        '(lambda (x) (downcase (pyim-hanzi2pinyin-simple x)))))

(defun eh-bibtex-english-autokey-setup ()
  (setq bibtex-autokey-titlewords 3)
  (setq bibtex-autokey-titleword-length 5)
  (setq bibtex-autokey-titlewords-stretch 0)
  (setq bibtex-autokey-titleword-separator "_")
  (setq bibtex-autokey-titleword-ignore
        '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
          "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")))

(defun eh-bibtex-autokey-get-title (orig-fun &rest args)
  (let ((case-fold-search t)
        (titlestring
         (bibtex-autokey-get-field "title")))
    (if (string-match-p "\\cc" titlestring)
        (eh-bibtex-chinese-autokey-setup)
      (eh-bibtex-english-autokey-setup))
    (apply orig-fun args)))

(advice-add 'bibtex-autokey-get-title :around #'eh-bibtex-autokey-get-title)

;; ebib window setting
(setq ebib-layout 'full)
(setq ebib-window-vertical-split nil)
(setq ebib-width 80)
(setq ebib-index-window-size 10)

;; ebib addition fields
(setq ebib-extra-fields
      '((BibTeX "keywords" "abstract" "timestamp" "file"  "url" "crossref" "annote" "doi")
        (biblatex "keywords" "abstract" "timestamp" "file"  "url" "crossref" "annote" "doi")))

;; allow the same keys
(setq ebib-uniquify-keys nil)

;; default column when wash bib file
(setq eh-ebib-entry-buffer-abstact-fill-column 80)

;; record the last opened bibfile name
(setq eh-ebib-recently-opened-bibfile nil)

;; record the last citation string
(setq eh-ebib-the-last-entry-key "")

;; If this varible is `t', index buffer will highlight lines instead of autokey word
;; setting this varible to *a list of field* is *useless* in my configure
(setq ebib-index-display-fields t)

(defun eh-ebib-get-abstract-field  (field key &optional match-str db)
  "Get abstract field of the entry"
  (or db (setq db ebib--cur-db))
  (let* ((case-fold-search t)
         (abstract-string (ebib-db-get-field-value field key db 'noerror nil 'xref)))
    (eh-wash-text (or abstract-string "") 80 0)))

(defun eh-ebib-quit-abstract-viewer ()
  (interactive)
  (if (and (eq ebib-layout 'index-only)
           ebib-popup-entry-window)
      (delete-window)
    (switch-to-buffer nil t))
  (ebib--pop-to-buffer 'index)
  (kill-buffer "*eh-ebib-abstract-viewer*"))

(defun eh-ebib-abstract-viewer ()
  (interactive)
  (ebib--execute-when
    ((entries)
     ;; if the current entry is the first entry,
     (let ((key (ebib--cur-entry-key))
           (wnd (selected-window))
           (buf (current-buffer)))
       (pop-to-buffer (generate-new-buffer "*eh-ebib-abstract-viewer*")
                      '((ebib--display-buffer-reuse-window
                         ebib--display-buffer-largest-window
                         display-buffer-pop-up-window
                         display-buffer-pop-up-frame))
                      t)
       (insert (eh-ebib-get-abstract-field 'abstract key))
       (goto-char (point-min))
       ;; show cursor in entry buffer
       (setq cursor-type t)
       ;; reset font size to default
       (text-scale-mode 0)
       ;; add additional 0.2 pixels between two lines
       (setq line-spacing 0.2)
       ;; enable view-mode
       (view-mode)
       (use-local-map
        (let ((map view-mode-map))
          (define-key map (kbd "q") 'eh-ebib-quit-abstract-viewer)
          map))))
    ((default)
     (beep))))

;; ebib index buffer format setting
(defun eh-ebib--display-entry-key (entry-key)
  "Display entry-key, title, journal, publisher and school in the index buffer at POINT. "
  (with-current-ebib-buffer 'index
    (with-ebib-buffer-writable
      (setq cursor-type t)
      (insert (format "%-30s %-15s %-45s %s\n"
                      entry-key
                      ;; author
                      (car (split-string
                            (or (ebib-db-get-field-value 'author entry-key ebib--cur-db 'noerror 'unbraced)
                                "  ") "[ \t\n]+and[ \t\n]+\\|," ))
                      ;; title
                      (let ((title (or (ebib-db-get-field-value 'title entry-key ebib--cur-db 'noerror 'unbraced) "")))
                        (if (> (string-width title) 40)
                            (if (string-match-p "\\cc+" title)
                                (concat (substring title 0 20) "...")
                              (concat (substring title 0 40) "..."))
                          title))
                      ;; journal publisher or school
                      (or (ebib-db-get-field-value 'journal entry-key ebib--cur-db 'noerror 'unbraced)
                          (ebib-db-get-field-value 'publisher entry-key ebib--cur-db 'noerror 'unbraced)
                          (ebib-db-get-field-value 'school entry-key ebib--cur-db 'noerror 'unbraced)
                          "——————————")
                      "")))))

(advice-add 'ebib--display-entry-key :override #'eh-ebib--display-entry-key)

(defun eh-ebib-get-matched-files (files-list match-str)
  (let ((match-string (replace-regexp-in-string "[ +-=_]+" "" match-str)))
    (if (string= match-string "")
        ""
      (delete-if
       (lambda (s)
         (let ((case-fold-search t)
               (string (replace-regexp-in-string "[ +-=_]+" "" s)))
           (not (or (string-match match-string string)
                    (if (featurep 'chinese-pyim)
                        (string-match match-string (pyim-hanzi2pinyin string)))))))
       files-list))))

(defun eh-ebib-view-file ()
  (interactive)
  (ebib--execute-when
    ((entries)
     (let* ((key (ebib--cur-entry-key))
            (db ebib--cur-db)
            (name-string
             (car (split-string
                   (or (ebib-db-get-field-value 'author key db 'noerror 'unbraced 'xref)
                       "  ") "[ \t\n]+and[ \t\n]+\\|," )))
            (all-files (eh-directory-files-recursively (file-name-directory (ebib-db-get-filename db)) t))
            (files-matched (eh-ebib-get-matched-files all-files name-string)))
       (cond
        ((> (length files-matched) 1)
         (let ((file (ido-completing-read "Open file:" files-matched)))
           ;; (ebib-db-set-field-value 'file (file-relative-name file (expand-file-name ".")) key db 'overwrite)
           ;; (ebib-save-database db)
           (start-process "" nil "xdg-open" file)))
        ((= (length files-matched) 1)
         (let ((file (car files-matched)))
           (message "Opening file: %s" file)
           ;; (ebib-db-set-field-value 'file (file-relative-name file (expand-file-name ".")) key db 'overwrite)
           ;; (ebib-save-database db)
           (start-process "" nil "xdg-open" file)))
        ((< (length files-matched) 1)
         (message "Can't find the corresponding file")))))
    ((default)
     (beep))))

(defun eh-ebib-insert-bibfile-info ()
  (interactive)
  (let* ((bibfiles-list (eh-directory-files-recursively "." t ".bib$"))
         (file
          (or
           (when (and eh-ebib-recently-opened-bibfile
                      (y-or-n-p (format "Insert recently opened bibfile (%s)?  "
                                        eh-ebib-recently-opened-bibfile)))
             eh-ebib-recently-opened-bibfile)
           (when bibfiles-list
             (ido-completing-read "Insert bibfile:" bibfiles-list))
           (ido-read-file-name "Insert bibfile:" (car ebib-file-search-dirs)))))
    (insert (format "# \\bibliography{%s}\n"
                    (file-relative-name file (expand-file-name "."))))))

(defun eh-ebib (&optional key)
  "Open ebib then search the marked string"
  (interactive)
  (let* ((bibfiles-list (eh-directory-files-recursively "." t ".bib$"))
         (file
          (or
           (when (buffer-file-name)
             (car (eh-reftex-get-bibfile-list)))
           (when (and eh-ebib-recently-opened-bibfile
                      (y-or-n-p (format "Load recently opened bibfile (%s)?  "
                                        eh-ebib-recently-opened-bibfile)))
             eh-ebib-recently-opened-bibfile)
           (when bibfiles-list
             (ido-completing-read "Open bibfile:" bibfiles-list))
           (ido-read-file-name "Open bibfile:" (car ebib-file-search-dirs))))
         (word (or (current-word nil t) ""))
         (length (length word))
         (search-string
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (if (and (string-match-p "\\cc+" word) (> length 3))
                (buffer-substring-no-properties (- (point) 2) (point))
              word))))
    (deactivate-mark)
    (setq eh-ebib-push-buffer (current-buffer))
    (ebib file (or key eh-ebib-the-last-entry-key))
    (setq eh-ebib-recently-opened-bibfile file)
    (ebib-select-and-popup-entry)))

(defun eh-ebib-quit ()
  "Quit Ebib.
The Ebib buffers are killed, all variables except the keymaps are set to nil."
  (interactive)
  (when (if (ebib--modified-p)
            (yes-or-no-p "There are modified databases. Quit anyway? ") t)
    (ebib-keywords-save-all-new)
    (ebib--filters-update-filters-file)
    (mapc #'(lambda (buffer)
              (kill-buffer buffer))
          (mapcar #'cdr ebib--buffer-alist))
    (setq ebib--databases nil
          ebib--cur-db nil
          ebib--buffer-alist nil
          ebib--initialized nil
          ebib--index-overlay nil
          ebib--fields-overlay nil
          ebib--strings-overlay nil
          ebib--export-filename nil
          ebib--window-before nil
          ebib--buffer-before nil
          ebib--cur-keys-list nil
          ebib--keywords-files-alist nil
          ebib--keywords-list-per-session nil
          ebib--filters-alist nil
          ebib--filters-modified nil)
    (set-window-configuration ebib--saved-window-config)
    (remove-hook 'kill-emacs-query-functions 'ebib--kill-emacs-query-function)
    (message "")))

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

(defun eh-ebib-push-bibtex-key (&optional leave-ebib-window)
  (interactive)
  (let ((buffer-mode (buffer-local-value 'major-mode (get-buffer eh-ebib-push-buffer))))
    (cond
     ((string= buffer-mode "org-mode")
      (eh-ebib-push-org-cite-link leave-ebib-window))
     (t
      (ebib-push-bibtex-key)
      (when leave-ebib-window
        (ebib-db-unmark-entry 'all ebib--cur-db)
        (ebib--fill-index-buffer)
        (setq eh-ebib-push-buffer nil)
        (ebib-leave-ebib-windows))))))

(defun eh-ebib-format-org-cite-link (key)
  (let ((author (car (split-string
                      (or (ebib-db-get-field-value 'author key ebib--cur-db 'noerror 'unbraced 'xref)
                          "  ") "[ \t\n]+and[ \t\n]+\\|," )))
        (year (or (ebib-db-get-field-value 'year key ebib--cur-db 'noerror 'unbraced 'xref) "20??")))
    (format " [[cite:%s][(%s %s)]]" key author year)))

(defun eh-ebib-push-org-cite-link (&optional leave-ebib-window)
  "Pushes the cite link of current entry to a org-mode buffer."
  (interactive)
  (ebib--execute-when
    ((entries)
     (let* ((key (ebib--cur-entry-key))
            (citation-string
             (if (ebib-db-marked-entries-p ebib--cur-db)
                 (mapconcat #'eh-ebib-format-org-cite-link (ebib-db-list-marked-entries ebib--cur-db) " ")
               (eh-ebib-format-org-cite-link key))))
       ;; 将citation-string插入到eh-ebib-push-buffer变量所
       ;; 对应的buffer, (调用eh-ebib命令时,会设置eh-ebib-push-buffer变量)
       (when citation-string
         (with-current-buffer eh-ebib-push-buffer
           ;; (let* ((point1 (or (save-excursion (search-forward "[[" nil t)) (+ 1 (point-max))))
           ;;         (point2 (save-excursion (search-forward "]]" nil t)))
           ;;         (point3 (save-excursion (search-backward "[[" nil t)))
           ;;         (point4 (or (save-excursion (search-backward "]]" nil t)) -1)))
           ;;   (when (and point2 point3 (> point1 point2) (> point3 point4))
           ;;      (search-forward "]]" nil t)))
           (insert citation-string)
           (message "Pushed \"%s\" to buffer: \"%s\"" citation-string eh-ebib-push-buffer))
         (setq eh-ebib-the-last-entry-key (ebib--cur-entry-key))
         ;; 隐藏ebib窗口
         (when leave-ebib-window
           (ebib-db-unmark-entry 'all ebib--cur-db)
           (ebib--fill-index-buffer)
           (setq eh-ebib-push-buffer nil)
           (ebib-leave-ebib-windows)))))
    ((default)
     (beep))))

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
                     (pyim-hanzi2pinyin-simple
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
  (let* ((current-bib-file (ebib-db-get-filename ebib--cur-db)))
    (ebib--execute-when
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
         (ebib--reload-database ebib--cur-db)
         (ebib--set-modified nil)
         (ebib--redisplay)
         (message "Reformat complete")))
      ((default)
       (beep)))))

(defun eh-convert-cite-key-to-pinyin ()
  "Convert bibtex key to pinyin"
  (interactive)
  (if (featurep 'chinese-pyim)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "\\\\cite{\\([^{}]+\\)}" nil t)
          (let ((string (match-string 1)))
            (replace-match (concat "\\\\cite{" (downcase (pyim-hanzi2pinyin-simple string)) "}") t))))
    (message "Can't find pyim-hanzi2pinyin")))

;; ebib mode keybinding
(ebib-key index "\C-xb" (lambda ()
                          (interactive)
                          (ebib-leave-ebib-windows)
                          (ibuffer)))
(ebib-key index "\C-cb" eh-ebib)
(ebib-key index "\C-xk" ebib-leave-ebib-windows)
(ebib-key index "\C-xq" eh-ebib-quit)
(ebib-key index "v" eh-ebib-abstract-viewer)
(ebib-key index "p" eh-ebib-push-bibtex-key)
(ebib-key index "\C-c\C-c" (lambda () (interactive) (eh-ebib-push-bibtex-key t)))
(ebib-key index "q" eh-ebib-quit)
(ebib-key index "f" eh-ebib-view-file)
(ebib-key index [(control k)] eh-ebib-reformat-all-entries)
(ebib-key index [(return)] ebib-select-and-popup-entry)

(provide 'eh-ebib)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-ebib.el ends here
