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
(require 'eh-bibtex)
(require 'reftex)
(require 'ebib)
(require 'chinese-pyim)

;; org cite link setting
(org-add-link-type "cite" 'eh-ebib)

(setq ebib-layout 'full
      ebib-window-vertical-split nil
      ebib-width 80
      ebib-index-window-size 10

      ;; allow the same keys
      ebib-uniquify-keys nil

      ;; If this varible is `t', index buffer will
      ;; highlight lines instead of autokey word
      ;; setting this varible to *a list of field*
      ;; is *useless* in my configure
      ebib-index-display-fields t

      ;; ebib addition fields
      ebib-extra-fields
      '((BibTeX "keywords" "abstract" "timestamp"
                "file"  "url" "crossref" "annote" "doi")
        (biblatex "keywords" "abstract" "timestamp"
                  "file"  "url" "crossref" "annote" "doi")))

(defface eh-ebib-display-default-face
  '((t (:inherit default :family "Liberation Serif")))
  "Face to display key string"
  :group 'eh-ebib)

(defface eh-ebib-display-key1-face
  '((t (:inherit default
                 :height 100
                 :box t
                 :bold t)))
  "Face to display key string"
  :group 'eh-ebib)

(defface eh-ebib-display-key2-face
  '((t (:inherit default :height 30)))
  "Face to display key string"
  :group 'eh-ebib)

(defface eh-ebib-display-key3-face
  '((t (:inherit ,eh-ebib-display-key1-face :box nil)))
  "Face to display key string"
  :group 'eh-ebib)

(defface eh-ebib-display-separator-face
  '((t (:inherit ,eh-ebib-display-default-face)))
  "Face to display separator string"
  :group 'eh-ebib)

(defface eh-ebib-display-author-face
  '((t (:inherit ,eh-ebib-display-default-face
                 :foreground "green")))
  "Face to display author string"
  :group 'eh-ebib)

(defface eh-ebib-display-title-face
  '((t (:inherit ,eh-ebib-display-default-face)))
  "Face to display title string"
  :group 'eh-ebib)

(defface eh-ebib-display-publisher-face
  '((t (:inherit ,eh-ebib-display-default-face
                 :foreground "blue")))
  "Face to display publisher string"
  :group 'eh-ebib)

(defface eh-ebib-display-year-face
  '((t (:inherit ,eh-ebib-display-default-face
                 :foreground "orange"
                 :italic t)))
  "Face to display year string"
  :group 'eh-ebib)

(defvar eh-ebib-recently-opened-bibfile nil
  "record the last opened bibfile name")

(defvar eh-ebib-the-last-entry-key ""
  "record the last citation string")

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

(defun eh-ebib--split-key (key)
  (split-string
   (replace-regexp-in-string
    "\\(.*[0-9]\\)\\([a-z].*\\)"
    "\\1@\\2" key)
   "@"))

(defun eh-ebib--remove-newlines (string)
  (replace-regexp-in-string "\n+" " " string))

;; ebib index buffer format setting
(defun eh-ebib--display-entry-key (entry-key)
  "Display entry-key, title, journal, publisher and school in the index buffer at POINT. "
  (with-current-ebib-buffer 'index
    (with-ebib-buffer-writable
      (setq cursor-type t)
      (insert (concat
               ;; entry key
               (let* ((list (eh-ebib--split-key entry-key))
                      (str1 (car list))
                      (str2 (car (cdr list))))
                 (concat
                  (propertize str1 'face 'eh-ebib-display-key1-face)
                  (propertize (format "%-20s" (or str2 ""))
                              'face 'eh-ebib-display-key2-face)
                  (propertize (make-string (max (- 20 (length str1)) 0) ? )
                              'face 'eh-ebib-display-key3-face)))
               ;; author
               (propertize
                (car (split-string
                      (or (ebib-db-get-field-value 'author entry-key ebib--cur-db 'noerror 'unbraced)
                          "  ") "[ \t\n]+and[ \t\n]+" ))
                'face 'eh-ebib-display-author-face)
               ;; separator
               (propertize ". " 'face 'eh-ebib-display-separator-face)
               ;; year
               (propertize
                (or (ebib-db-get-field-value 'year entry-key ebib--cur-db 'noerror 'unbraced 'xref) "20??")
                'face 'eh-ebib-display-year-face)
               ;; separator
               (propertize ". " 'face 'eh-ebib-display-separator-face)
               ;; title
               (propertize
                (eh-ebib--remove-newlines
                 (or (ebib-db-get-field-value 'title entry-key ebib--cur-db 'noerror 'unbraced) ""))
                'face 'eh-ebib-display-title-face)
               ;; separator
               (propertize ". " 'face 'eh-ebib-display-separator-face)
               ;; journal publisher or school
               (propertize
                (eh-ebib--remove-newlines
                 (or (ebib-db-get-field-value 'journal entry-key ebib--cur-db 'noerror 'unbraced)
                     (ebib-db-get-field-value 'publisher entry-key ebib--cur-db 'noerror 'unbraced)
                     (ebib-db-get-field-value 'school entry-key ebib--cur-db 'noerror 'unbraced)
                     ""))
                'face 'eh-ebib-display-publisher-face)
               "\n")))))

(advice-add 'ebib--display-entry-key :override #'eh-ebib--display-entry-key)

(defun eh-ebib-get-matched-files (files-list match-str)
  (let ((match-string (replace-regexp-in-string "[ +-=_]+" "" match-str)))
    (if (string= match-string "")
        ""
      (cl-delete-if
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
         (file (or (when (buffer-file-name)
                     (car (ignore-errors (reftex-get-bibfile-list))))
                   (when (and eh-ebib-recently-opened-bibfile
                              (y-or-n-p (format "Load recently opened bibfile (%s)?  "
                                                eh-ebib-recently-opened-bibfile)))
                     eh-ebib-recently-opened-bibfile)
                   (when bibfiles-list
                     (ido-completing-read "Open bibfile:" bibfiles-list))
                   (ido-read-file-name "Open bibfile:" (car ebib-file-search-dirs)))))
    (setq eh-ebib-push-buffer (current-buffer))
    (ebib file (or key eh-ebib-the-last-entry-key))
    (setq eh-ebib-recently-opened-bibfile file)
    (ebib-select-and-popup-entry)))

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

;; ebib mode keybinding
(ebib-key index "\C-xb" (lambda ()
                          (interactive)
                          (ebib-leave-ebib-windows)
                          (ibuffer)))
(ebib-key index "\C-cb" eh-ebib)
(ebib-key index "\C-xk" ebib-leave-ebib-windows)
(ebib-key index "\C-xq" ebib-force-quit)
(ebib-key index "v" eh-ebib-abstract-viewer)
(ebib-key index "p" eh-ebib-push-bibtex-key)
(ebib-key index "\C-c\C-c" (lambda () (interactive) (eh-ebib-push-bibtex-key t)))
(ebib-key index "q" ebib-force-quit)
(ebib-key index "f" eh-ebib-view-file)
(ebib-key index [(control k)] eh-ebib-reformat-all-entries)
(ebib-key index [(return)] ebib-select-and-popup-entry)

(provide 'eh-ebib)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-ebib.el ends here
