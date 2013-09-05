;;; eh-org-contacts.el --- Tumashu's emacs configuation

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
(require 'org-contacts)

(defcustom org-contacts-csv-file "contacts.csv"
  "Default file for csv export."
  :group 'org-contacts
  :type 'file)

(defvar org-contacts-csv-line-prefix "")

(defun org-contacts-export-as-vcard (&optional name tags property file to-buffer)
  "Export all contacts matching NAME as VCard 3.0.
If TO-BUFFER is nil, the content is written to FILE or
`org-contacts-vcard-file'.  If TO-BUFFER is non-nil, the buffer
is created and the VCard is written into that buffer."
  (interactive) ; TODO ask for name?
  (let* ((filename (or file org-contacts-vcard-file))
	 (buffer (if to-buffer
		     (get-buffer-create to-buffer)
		   (find-file-noselect filename))))
    (message "Exporting...")
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (when (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system coding-system-for-write))
    (loop for contact in (org-contacts-filter name tags property)
	  do (insert (org-contacts-vcard-format contact)))
    (if to-buffer
	(current-buffer)
      (progn (save-buffer) (kill-buffer)))))


(defun org-contacts-export-as-csv (&optional name tags property file to-buffer)
  "Export all contacts matching NAME as csv
If TO-BUFFER is nil, the content is written to FILE or
`org-contacts-vcard-file'.  If TO-BUFFER is non-nil, the buffer
is created and the VCard is written into that buffer."
  (interactive) ; TODO ask for name?
  (let* ((filename (or file org-contacts-csv-file))
	 (buffer (if to-buffer
		     (get-buffer-create to-buffer)
		   (find-file-noselect filename))))
    (message "Exporting...")
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (when (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system coding-system-for-write))
    (loop for contact in (org-contacts-filter name tags property)
	  do (insert (org-contacts-csv-format contact)))
    (if to-buffer
	(current-buffer)
      (progn (save-buffer) (kill-buffer)))))

(defun org-contacts-csv-format (contact)
  "Formats CONTACT in csv format."
  (let* ((properties (caddr contact))
	 (name (org-contacts-vcard-escape (car contact)))
	 (n (org-contacts-vcard-encode-name name))
	 (email (cdr (assoc-string org-contacts-email-property properties)))
	 (tel (cdr (assoc-string org-contacts-tel-property properties)))
	 (ignore-list (cdr (assoc-string org-contacts-ignore-property properties)))
	 (ignore-list (when ignore-list
			(org-contacts-split-property ignore-list)))
	 (note (cdr (assoc-string org-contacts-note-property properties)))
	 (bday (org-contacts-vcard-escape (cdr (assoc-string org-contacts-birthday-property properties))))
	 (addr (cdr (assoc-string org-contacts-address-property properties)))
	 (nick (org-contacts-vcard-escape (cdr (assoc-string org-contacts-nickname-property properties))))
	 emails-list result phones-list)
    (concat (when org-contacts-csv-line-prefix (format "%s " org-contacts-csv-line-prefix))
            ;; 中国人的名字一般是三个字，为实现对齐，两个字的姓名后面
            ;; 添加一个全角空格
            (format "\"%s\", " (if (< (length name) 3) (concat name "　") name))
            (when tel
              (format "\"%s\", "
                      (progn
			(setq phones-list (org-contacts-remove-ignored-property-values ignore-list (org-contacts-split-property tel)))
			(setq result "")
			(while phones-list
			  (setq result (concat result (org-contacts-strip-link (car phones-list)) "; "))
			  (setq phones-list (cdr phones-list)))
			result)))
	    (when email
              (format "\"%s\", "
                      (progn
                        (setq emails-list (org-contacts-remove-ignored-property-values ignore-list (org-contacts-split-property email)))
                        (setq result "")
                        (while emails-list
                          (setq result (concat result (org-contacts-strip-link (car emails-list)) "; "))
                          (setq emails-list (cdr emails-list)))
                        result)))
	    (when addr
	      (format "\"%s\", " (replace-regexp-in-string "\\, ?" ";" addr)))
	    (when bday
	      (let ((cal-bday (calendar-gregorian-from-absolute (org-time-string-to-absolute bday))))
		(format "\"%04d-%02d-%02d\", "
			(calendar-extract-year cal-bday)
			(calendar-extract-month cal-bday)
			(calendar-extract-day cal-bday))))
	    (when nick (format "\"%s\", " nick))
	    (when note (format "\"%s\"," (replace-regexp-in-string "," ";" note)))
            (when (featurep 'eh-hanzi2pinyin)
              (format "\"(%s)\"" (eh-hanzi2pinyin name t)))
            "\n")))

(defun eh-org-contacts-parse-csv-line (line)
  "Build a org contact from a csv line"
  (let ((list (split-string line ",")))
    (concat "* " (nth 0 list) "\n"
            ":PROPERTIES:\n"
            ":PHONE: " (nth 1 list) "\n"
            ":EMAIL: " (let ((string (nth 2 list)))
                         (if (string-match-p "@" string) string
                           (if (> (length string) 0) (concat string "@qq.com")))) "\n"
            ":NOTE: "  (mapconcat 'identity (nthcdr 3 list) " ") "\n"
            ":END:\n")))

(defun eh-org-contacts-csv-import (&optional filename)
  "Convert a csv file to org contacts format and insert current point"
  (interactive)
  (let ((file (if filename filename (read-file-name "CSV file:")))
        (buffer (current-buffer))
        (point (point))
        contacts-string)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (setq contacts-string (concat contacts-string (eh-org-contacts-parse-csv-line (buffer-substring (point) (progn (end-of-line) (point)))) "\n"))
      (forward-line 1)
      (beginning-of-line 1)))
  (switch-to-buffer buffer)
  (goto-char point)
  (insert contacts-string)))

(defun eh-org-contacts-merge-contacts ()
  "Merge duplicate contacts"
  (interactive)
  (dolist (name (let ((contact-list (org-contacts-filter nil nil)))
                  (delete-dups
                   (loop for contact in contact-list
                         collect (substring-no-properties
                                  (org-contacts-vcard-escape (car contact)))))))
    ;; let UPDATE be the first contact matching NAME
    (let* ((contacts (org-contacts-filter (concat "\\b" name "\\b")))
           (update (first contacts)))
      (save-excursion
        (with-current-buffer (marker-buffer (second update))
          (goto-char (marker-position (second update)))
          ;; for all other contacts matching NAME as CONTACT
          (dolist (contact (rest contacts))
            ;; add all properties to UPDATE
            (loop with special-properties = (mapcar 'first (org-entry-properties nil 'special))
                  for (property . value) in (third contact)
                  unless (member property special-properties)
                  do (if (or (string= property "TAGS") (string= property "ALLTAGS"))
                         (org-set-tags-to (delete-dups
                                           (nconc (org-get-tags-at)
                                                  (split-string (if value value "") ":" t))))
                       (let ((second-value (org-entry-get nil property)))
                         (unless (string= (org-contacts-strip-link (if second-value second-value ""))
                                          (org-contacts-strip-link (if value value "")))
                           (org-entry-put nil property
                                          (concat (org-entry-get nil property)
                                                  " " value))))))
            ;; delete CONTACT
            (save-excursion
              (with-current-buffer (marker-buffer (second contact))
                (goto-char (marker-position (second contact)))
                (let ((plist (second (org-element-at-point))))
                  (delete-region (plist-get plist :begin)
                                 (plist-get plist :end)))))))))))

(defun eh-org-contacts-add-pinyin-alias ()
  "Add pinyin alias to all head of current buffer"
  (interactive)
  (if (featurep 'eh-hanzi2pinyin)
      (org-map-entries '(lambda () (let ((pinyin-alias (eh-hanzi2pinyin (org-get-heading 1 1) t)))
                                (org-set-property org-contacts-alias-property pinyin-alias))))))

(defun eh-org-contacts-generate-phone-and-email-links ()
  "Add pinyin alias to all head of current buffer"
  (interactive)
  (if (featurep 'eh-hanzi2pinyin)
      (org-map-entries '(lambda () (let ((email (org-entry-get nil org-contacts-email-property ))
                                    (tel (org-entry-get nil org-contacts-tel-property )))
                                (when tel
                                  (org-set-property org-contacts-tel-property
                                                    (replace-regexp-in-string
                                                     " +\\([0-9]\\{7,11\\}\\)"
                                                     " [[tel:\\1]]"
                                                     (concat " " (if tel tel "")))))
                                (when email
                                  (org-set-property org-contacts-email-property
                                                  (replace-regexp-in-string
                                                   " +\\([a-zA-Z0-9_.-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9-.]+.[a-zA-Z0-9]?\\)"
                                                   " [[mailto:\\1]]"
                                                   (concat " "(if email email ""))))))))))

(provide 'eh-org-contacts)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-contacts.el ends here
