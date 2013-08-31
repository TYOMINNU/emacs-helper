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

(defun org-contacts-export-as-csv (&optional name file to-buffer)
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
    (loop for contact in (org-contacts-filter name)
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
    (concat (when org-contacts-csv-line-prefix (format "%s, " org-contacts-csv-line-prefix))
            (format "\"%s%s\", " name
                    (when (featurep 'eh-hanzi2pinyin)
                      (concat "(" (eh-hanzi2pinyin name t) ")")))
            (when tel
              (format "\"%s\", "
                      (progn
			(setq phones-list (org-contacts-remove-ignored-property-values ignore-list (org-contacts-split-property tel)))
			(setq result "")
			(while phones-list
                          (setq result (concat result " "))
			  (setq result (concat result "" (org-contacts-strip-link (car phones-list))))
			  (setq phones-list (cdr phones-list)))
			result)))
	    (when email
              (format "\"%s\", "
                      (progn
                        (setq emails-list (org-contacts-remove-ignored-property-values ignore-list (org-contacts-split-property email)))
                        (setq result "")
                        (while emails-list
                          (setq result (concat result " "))
                          (setq result (concat result (org-contacts-strip-link (car emails-list))))
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
	    (when note (format "\"%s\"," note))
            "\n")))

(defun eh-org-contacts-parse-csv-line (line)
  "Build a org contact from a csv line"
  (let ((list (split-string line ",")))
    (concat "* " (nth 0 list) "\n"
            ":PHONE: " (nth 1 list) "\n"
            ":EMAIL: " (let ((string (nth 2 list)))
                         (if (string-match-p "@" string) string
                           (if (> (length string) 0) (concat string "@qq.com")))) "\n"
            ":NOTE: "  (mapconcat 'identity (nthcdr 3 list) " ") "\n")))

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

(provide 'eh-org-contacts)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-contacts.el ends here
