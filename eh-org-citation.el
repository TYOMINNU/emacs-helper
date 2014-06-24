;;; eh-org-citation.el --- Tumashu's emacs configuation

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

;; Note:
;; eval the following elisp and install emacs package `org-odt'
;; (add-to-list 'package-archives
;; 	     '("org-odt" . "http://repo.or.cz/w/org-mode/org-kjn.git/blob_plain/master:/") t)
;;

;;; Code:

(require 'ox-bibtex)

;; bibtex default style file
(setq org-bibtex-default-style-file
      (concat (file-name-directory
	       (locate-library "eh-org.el")) "templates/GBT7714-2005-latex/GBT7714-2005NLang-UTF8.bst"))

;; redefine org-bitex-get-style
(defun org-bibtex-get-style (keyword)
  "Return bibliography style as a string.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no style is found,
return the first element of `org-bibtex-style-files' instead."
  (let*  ((value (org-element-property :value keyword))
	  (style (and value
		      (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
		      (match-string 2 value))))
    (if (org-not-nil style)
	style
      org-bibtex-default-style-file)))

;; org-jabref cache directory
(setq org-jabref-cache-directory "~/.org-jabref-cache")

;; 官方org-mode没有包含ox-jabref.el，其包含在org-odt包中，
(when (locate-library "org-odt")
    (require 'ox-jabref))

(setq eh-org-jabref-file "~/bin/JabRef-2.9.2.jar")
(setq org-jabref-command (list "java" "-jar" (expand-file-name eh-org-jabref-file) "-n" "true"))

(setq org-jabref-export-formats
      '(("odt"
	 ("GBT7714-2005"
	  :in-text "GBT7714-2005.ODF.text" :bibliography "GBT7714-2005.ODF.reference"))))

(setq org-jabref-odt-citation-transcoders
      '(org-odt-citation-reference/numbered
	. org-jabref-odt-bibliography/numbered))

(defun org-jabref-group-consecutive-integers (integers)
  "Partition a sorted list of integers into sorted lists of the consecutive integers that it contains.
For example:
    (org-jabref-group-consecutive-integers '(1 2 3 5 6 8 9 13 48))
    => ((1 2 3) (5 6) (8 9) (13) (48))"
  (let ((integres (delete-dups (sort integers '<)))
	(previous-integer nil)
	(this-set ())
	(result ()))
    (mapc (lambda (this-integer)
	    (unless (integerp this-integer) (error "The set contains a non-integer."))
	    (cond ((null this-set)
		   (push this-integer this-set)
		   (setq previous-integer this-integer))
		  ((= this-integer (1+ previous-integer))
		   (push this-integer this-set)
		   (setq previous-integer this-integer))
		  (t
		   (setq this-set (nreverse this-set))
		   (push this-set result)
		   (setq this-set ())
		   (push this-integer this-set)
		   (setq previous-integer this-integer))))
	  integers)
    (cond ((not (null this-set))
	   (setq this-set (nreverse this-set))
	   (push this-set result)
	   (nreverse result)))))

(defun org-jabref-compress-citation-number (number-list)
  (mapconcat
   (lambda (list)
     (let ((max (apply 'max list))
	   (min (apply 'min list)))
       (if (eq max min)
	   (format "%s" max)
	 (format "%s-%s" min max))))
   (org-jabref-group-consecutive-integers number-list) ","))

(defun org-odt-citation-reference/numbered (citation-reference contents info)
  "Transcode a CITATION-REFERENCE element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information.

Replace each CITE-KEY from CITATION-REFERENCE with it's numerical
order in the exported Org file.  Return the concatenated result,
after adding some separators."
  (let* ((latex-frag (org-element-property :value citation-reference))
	 (value (and (string-match org-odt-cite-regexp latex-frag)
		     (substring (match-string 3 latex-frag) 1 -1)))
	 (cite-keys (org-split-string value ",")))
    (format "<text:span text:style-name=\"OrgSuperscript\">[%s]</text:span>"
	    (org-jabref-compress-citation-number
	     (mapcar
	      (lambda (cite-key)
		(setq cite-key (org-trim cite-key))
		(let* ((citations-alist (plist-get info :citations-alist))
		       (cite-key-entry (assoc cite-key citations-alist))
		       (n (length (memq cite-key-entry citations-alist))))
		  n))
	      cite-keys)))))

(provide 'eh-org-citation)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-citation.el ends here
