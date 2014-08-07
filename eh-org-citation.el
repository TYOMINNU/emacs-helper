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
(when (locate-library "org-odt")
  (require 'ox-jabref))

(setq eh-org-jabref-file "~/bin/JabRef-2.9.2.jar")
(setq org-jabref-command (list "java" "-jar" (expand-file-name eh-org-jabref-file) "-n" "true"))

;; org cite link setting
(org-add-link-type "cite" 'eh-ebib)

;; bibtex default style file
(setq eh-org-bibtex-default-style-file
      (concat (file-name-directory
	       (locate-library "eh-org.el")) "templates/GBT7714-2005-latex/GBT7714-2005NLang-UTF8.bst"))

;; bibtex2html default options
(setq eh-org-bibtex-bibtex2html-options
      '("-a" "-noabstract" "-nokeywords" "-i" "-nolinks"))

;; defadvice org-bitex-get-style
(defadvice org-bibtex-get-style (after eh-org-bibtex-get-style () activate)
  (if (not (org-not-nil ad-return-value))
      (setq ad-return-value
	    eh-org-bibtex-default-style-file)))

;; defadvice org-bibtex-get-arguments
(defadvice org-bibtex-get-arguments (after eh-org-bibtex-get-arguments () activate)
  (let ((orig-options (plist-get ad-return-value :options)))
    (setq options (plist-put ad-return-value :options
			     (delete-dups (append eh-org-bibtex-bibtex2html-options orig-options))))))
(provide 'eh-org-citation)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-citation.el ends here
