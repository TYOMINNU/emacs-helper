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

;;; Code:
(use-package ox-bibtex
  :ensure nil
  :config
  (defvar eh-org-bibtex-default-style-file
    (concat (file-name-directory
             (locate-library "eh-org.el"))
            "templates/GBT7714-2005-latex/GBT7714-2005NLang-UTF8.bst")
    "emacs-helper default bibtex style file")

  (defvar eh-org-bibtex-bibtex2html-options
    '("-a" "-noabstract" "-nokeywords" "-i" "-nolinks")
    "emacs-helper bibtex2html default options")

  (defun eh-org-bibtex-add-default-style (style)
    "If `org-bibtex-get-style not return a valid style, return a default"
    (if (org-not-nil style)
        style
      eh-org-bibtex-default-style-file))

  (defun eh-org-bibtex-add-default-arguments (arguments)
    "Add extra arguments to `org-bibtex-get-arguments returned"
    (let ((orig-options (plist-get arguments :options)))
      (plist-put arguments :options
                 (delete-dups (append eh-org-bibtex-bibtex2html-options orig-options)))))

  (advice-add 'org-bibtex-get-style :filter-return #'eh-org-bibtex-add-default-style)
  (advice-add 'org-bibtex-get-arguments :filter-return #'eh-org-bibtex-add-default-arguments))

(provide 'eh-org-citation)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-citation.el ends here
