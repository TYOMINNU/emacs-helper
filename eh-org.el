;;; eh-org.el --- Tumashu's org-mode configuation 

;; Copyright (c) 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.2
;; Package-Requires: ((org "7.8.00"))
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

(require 'org)
(require 'ox-ascii)
(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-html)
(require 'ox-deck)
(require 'ox-s5)
(require 'ox-rss)
(require 'ox-md)
(require 'ox-odt)
(require 'org-contacts)
(require 'org-mime)
(require 'org-bookmark)

(setq org-export-backends
      '(ascii beamer html latex md odt deck rss s5))

(add-to-list 'auto-mode-alist '("\.\(org\|org_archive\)$" . org-mode))   
(setq org-log-done t)   
(setq org-startup-indented nil)
(setq org-confirm-babel-evaluate nil)

;; org-bable设置
; font-lock in src code blocks
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (mscgen . t)
   (latex . t) 
   (ocaml . nil)
   (perl . t)
   (python . t)
   (ruby . nil)
   (screen . nil)
   (sh . t)
   (sql . nil)
   (sqlite . nil)))

;; org-babel hook
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)



;;export
(setq org-default-language "zh-CN")

;; html
(setq org-html-coding-system 'utf-8)
(setq org-html-style-include-default nil)
(setq org-html-style-include-scripts nil)

;; latex
(setq org-latex-coding-system 'utf-8)
(setq org-latex-date-format "%Y-%m-%d")
(setq org-export-with-LaTeX-fragments 'imagemagick)
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f" 
                                 "xelatex -interaction nonstopmode -output-directory %o %f" 
                                 "xelatex -interaction nonstopmode -output-directory %o %f"))


(add-to-list 'org-latex-classes
             '("ctexart"
               "\\documentclass{ctexart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("ctexrep"
               "\\documentclass{ctexrep}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("ctexbook"
               "\\documentclass{ctexbook}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass{beamer}
               \\usepackage{ctex}"
               org-beamer-sectioning))

(setq  org-latex-packages-alist
       '("
\\setCJKmainfont[ItalicFont={AR PL UKai CN}]{AR PL UMing CN}% 文鼎宋体和楷书
\\setCJKsansfont{WenQuanYi Zen Hei}% 文泉驿的黑体
\\setCJKmonofont{WenQuanYi Zen Hei}
"))

;; latex公式预览

;; 设置默认缩放比例为1.2.
(setq org-format-latex-options 
      (plist-put org-format-latex-options :scale 1.2))

(setq org-format-latex-header "\\documentclass{ctexart}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage[mathscr]{eucal}
\\pagestyle{empty}             % do not remove
\[PACKAGES]
\[DEFAULT-PACKAGES]
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

;; org-mode global keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; org-contacts

(setq org-contacts-tel-property "PHONE")
(setq org-contacts-note-property "NOTE")
(defun org-contacts-vcard-format (contact)
  "Formats CONTACT in VCard 3.0 format."
  (let* ((properties (caddr contact))
	 (name (org-contacts-vcard-escape (car contact)))
	 (n (org-contacts-vcard-encode-name name))
	 (email (cdr (assoc-string org-contacts-email-property properties)))
	 (bday (org-contacts-vcard-escape (cdr (assoc-string org-contacts-birthday-property properties))))
	 (addr (cdr (assoc-string org-contacts-address-property properties)))
	 (nick (org-contacts-vcard-escape (cdr (assoc-string org-contacts-nickname-property properties))))
	 (tel (cdr (assoc-string org-contacts-tel-property properties)))
	 (note (org-contacts-vcard-escape (cdr (assoc-string org-contacts-note-property properties))))
	 (head (format "BEGIN:VCARD\nVERSION:3.0\nN:%s\nFN:%s\n" n name)))
    (concat head
	    (when email (progn 
			  (setq emails-list (split-string email "[,;: ]+"))
			  (setq result "")
			  (while emails-list
			    (setq result (concat result  "EMAIL:" (car emails-list) "\n"))
			    (setq emails-list (cdr emails-list)))
			  result))
	    (when addr
	      (format "ADR:;;%s\n" (replace-regexp-in-string "\\, ?" ";" addr)))
	    (when tel (progn 
			(setq phones-list (split-string tel "[,;: ]+"))
			(setq result "")
			(while phones-list
			  (setq result (concat result  "TEL:" (car phones-list) "\n"))
			  (setq phones-list (cdr phones-list)))
			result))
	    (when bday
	      (let ((cal-bday (calendar-gregorian-from-absolute (org-time-string-to-absolute bday))))
		(format "BDAY:%04d-%02d-%02d\n"
			(calendar-extract-year cal-bday)
			(calendar-extract-month cal-bday)
			(calendar-extract-day cal-bday))))
	    (when nick (format "NICKNAME:%s\n" nick))
	    (when note (format "NOTE:%s\n" note))
	    "END:VCARD\n\n")))

;;;###autoload(require 'eh-org)
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here












