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

;; org-odt-data-dir必须在load org之前定义！
(setq org-odt-data-dir
      (concat (file-name-directory
 	       (directory-file-name
 		(file-name-directory
 		 (locate-library "org.el")))) "etc"))

;; require package
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
(require 'org-protocol)
(require 'org-screenshot)
(require 'ob-R)

;; 变量设定
(setq org-latex-to-mathml-convert-command
      "java -jar %j -unicode -force -df %o %I"
      org-latex-to-mathml-jar-file
      "~/bin/mathtoweb.jar")

(setq org-jabref-command '("java" "-jar" "/home/feng/bin/JabRef-2.9.2.jar" "-n" "true"))

(setq org-agenda-files 
      (append (file-expand-wildcards "~/org/*.org")))

;; capture模板
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/i-todo.org" "Tasks")
         "* TODO %? %^g\n %i \n %a")
        ("u" "Notes: 直接保存" entry  (file+headline "~/org/i-notes-auto.org" "Notes")
"** %c
   :PROPERTIES:
   :DATE: %u
   :END:
%i"
:empty-lines 1
:immediate-finish
:kill-buffer)

        ("w" "Notes: 一般事项" entry  (file+headline "~/org/i-notes.org" "Notes")
"** %c
   :PROPERTIES:
   :DATE: %u
   :END:
%i"
:empty-lines 1)

        ("x" "Notes: 学习感悟" entry  (file+headline "~/org/i-notes-study.org" "Notes")
"* %?
   :PROPERTIES:
   :DATE: %u
   :LINK: %a
   :END:
%i"
:empty-lines 1)

        ("l" "Link" entry (file+olp "~/org/i-notes.org" "Web Links")
         "* %a\n %?\n %i")
        ("m" "account" table-line (file+headline "~/org/i-account.org" "account")
         "|%?||||||%u|")
        ("j" "Journal" entry (file+datetree "~/org/i-journal.org")
         "* %?\n %U\n %i\n  %a")
        ("s" "Schedule" entry (file+headline "~/org/i-schedule.org" "Schedule")
         "* %?\n %T\n  %a")
        ("v" "Contacts" entry (file "~/org/i-contacts.org")
               "* %(org-contacts-template-name) %^G
  :PROPERTIES:
  :ALIAS: 
  :NOTE:  
  :EMAIL: %(org-contacts-template-email)
  :PHONE: 
  :IGNORE:
  :END:")
        ("c" "Contacts: 手动输入" entry (file "~/org/i-contacts.org")
               "* %? %^g
  :PROPERTIES:
  :ALIAS: 
  :NOTE: 
  :EMAIL: %x
  :PHONE: 
  :IGNORE: 
  :END:")))

(setq org-agenda-custom-commands
      '(("l" "agenda: 常用"
         ((agenda  "" ((org-agenda-span 1)))
          (tags-todo "生活|IT|购物")))
        ("m" "agenda: org-mobile上使用"
          ((agenda  "" ((org-agenda-overriding-header "Two-Days-Agenda")
                        (org-agenda-span 2)))))
        ("p" "agenda: 项目"
         ((tags-todo "学习太极拳项目")
          (tags-todo "三甲评审项目")))
        ("s" "agenda: 学习相关+书籍杂志文献阅读"
         ((tags-todo "学习")
          (tags-todo "书籍杂志")
          (tags-todo "文献")))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-export-backends
      '(ascii beamer html latex md odt deck rss s5))

(add-to-list 'auto-mode-alist '("\.\(org\|org_archive\)$" . org-mode))   
(setq org-log-done t)   
(setq org-startup-indented nil)
(setq org-confirm-babel-evaluate nil)
(setq org-agenda-remove-tags t)

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

;; org-mode hook
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-hook 'org-mode-hook '(lambda ()
 			    (setq visual-line-fringe-indicators '(nil nil))
 			    (visual-line-mode)
 			    (if visual-line-mode
 				(setq word-wrap nil))))

;; export时替换标点
(defun eh-convert-punctuation (text backend info)
  "将半角标点符号全部替换为全角标点符号"
  (when (memq backend '(odt html))
    (replace-regexp-in-string
     ";" "；"
     (replace-regexp-in-string
      ":" "："
      (replace-regexp-in-string
       "·" "。"
       (replace-regexp-in-string 
	","  "，"
	(replace-regexp-in-string 
	 "\\."  "。"
	 (replace-regexp-in-string "\n" "" text))))))))

(add-to-list 'org-export-filter-plain-text-functions
             'eh-convert-punctuation)

;; use Cairo graphics device by default,which can get better graphics quality.
;; you shoule add require("Cairo") to you ~/.Rprofile
;; (setq org-babel-R-graphics-devices
;;   '((:bmp "bmp" "filename")
;;     (:jpg "jpeg" "filename")
;;     (:jpeg "jpeg" "filename")
;;     (:tikz "tikz" "file")
;;     (:tiff "tiff" "filename")
;;     (:png "CairoPNG" "filename")
;;     (:svg "CairoSVG" "file")
;;     (:pdf "CairoPDF" "file")
;;     (:ps "CairoPS" "file")
;;     (:postscript "postscript" "file")))

;; Export language
(setq org-export-default-language "zh-CN")

;; html
(setq org-html-coding-system 'utf-8)
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

;; latex
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f" 
                              "xelatex -interaction nonstopmode -output-directory %o %f" 
                              "xelatex -interaction nonstopmode -output-directory %o %f"))



(setq org-latex-default-class "ctexart")
(add-to-list 'org-latex-classes
             '("ctexart"
               "\\documentclass[fancyhdr,fntef,nofonts,UTF8,a4paper,cs4size]{ctexart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("ctexrep"
               "\\documentclass[fancyhdr,fntef,nofonts,UTF8,a4paper,cs4size]{ctexrep}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("ctexbook"
               "\\documentclass[fancyhdr,fntef,nofonts,UTF8,a4paper,cs4size]{ctexbook}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass{beamer}
               \\usepackage[fntef,nofonts,fancyhdr]{ctex}"
               org-beamer-sectioning))

(add-to-list 'org-latex-classes
             '("hbuuthesis"
               "\\documentclass[unicode]{hbuuthesis}
 [DEFAULT-PACKAGES]
 [NO-PACKAGES]
 [EXTRA]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


;; org不建议自定义org-latex-default-package-alist变量，但"inputenc" and "fontenc"两个宏包似乎和
;; xelatex有冲突，调整默认值！
(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
(setf org-latex-default-packages-alist
      (remove '("T1" "fontenc" t) org-latex-default-packages-alist))
(setf org-latex-default-packages-alist
      (remove '("normalem" "ulem" t) org-latex-default-packages-alist))
(setcar (rassoc '("wasysym" t)
                org-latex-default-packages-alist) "nointegrals")

(setq  org-latex-packages-alist
       '("
%%% 默认使用的latex宏包 %%%
\\usepackage{xeCJK}
\\usepackage{tikz}
\\usepackage{CJKulem}
\\usepackage{graphicx}

%%% 设置中文字体 %%%
\\setCJKmainfont[ItalicFont={KaiTi_GB2312}]{SimSun}% 文鼎宋体和楷书
\\setCJKsansfont{WenQuanYi Micro Hei}% 文泉驿的黑体
\\setCJKmonofont{WenQuanYi Micro Hei}

%%% 设置页面边距 %%%
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry} % 
"))

;; latex公式预览, 调整latex预览时使用的header,默认使用ctexart类
;; (setq org-format-latex-header
;;       (replace-regexp-in-string
;;        "\\\\documentclass{.*}"
;;        "\\\\documentclass{ctexart}"
;;        org-format-latex-header))


;; 如果一个标题包含TAG: “ignoreheading” ,导出latex时直接忽略这个标题，
;; 但对它的内容没有影响，这个可以使用在这种情况下：
;; * 摘要
;; #+LATEX: \abstract{摘\quad要}

(defadvice org-latex-headline (around my-latex-skip-headlines
                                      (headline contents info) activate)
  (if (member "ignoreheading" (org-element-property :tags headline))
      (setq ad-return-value contents)
    ad-do-it))

(defadvice org-odt-headline (around my-odt-skip-headlines
                                      (headline contents info) activate)
  (if (member "ignoreheading" (org-element-property :tags headline))
      (setq ad-return-value contents)
    ad-do-it))

;; org-mode和reftex的集成,添加下面的配置到org文件头。
;; # \bibliography{bibfilename}

(defun eh-org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
 	 (global-auto-revert-mode t)
	 (reftex-parse-all)
         ;; add a custom reftex cite format to insert links
	 (setq reftex-cite-format
               '((?b . "[[cite:%l]]")
                 (?c . "\\cite{%l}")
                 (?t . "%t")))))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c )") (lambda () (interactive)
                                           (let ((reftex-cite-format "[[cite:%l]]"))
                                             (reftex-citation)))))

(add-hook 'org-mode-hook 'eh-org-mode-reftex-setup)

(defun eh-bibtex-open (key)
  "Get bibfile from \\bibliography{...} and open it with function `org-open-file'"
  (let* ((path (car (reftex-get-bibfile-list))))
    (org-open-file path t nil key)))

(org-add-link-type "cite" 'eh-bibtex-open)





;;;###autoload(require 'eh-org)
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here












