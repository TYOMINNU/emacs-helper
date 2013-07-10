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
(require 'org-protocol)
(require 'org-screenshot)
(require 'ob-R)
(require 'ox-bibtex)

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

;;export
(setq org-default-language "zh-CN")

;; html
(setq org-html-coding-system 'utf-8)
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

;; latex
(setq org-latex-coding-system 'utf-8)
(setq org-latex-date-format "%Y-%m-%d")
(setq org-export-with-LaTeX-fragments 'imagemagick)
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f" 
                                 "xelatex -interaction nonstopmode -output-directory %o %f" 
                                 "xelatex -interaction nonstopmode -output-directory %o %f"))



(defun org-latex-derived-class (class class-options template-class)
  "build  a new `org-latex-class item with exist item's information"
  (let* ((template-class
          (if (stringp template-class)
              template-class "article"))
         (template-class-header
          (nth 1 (assoc template-class org-latex-classes)))
         (template-class-rest
          (cdr (cdr (assoc template-class org-latex-classes))))
         (class-header-tmp
          (and (stringp template-class-header)
               (if (stringp class-options)
                   (replace-regexp-in-string
                    "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
                    class-options template-class-header t nil 1)
                 template-class-header)))
         (class-header
          (replace-regexp-in-string
           "^[ \t]*\\\\documentclass\\[[^]]*\\]?{\\(.*\\)}"
           class class-header-tmp t nil 1)))
    (append (list class) (list class-header) template-class-rest)))

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

;; org不建议自定义这个变量，但"inputenc" and "fontenc"两个宏包似乎和
;; xelatex有冲突，如果使用xelatex的话，这个变量还得重新定义！
(setq  org-latex-default-packages-alist
  '((""     "fixltx2e"  nil)
    (""     "graphicx"  t)
    (""     "longtable" nil)
    (""     "float"     nil)
    (""     "wrapfig"   nil)
    (""     "soul"      t)
    (""     "textcomp"  t)
    (""     "marvosym"  t)
    (""     "wasysym"   t)
    (""     "latexsym"  t)
    (""     "amssymb"   t)
    (""     "amstext"   nil)
    (""     "hyperref"  nil)
    "\\tolerance=1000"))


(setq  org-latex-packages-alist
       '("
%%% 默认使用的latex宏包 %%%
\\usepackage{tikz}
\\usepackage{CJKulem}
\\usepackage{graphicx}

%%% 设置中文字体 %%%
\\setCJKmainfont[ItalicFont={KaiTi_GB2312}]{SimSun}% 文鼎宋体和楷书
\\setCJKsansfont{WenQuanYi Micro Hei}% 文泉驿的黑体
\\setCJKmonofont{WenQuanYi Micro Hei}

%%% 设置页面边距 %%%
%\\usepackage[top=1.55cm, bottom=2.29cm, left=1.6cm, right=1.47cm]{geometry} % 
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry} % 
"))


;; 中文下划线使用\CJKunderline效果比较好,
;; 这个命令需要CJKfntef宏包，如果使用ctex，可以添加fntef选项
(setq org-latex-text-markup-alist 
      '((bold . "\\textbf{%s}")
        (code . verb)
        (italic . "\\emph{%s}")
        (strike-through . "\\sout{%s}")
        (underline . "\\uline{%s}")
        (verbatim . protectedtexttt)))

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
\\pagestyle{empty}             % do not remove too
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


;; 如果一个标题包含TAG: “ignoreheading” ,导出latex时直接忽略这个标题，
;; 但对它的内容没有影响，这个可以使用在这种情况下：
;; * 摘要
;; #+LATEX: \abstract{摘\quad要}

(defadvice org-latex-headline (around my-latex-skip-headlines
                                      (headline contents info) activate)
  (if (member "ignoreheading" (org-element-property :tags headline))
      (setq ad-return-value contents)
    ad-do-it))


;; org-mode和reftex的集成,添加下面的配置到org文件头。
;; #+LINK: bib cite:%s
;; # \bibliography{./filename.bib}


(defun eh-org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ; enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
         ; add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[cite:%l]]")
            (?c . "\\cite{%l}")
	    (?t . "%t")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

(add-hook 'org-mode-hook 'eh-org-mode-reftex-setup)

(defun eh-bibtex-open (key)
  "Get bibfile from \\bibliography{...} and open it with function `org-open-file'"
  (let* ((path (car (reftex-get-bibfile-list))))
    (org-open-file path t nil key)))

(org-add-link-type "cite" 'eh-bibtex-open)

;; org-mode global keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;###autoload(require 'eh-org)
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here












