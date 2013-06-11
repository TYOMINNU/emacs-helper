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
(setq org-babel-R-graphics-devices
  '((:bmp "bmp" "filename")
    (:jpg "jpeg" "filename")
    (:jpeg "jpeg" "filename")
    (:tikz "tikz" "file")
    (:tiff "tiff" "filename")
    (:png "CairoPNG" "filename")
    (:svg "CairoSVG" "file")
    (:pdf "CairoPDF" "file")
    (:ps "CairoPS" "file")
    (:postscript "postscript" "file")))

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
\\usepackage[fancyhdr,fntef,nofonts,UTF8]{ctex}
\\usepackage{tikz}
\\usepackage{CJKulem}
%\\usepackage{amsmath,amsfonts,amsthm}
\\usepackage{graphicx}
\\usepackage{multicol}
\\usepackage{titlesec}

%%% 设置中文字体 %%%
\\setCJKmainfont[ItalicFont={KaiTi_GB2312}]{SimSun}% 文鼎宋体和楷书
\\setCJKsansfont{WenQuanYi Micro Hei}% 文泉驿的黑体
\\setCJKmonofont{WenQuanYi Micro Hei}

%%% 设置页面边距 %%%
%\\usepackage[top=1.55cm, bottom=2.29cm, left=1.6cm, right=1.47cm]{geometry} % 
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry} % 


%%% 设置段落与段落，段落与标题之间的间隔 %%%
\\setlength{\\parindent}{0pt}		% indentation on new paragraph
\\setlength{\\parskip}{0pt}		% vertical spacing on new paragraph
\\setlength{\\lineskip}{1pt}		% vertical spacing between lines
\\setlength{\\columnsep}{1cm}		% spacing between columns
\\setlength{\\belowcaptionskip}{0pt}	% spacing below captions
\\setlength{\\abovecaptionskip}{5pt}	% spacong above captions

%%% 章节相关设置 %%%
\\titleformat{\\chapter}{\\centering\\Huge\\bfseries}{第\\,\\thechapter\\,章}{1em}{}
\\titleformat{\\section}{\\centering\\Large\\bfseries}{\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}


%%% 设置页眉页脚 %%%
\\pagestyle{fancy}
\\fancyhead{} % clear all fields
\\fancyhead[CO]{河北联合大学硕士论文}
\\fancyhead[CE]{\\leftmark}
\\fancyfoot[CO,CE]{-~\\thepage~-}
\\renewcommand{\\headrulewidth}{0.4pt}
\\renewcommand{\\footrulewidth}{0pt}
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

(setq org-format-latex-header "\\documentclass{article}
\\usepackage[fancyhdr,fntef,nofonts]{ctex}
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












