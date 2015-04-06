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

(require 'ox)
(require 'ox-extra)
(require 'ox-ascii)
(require 'ox-beamer)
(require 'ox-html)
(require 'ox-latex)
(require 'ox-md)
(require 'ox-deck)
(require 'ox-rss)
(require 'ox-s5)

(require 'org-contacts)
(require 'org-mime)
(require 'org-bookmark)
(require 'org-protocol)
(require 'org-screenshot)
(require 'ob-R)
(require 'ob-plantuml)

;;; 自定义变量
(setq eh-org-mathtoweb-file "~/bin/mathtoweb.jar")
(setq org-plantuml-jar-path "~/bin/plantuml.jar")

(setq org-latex-to-mathml-convert-command
      "java -jar %j -unicode -force -df %o %I"
      org-latex-to-mathml-jar-file
      eh-org-mathtoweb-file)

(setq org-agenda-files
      (append (file-expand-wildcards "~/org/*.org")))

(setq org-agenda-custom-commands
      '(("l" "agenda:"
         ((agenda  "" ((org-agenda-overriding-header "Two-Days")
                       (org-agenda-span 2)))
          (tags-todo "生活|IT|购物")))))

(setq org-agenda-remove-tags t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(add-to-list 'auto-mode-alist '("\.\(org\|org_archive\)$" . org-mode))
(setq org-insert-heading-respect-content nil)
(setq org-log-done t)
(setq org-startup-indented nil)
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
(setq org-export-backends
      '(ascii beamer html latex md deck rss s5))

;; truncate line depend context
(defun eh-org-truncate-lines (&optional arg)
  (interactive "P")
  (cond
   ((or (and (boundp 'org-clock-overlays) org-clock-overlays)
        org-occur-highlights
        org-latex-fragment-image-overlays)
    (and (boundp 'org-clock-overlays) (org-clock-remove-overlays))
    (org-remove-occur-highlights)
    (org-remove-latex-fragment-image-overlays)
    (message "Temporary highlights/overlays removed from current buffer"))
   (t
    (let* ((context (org-element-context)) (type (org-element-type context)))
      (case type
        ((table table-cell table-row item plain-list)
         (toggle-truncate-lines 1))
        (t (toggle-truncate-lines -1)))))))

(defun eh-org-ctrl-c-ctrl-c (&optional arg)
  (interactive)
  (eh-org-truncate-lines arg)
  (org-ctrl-c-ctrl-c arg))

(org-defkey org-mode-map "\C-c\C-c" 'eh-org-ctrl-c-ctrl-c)

;;; Chinese hack
(defun eh-org-clean-space (text backend info)
  "在export为HTML时，删除中文之间不必要的空格"
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之前的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) +\\(<\\)" regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(>\\) +\\(%s\\)" regexp)
             "\\1\\2" string))
      string)))

(add-to-list 'org-export-filter-paragraph-functions
             'eh-org-clean-space)

;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
;; 但这种方式在中文环境中与下划线冲突。
;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})

;;; org-bable设置
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (org . t)
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
   (shell . t)
   (sql . nil)
   (sqlite . nil)))

;; org-babel hook
(add-hook 'org-babel-after-execute-hook 'eh-org-babel-after-execute-function)

(defun eh-org-babel-after-execute-function ()
  (when (not org-export-current-backend)
    (org-display-inline-images)
    (eh-org-babel-align-tables)))

(defun eh-org-babel-align-tables (&optional info)
  "Align all tables in the result of the current source"
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil info)))
    (when location
      (save-excursion
        (goto-char location)
        (when (looking-at (concat org-babel-result-regexp ".*$"))
          (while (< (point) (progn (forward-line 1) (org-babel-result-end)))
            (when (org-at-table-p)
              (toggle-truncate-lines 1)
              (org-table-align)
              (goto-char (org-table-end)))
            (forward-line)))))))

;; 开启cdlatex
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; 开启自动断行
(add-hook 'org-mode-hook '(lambda ()
                            (setq visual-line-fringe-indicators '(nil nil))
                            (visual-line-mode)
                            (if visual-line-mode
                                (setq word-wrap nil))))

;; export filter
;; (defun eh-convert-punctuation (text backend info)
;;   "将半角标点符号全部替换为全角标点符号"
;;   (when (memq backend '(odt html))
;;     (replace-regexp-in-string
;;      ";" "；"
;;      (replace-regexp-in-string
;;       ":" "："
;;       (replace-regexp-in-string
;;        "·" "。"
;;        (replace-regexp-in-string
;;	","  "，"
;;	(replace-regexp-in-string
;;   "\\."  "。"
;;   (replace-regexp-in-string "\n" "" text))))))))

;; (add-to-list 'org-export-filter-plain-text-functions
;;              'eh-convert-punctuation)

;; Use Cairo graphics device by default,which can get better graphics quality.
;; you shoule add below lines to you ~/.Rprofile
;;    require("Cairo")
;;    CairoFonts(regular="SimSun:style=Regular",
;;             bold="SimHei:style=Regular",
;;             italic="KaiTi_GB2312:style=Regular",
;;             symbol="Symbol")
;;
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

;;; Add new easy templates
(setq org-structure-template-alist
      (append '(("r" "#+BEGIN_SRC R\n?\n#+END_SRC")
                ("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
                ("ex" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
                ("rh" "#+PROPERTY: header-args:R  :session *R* :tangle yes :colnames yes :rownames no :width 700 :height 500 :exports both")
                ("rv" "#+BEGIN_SRC R :results value\n?\n#+END_SRC")
                ("ro" "#+BEGIN_SRC R :results output verbatim\n?\n#+END_SRC")
                ("rg" "#+BEGIN_SRC R :results graphics :file ?\n\n#+END_SRC")
                ("rs" "#+BEGIN_SRC R :results output silent\n?\n#+END_SRC")
                ("rd" "#+BEGIN_SRC R :colnames no :results value drawer\n`%c%` <- function(a,b){c(a,b)}\n?\n#+END_SRC"))
              org-structure-template-alist))

;;; Export language
(setq org-export-default-language "zh-CN")

;;; html
(setq org-html-coding-system 'utf-8)
(setq org-html-head-include-default-style t)
(setq org-html-head-include-scripts t)

;;; latex
(setq org-latex-coding-system 'utf-8)
;; 不要在latex输出文件中插入\maketitle
(setq org-latex-title-command "")
(setq org-latex-date-format "%Y-%m-%d")
(setq org-export-with-LaTeX-fragments 'imagemagick)
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-commands '(("xelatex -interaction nonstopmode -output-directory %o %f"
                            "bibtex %b"
                            "xelatex -interaction nonstopmode -output-directory %o %f"
                            "xelatex -interaction nonstopmode -output-directory %o %f")
                           ("xelatex -interaction nonstopmode -output-directory %o %f")))

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
(setq org-format-latex-header
      (replace-regexp-in-string
       "\\\\documentclass{.*}"
       "\\\\documentclass[nofonts,UTF8]{ctexart}"
       org-format-latex-header))


;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
;; 但对它的内容没有影响，这个可以使用在这种情况下：
;; * 摘要
;; #+LATEX: \abstract{摘\quad要}
;; 这个功能包含在ox-extra.el中。
(ox-extras-activate '(latex-header-blocks ignore-headlines))

(defun eh-org-latex-compile (orig-fun texfile &optional snippet)
  (let ((org-latex-pdf-process
         (if snippet (car (cdr org-latex-commands))
           (car org-latex-commands))))
    (funcall orig-fun texfile snippet)))

(advice-add 'org-latex-compile :around #'eh-org-latex-compile)

(defun eh-org-open-cite-link (key)
  "Get bibfile from \\bibliography{...} and open it with ebib"
  (let* ((path (car (reftex-get-bibfile-list))))
    (ebib path)
    (eh-isearch-string key)
    (ebib-select-and-popup-entry)))

(org-add-link-type "cite" 'eh-org-open-cite-link)

(defun eh-org-fill-paragraph ()
  "Fill org paragraph"
  (interactive)
  (let ((fill-column 10000000))
    (org-fill-paragraph)))

;;;###autoload(require 'eh-org)
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
