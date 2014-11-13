;;; eh-complete.el --- Tumashu's emacs complete configuation

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
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

;; Completion for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; IDO & filecache: smart file name completion
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido) ;Improved flex matching
(require 'ido-vertical-mode) ;Vertical completion menu

(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".org" ".R" ".el" ".java" ".js" ".el" ".xml")
      ido-use-filename-at-point 'guess
      ido-use-faces nil
      flx-ido-use-faces t)

(ido-mode 1)
(ido-ubiquitous)
(flx-ido-mode 1)
(ido-vertical-mode)

;; ido keybindings
(add-hook 'ido-make-file-list-hook 'eh-ido-sort-mtime) ;文件的排序方法
(add-hook 'ido-make-dir-list-hook 'eh-ido-sort-mtime) ;目录的排序方法
(defun eh-ido-sort-mtime ()
  (setq ido-temp-list
	(sort ido-temp-list
	      (lambda (a b)
		(time-less-p
		 (sixth (file-attributes (concat ido-current-directory b)))
		 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;move . files to end (again)
   (delq nil (mapcar
	      (lambda (x) (and (char-equal (string-to-char x) ?.) x))
	      ido-temp-list))))

(add-hook 'ido-setup-hook 'eh-ido-keybinding)
(defun eh-ido-keybinding ()
  (define-key ido-completion-map (kbd "C-SPC") nil)
  (define-key ido-completion-map (kbd "C-@") nil)
  (define-key ido-completion-map (kbd "C-i") 'ido-edit-input)
  (define-key ido-completion-map (kbd "C-l") 'ido-delete-backward-updir))

;; company-mode
(require 'company)
(require 'color)

(setq eh-origin-mode-line-format mode-line-format)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-dabbrev-downcase nil)
(setq company-tooltip-limit 10)
(setq company-echo-delay 0)
(setq company-global-modes '(not git-commit-mode))
(setq eh-company-current-setup nil)

(add-to-list 'company-begin-commands 'ibus-exec-callback)
(add-to-list 'company-begin-commands 'ibus-handle-event)

(setq company-backends
      '((company-capf company-dabbrev company-files)
	(company-dabbrev-code company-gtags company-etags
			      company-keywords)))

(defun eh-company-echo-format ()
  "show candidates like ido-vertical-mode"
  (let ((lines 0)
	;; Roll to selection.
	(candidates (nthcdr company-selection company-candidates))
	(i (if company-show-numbers company-selection 99999))
	comp msg)
    (setq eh-output-1 candidates)
    (while candidates
      (setq comp (company-reformat (pop candidates))
	    lines (+ lines 1))
      (if (< i 10)
	  ;; Add number.
	  (progn
	    (setq comp (propertize (format "%d: %s\n" i comp)
				   'face 'company-echo))
	    (cl-incf i)
	    (add-text-properties 3 (+ 3 (length company-common))
				 '(face company-echo-common) comp))
	(setq comp (propertize (format "-> %s\n" comp) 'face 'company-echo))
	(add-text-properties 0 (length company-common)
			     '(face company-echo-common) comp))
      (if (>= lines 5)
	  (setq candidates nil)
	(push comp msg)))
    (mapconcat 'identity (nreverse msg) "")))

(defun eh-company-echo-frontend (command)
  "`company-mode' front-end showing the candidates in the echo area."
  (pcase command
    (`post-command (company-echo-show-soon 'eh-company-echo-format))
    (`hide (company-echo-hide))))

(defun eh-company-select-nonascii-candidates (candidates)
  (remove-if (lambda (x) (not (string-match-p "[[:nonascii:]]+" x)))
	     candidates))

(defun eh-company-select-ascii-candidates (candidates)
  (remove-if (lambda (x) (string-match-p "[[:nonascii:]]+" x))
	     candidates))

(defun eh-company-switch-setup ()
  (interactive)
  (if (string= eh-company-current-setup "ascii")
      (eh-company-nonascii-setup)
    (eh-company-ascii-setup))
  (company-abort)
  (company-manual-begin))

(defun eh-company-ascii-setup ()
  (interactive)
  (setq eh-company-current-setup "ascii")
  (setq company-transformers
	'(company-sort-by-occurrence
	  eh-company-select-ascii-candidates))
  (setq company-frontends
	'(company-pseudo-tooltip-unless-just-one-frontend
	  company-preview-if-just-one-frontend
	  company-echo-metadata-frontend)))

(defun eh-company-nonascii-setup ()
  (interactive)
  (setq eh-company-current-setup "nonascii")
  (setq company-transformers
	'(company-sort-by-occurrence))
  (setq company-frontends
	'(eh-company-echo-frontend
	  company-preview-if-just-one-frontend)))

(defun eh-company-theme ()
  (interactive)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
     `(company-preview ((t (:inherit default :background ,(color-lighten-name bg 2))))))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (eh-company-theme))))
  (eh-company-theme))

(eh-company-ascii-setup)
(global-set-key (kbd "M-/") 'eh-company-switch-setup)
(define-key company-active-map (kbd "M-i") 'eh-company-switch-setup)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p")'company-select-previous)
(define-key company-active-map (kbd "M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p")'company-select-previous)
(add-hook 'after-init-hook 'global-company-mode)

;;;autoload(require 'eh-complete)
(provide 'eh-complete)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-complete.el ends here
