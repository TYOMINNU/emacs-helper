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
	(metadata (company-fetch-metadata))
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
    (concat
     (when (> (length metadata) 0)
       (format "NOTE: %s\n" metadata))
     (mapconcat 'identity (nreverse msg) ""))))

(defun eh-company-echo-frontend (command)
  "`company-mode' front-end showing the candidates in the echo area."
  (pcase command
    (`post-command (company-echo-show-soon 'eh-company-echo-format))
    (`hide (company-echo-hide))))

(defun eh-company-ascii-setup ()
  (interactive)
  (setq company-transformers
	'(company-sort-by-occurrence))
  (setq company-frontends
	'(company-pseudo-tooltip-frontend
	  company-echo-metadata-frontend)))

(defun eh-company-nonascii-setup ()
  (interactive)
  (setq company-transformers
	'(company-sort-by-occurrence))
  (setq company-frontends
	'(eh-company-echo-frontend)))

(defun eh-company-call-frontend (orig-fun command)
  ;; different setup for ascii and nonascii candidates.
  (if (some (lambda (x) (string-match-p "[[:nonascii:]]+" x))
	    company-candidates)
      (eh-company-nonascii-setup)
    (eh-company-ascii-setup))
  (funcall orig-fun command))

(advice-add 'company-call-frontends :around #'eh-company-call-frontend)

(defun eh-company-theme ()
  (interactive)
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (eh-company-theme))))
  (eh-company-theme))

;; (eh-company-ascii-setup)
(global-set-key (kbd "M-/") 'company-complete)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "M-i") 'company-complete-selection)
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
