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
      flx-ido-use-faces t
      tramp-default-method nil)

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
(require 'adaptive-wrap)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-echo-delay 0)
(setq company-global-modes '(not git-commit-mode))

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-require-match nil)

(setq eh-company-buffer-name "*eh-company-buffer*")
(setq eh-company-sidebar-side 'right)
(setq eh-company-sidebar-width 25)

(add-to-list 'company-begin-commands 'ibus-exec-callback)
(add-to-list 'company-begin-commands 'ibus-handle-event)

(setq company-backends
      '((company-capf company-dabbrev company-files)
	(company-dabbrev-code company-gtags company-etags
			      company-keywords)))
(setq company-transformers
      '(company-sort-by-occurrence))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
	company-echo-metadata-frontend))

(defun eh-company-sidebar-format ()
  "show candidates in sidebar"
  (let ((lines 0)
	;; Roll to selection.
	(candidates (nthcdr company-selection company-candidates))
	(i (if company-show-numbers company-selection 99999))
	(metadata (company-fetch-metadata))
	comp msg)
    (while candidates
      (setq comp (company-reformat (pop candidates))
	    lines (+ lines 1))
      (if (< i 10)
	  ;; Add number.
	  (progn
	    (setq comp (propertize (format "%d　%s\n\n" i comp)
				   'face 'company-echo))
	    (cl-incf i)
	    (add-text-properties 3 (+ 3 (length company-common))
				 '(face company-echo-common) comp))
	(setq comp (propertize (format "+　%s\n\n" comp)
			       'face 'company-echo))
	(add-text-properties 0 (length company-common)
			     '(face company-echo-common) comp))
      (if (>= lines 30)
	  (setq candidates nil)
	(push comp msg)))
    (concat
     (when company-search-string
       (format "Search: %s\n"
	       company-search-string))
     (mapconcat 'identity (nreverse msg) ""))))

(defun eh-company-sidebar-show (&optional string)
  (let* ((origin-window (selected-window))
	 (buffer (get-buffer-create eh-company-buffer-name))
	 existing-window window)

    (cl-dolist (win (window-list))
      (and (string= (buffer-name (window-buffer win))
		    eh-company-buffer-name)
	   (not (window-parameter win 'window-side))
	   (eq t (window-deletable-p win))
	   (delete-window win)))

    (setq existing-window
	  (cl-find-if
	   (lambda (win)
	     (and (string= (buffer-name (window-buffer win))
			   eh-company-buffer-name)
		  (window-parameter win 'window-side)))
	   (window-list)))

    (setq window
	  (or existing-window
	      (display-buffer-in-side-window
	       buffer
	       `((side . ,eh-company-sidebar-side)
		 (window-width . ,eh-company-sidebar-width)))))

    (when existing-window
      (setf (window-dedicated-p window) nil
	    (window-buffer window) buffer))
    (setf (window-dedicated-p window) t)

    (with-current-buffer buffer
      (visual-line-mode 1)
      (adaptive-wrap-prefix-mode t)
      (setq adaptive-wrap-extra-indent 3)

      ;; hide fringe and vertical-border
      ;; (let ((color (face-attribute 'default :background)))
      ;;	(set-face-attribute 'vertical-border nil
      ;;			    :foreground color)
      ;;	(set-face-attribute 'fringe nil
      ;;			    :background color))

      ;; hide cursor
      (setq cursor-type nil)
      (setq mode-line-format
	    (list "  "
		  '(:eval (propertize "%b " 'face 'font-lock-keyword-)
			  'help-echo (buffer-file-name))))
      (delete-region (point-min) (point-max))
      (when string
	(insert string)))
    (select-window origin-window)))

(defun eh-company-sidebar-hide ()
  (interactive)
  (let ((window
	 (cl-find-if
	  (lambda (window)
	    (and (string= (buffer-name (window-buffer window))
			  eh-company-buffer-name)
		 (window-parameter window 'window-side)))
	  (window-list))))
    (and (window-live-p window)
	 (window-deletable-p window)
	 (delete-window window))))

(defun eh-display-buffer (orig-fun &rest args)
  "hide company sidebar before pop to buffer"
  (eh-company-sidebar-hide)
  (apply orig-fun args))

(advice-add 'display-buffer :around #'eh-display-buffer)

(defun eh-company-sidebar-frontend (command)
  (pcase command
    (`post-command (eh-company-sidebar-show
		    (eh-company-sidebar-format)))
    (`hide (eh-company-sidebar-show ""))))

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
  (when (eq window-system 'x)
    (eh-company-theme)))

(global-set-key (kbd "M-/") 'company-complete)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "M-i") 'company-complete-selection)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p")'company-select-previous)
(define-key company-active-map (kbd "M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p")'company-select-previous)

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (x)
		(global-company-mode)))
  (global-company-mode))

;;;autoload(require 'eh-complete)
(provide 'eh-complete)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-complete.el ends here
