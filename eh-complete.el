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

;; ido
(use-package ido
  :ensure nil
  :config

  (use-package tramp
    :config
    (setq tramp-default-method nil))
  (use-package org
    :config
    (setq org-completion-use-ido t))
  (use-package ido-ubiquitous
    :config
    (setq ido-everywhere t)
    (ido-ubiquitous-mode 1))
  (use-package flx-ido
    :config
    (setq flx-ido-use-faces t)
    (flx-ido-mode 1))
  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode 1))

  (setq ido-enable-flex-matching t
        ido-enable-regexp t
        ido-enable-prefix nil
        ido-create-new-buffer 'always
        ido-file-extensions-order '(".org" ".R" ".el" ".java" ".js" ".el" ".xml")
        ido-use-filename-at-point 'guess
        ido-auto-merge-work-directories-length -1
        ido-auto-merge-delay-time 2
        ido-use-url-at-point t
        ido-use-faces nil
        gc-cons-threshold 20000000)

  (ido-mode -1)

  ;; ido sort
  (add-hook 'ido-make-file-list-hook
            'eh-ido-sort-mtime) ; 文件的排序方法
  (add-hook 'ido-make-dir-list-hook
            'eh-ido-sort-mtime)  ; 目录的排序方法

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

  ;; ido keybindings
  (add-hook 'ido-setup-hook 'eh-ido-keybinding)
  (defun eh-ido-keybinding ()
    (define-key ido-completion-map (kbd "C-SPC") nil)
    (define-key ido-completion-map (kbd "C-@") nil)
    (define-key ido-completion-map (kbd "C-i") 'ido-edit-input)
    (define-key ido-completion-map (kbd "C-l") 'ido-delete-backward-updir))
  (global-set-key (kbd "C-x C-b") 'ido-display-buffer))

;; smex swiper and ivy-mode
(use-package smex
  :config
  (setq smex-completion-method 'ivy)
  (smex-initialize))

(use-package swiper
  :config

  (use-package counsel
    :config
    (use-package smex
      :config
      (setq smex-completion-method 'ivy)
      (smex-initialize))
    (define-key counsel-find-file-map (kbd "C-f") 'eh-ivy-open-typed-path)
    :bind
    (("C-c C-r" . ivy-resume)
     ("M-x" . counsel-M-x)
     ("C-x C-f" . counsel-find-file)
     ("C-h f" . counsel-describe-function)
     ("C-h v" . counsel-describe-variable)))

  (use-package org
    :ensure nil
    :config
    (org-defkey org-mode-map (kbd "C-c C-c") 'counsel-org-tag))

  (ivy-mode 1)
  (setq ivy-count-format ""
        ;; ivy-count-format "%-2d "
        ivy-extra-directories nil
        ivy-format-function 'eh-ivy-format-function)

  (defun eh-ivy-format-function (cands)
    (let ((i -1))
      (mapconcat
       (lambda (s)
         (concat (if (eq (cl-incf i) ivy--index)
                     "> "
                   "  ")
                 s))
       cands "\n")))

  (defun eh-ivy-return-recentf-index (dir)
    (when (and (boundp 'recentf-list)
               recentf-list)
      (let ((files-list
             (cl-subseq recentf-list
                        0 (min (- (length recentf-list) 1) 20)))
            (index 0))
        (while files-list
          (if (string-match-p dir (car files-list))
              (setq files-list nil)
            (setq index (+ index 1))
            (setq files-list (cdr files-list))))
        index)))

  (defun eh-ivy-sort-file-function (x y)
    (let* ((x (concat ivy--directory x))
           (y (concat ivy--directory y))
           (x-mtime (nth 5 (file-attributes x)))
           (y-mtime (nth 5 (file-attributes y))))
      (if (file-directory-p x)
          (if (file-directory-p y)
              (let ((x-recentf-index (eh-ivy-return-recentf-index x))
                    (y-recentf-index (eh-ivy-return-recentf-index y)))
                (if (and x-recentf-index y-recentf-index)
                    ;; Directories is sorted by `recentf-list' index
                    (< x-recentf-index y-recentf-index)
                  (string< x y)))
            t)
        (if (file-directory-p y)
            nil
          ;; Files is sorted by mtime
          (time-less-p y-mtime x-mtime)))))

  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-function))

  (defun eh-open-typed-path (path)
    (let ((parent-directory
           (if (file-directory-p path)
               (file-name-directory (directory-file-name path))
             (file-name-directory path))))
      (find-file (concat parent-directory ivy-text))))

  (ivy-set-actions
   'counsel-find-file
   '(("f" eh-open-typed-path  "Open typed path")))

  (defun eh-ivy-open-typed-path ()
    (interactive)
    (when ivy--directory
      (ivy-set-action 'eh-open-typed-path)
      (ivy-done)))

  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

;; company-mode
(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-echo-delay 0)
  (setq company-global-modes
        '(not message-mode git-commit-mode eshell-mode
              sfh/sawfish-console-mode))

  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-require-match nil)

  (setq company-backends
        '((company-capf company-dabbrev company-files)
          (company-dabbrev-code company-gtags company-etags
                                company-keywords)))
  (setq company-transformers
        '(company-sort-by-occurrence))

  (setq company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  (global-set-key (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-i") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p")'company-select-previous)
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p")'company-select-previous)

  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                (lambda (x)
                  (global-company-mode)))
    (global-company-mode)))

;;;autoload(require 'eh-complete)
(provide 'eh-complete)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-complete.el ends here
