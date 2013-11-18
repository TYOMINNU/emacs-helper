;;; eh-org-capture.el --- Tumashu's emacs configuation

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

;; capture模板
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline eh-org-todo-file "Tasks")
         "* TODO %? %^g\n %i \n %a")
        ("u" "Notes: 直接保存" entry  (file+headline eh-org-auto-note-file "Notes")
"** %c
   :PROPERTIES:
   :DATE: %u
   :END:
%i"
:empty-lines 1
:immediate-finish
:kill-buffer)

        ("w" "Notes: 一般事项" entry  (file+headline eh-org-note-file "Notes")
"** %c
   :PROPERTIES:
   :DATE: %u
   :END:
 %i"
:empty-lines 1)

        ("x" "Notes: 学习感悟" entry  (file+headline eh-org-study-note-file "Notes")
"* %?
   :PROPERTIES:
   :DATE: %u
   :LINK: %a
   :END:
%x

%i"
:empty-lines 1)

        ("l" "Link" entry (file+olp eh-org-note-file "Web Links")
         "* %a\n %?\n %i")
        ("m" "account" table-line (file+headline eh-org-account-file "account")
         "|%?||||||%u|")
        ("j" "Journal" entry (file+datetree eh-org-journal-file)
         "* %?\n %U\n %i\n  %a")
        ("s" "Schedule" entry (file+headline eh-org-schedule-file "Schedule")
         "* %?\n %T\n  %a")
        ("v" "Contacts" entry (file eh-org-contacts-file)
               "* %(org-contacts-template-name) %^G
  :PROPERTIES:
  :ALIAS: 
  :NOTE:  
  :EMAIL: %(org-contacts-template-email)
  :PHONE: 
  :IGNORE:
  :END:")
        ("c" "Contacts: 手动输入" entry (file eh-org-contacts-file)
               "* %? %^g
  :PROPERTIES:
  :ALIAS: 
  :NOTE: 
  :EMAIL: %x
  :PHONE: 
  :IGNORE: 
  :END:")))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun eh-org-capture (&optional goto keys)
  "Create a new frame and run org-capture."
  (interactive)
  (let ((after-make-frame-functions
	 (lambda (frame)
	   (progn
	     (select-frame frame)
	     (setq word-wrap 1)
	     (setq truncate-lines nil)
	     (org-capture goto keys)))))
    (make-frame '((name . "org-capture")
		  (window-system . x)
		  (width . 120)
		  (height . 15)))))

;; keybinding
(define-key global-map "\C-ct"
  (lambda () (interactive) (eh-org-capture nil "t")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (eh-org-capture nil "j")))
(define-key global-map "\C-cs"
  (lambda () (interactive) (eh-org-capture nil "s")))
(define-key global-map "\C-cl"
  (lambda () (interactive) (eh-org-capture nil "l")))
(define-key global-map "\C-cw"
  (lambda () (interactive) (eh-org-capture nil "w")))
(define-key global-map "\C-cx"
  (lambda () (interactive) (eh-org-capture nil "x")))
(define-key global-map "\C-cv"
  (lambda () (interactive) (eh-org-capture nil "v")))
(define-key global-map "\C-cc"
  (lambda () (interactive) (eh-org-capture nil "c")))
(define-key global-map "\C-cm"
  (lambda () (interactive) (eh-org-capture nil "m")))

(provide 'eh-org-capture)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-capture.el ends here
