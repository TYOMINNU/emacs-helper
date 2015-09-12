;;; eh-exwm.el --- Tumashu's basic emacs configuation

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.3

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

(use-package exwm
  :if (string= (getenv "eh_enable_exwm") "yes")
  :ensure nil
  :demand t
  :config

  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Shrink fringes to 1 pixel
  (fringe-mode 1)

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  ;; when a new window class name or title is available. Here's some advice on
  ;; this subject:
  ;; + Always set the second argument of `rename-buffer' to `t' to avoid naming
  ;;   conflict.
  ;; + Only renaming buffer in one hook and avoid it in the other. There's no
  ;;   guarantee on the order in which they are run.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of all
  ;;   windows are probably the same. Using window titles for them makes more
  ;;   sense.
  ;; + Some application change its title frequently (e.g. browser, terminal).
  ;;   Its class name may be more suitable for such case.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            #'(lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer (concat "Exwm:" exwm-class-name)))))
  (add-hook 'exwm-update-title-hook
            #'(lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer (concat "Exwm:" exwm-title)))))

  ;; `exwm-input-set-key' allows you to set a global key binding (available in
  ;; any case). Following are a few examples.
  ;; + We always need a way to go back to line-mode from char-mode
  (exwm-input-set-key (kbd "s-r") 'exwm-reset)
  ;; + Bind a key to switch workspace interactively
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  ;; + Set shortcuts to switch to a certain workspace.
  (exwm-input-set-key (kbd "s-0")
                      #'(lambda ()
                          (interactive)
                          (exwm-workspace-switch 0)))
  (exwm-input-set-key (kbd "s-1")
                      #'(lambda ()
                          (interactive)
                          (exwm-workspace-switch 1)))
  (exwm-input-set-key (kbd "s-2")
                      #'(lambda ()
                          (interactive)
                          (exwm-workspace-switch 2)))
  (exwm-input-set-key (kbd "s-3")
                      #'(lambda ()
                          (interactive)
                          (exwm-workspace-switch 3)))
  ;; + Application launcher ('M-&' also works if the output buffer does not
  ;;   bother you). Note that there is no need for processes to be created by
  ;;   Emacs.
  (exwm-input-set-key (kbd "s-&")
                      #'(lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))
  ;; + 'slock' is a simple X display locker provided by suckless tools. 'i3lock'
  ;;   is a more feature-rich alternative.
  (exwm-input-set-key (kbd "s-<f2>")
                      #'(lambda ()
                          (interactive)
                          (start-process "" nil "slock")))

  ;; The following example demonstrates how to set a key binding only available
  ;; in line mode. It's simply done by first push the prefix key to
  ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
  ;; The example shorten 'C-c q' to 'C-q'.
  (push ?\C-q exwm-input-prefix-keys)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic the
  ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
  ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
  ;; sequence (of type vector or string), while DEST can also be a single key.
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)))

  ;; Debian menu
  (defun exwm-generate-debian-menu-commands ()
    (load "/var/lib/emacs/exwm/exwm-menu.el")
    (mapc #'(lambda (x)
              (let* ((debian-menu-command (nth 1 x))
                     (debian-menu-name (nth 2 x))
                     (exwm-command-name
                      (concat "EXWM/"
                              (replace-regexp-in-string
                               "^-\\|-$" ""
                               (replace-regexp-in-string
                                "-+" "-"
                                (replace-regexp-in-string
                                 "[^a-zA-Z0-9]" "-"
                                 (replace-regexp-in-string
                                  "/Debian\\|/Applications" ""
                                  debian-menu-name)))))))
                (eval `(defun ,(intern exwm-command-name) ()
                         (interactive)
                         (start-process-shell-command ,exwm-command-name nil ,debian-menu-command)))))
          exwm-debian-menu-alist))
  (exwm-generate-debian-menu-commands)

  ;; Do not forget to enable EXWM. It will start by itself when things are ready.
  (exwm-enable)

  ;; Active exim
  (require 'exim)
  (add-hook 'exwm-init-hook 'exim-start))

(provide 'eh-exwm)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-exwm.el ends here
