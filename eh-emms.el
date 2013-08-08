;;; eh-emms.el --- Tumashu's emacs configuation

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
(require 'emms-setup)
(emms-devel)
(emms-default-players)
(when (fboundp 'emms-cache) (emms-cache 1))

(setq emms-source-file-default-directory "~/Music")
(setq emms-playlist-buffer-name "*Music*")

(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
(setq emms-track-description-function 'eh-emms-info-track-description)
(setq emms-info-asynchronously nil)

;; Use faster finding facility if you have "GNU find"
(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)

;; Show playing track in the minibuffer
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "正在播放: [%s]")

;; Mode-line format
(setq emms-mode-line-format "%s")
(setq emms-playing-time-display-format "%s ]")
(setq global-mode-string
      '(" " emms-mode-line-string " " emms-playing-time-string " "))
(setq emms-mode-line-mode-line-function
      'eh-emms-mode-line-playlist-current)

(defun eh-emms-mode-line-playlist-current ()
  "Format the currently playing."
  (let ((track (emms-playlist-current-selected-track)))
    (if (eq 'file (emms-track-type track))
        (if (and (emms-track-get track 'info-artist)
                 (emms-track-get track 'info-title))
            (let ((art  (emms-track-get track 'info-artist))
                  (tit  (emms-track-get track 'info-title)))
              (format "[ %s -- %s" art tit))
          (format "[ %s"
                  (file-relative-name (emms-track-name track)
                                      emms-source-file-default-directory))))))

;; lyrics
(emms-lyrics 1)
(setq emms-lyrics-display-on-modeline t)

(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%H:%M")
	(604800                           . "%H:%M")
	((emms-last-played-seconds-month) . "%d")
	((emms-last-played-seconds-year)  . "%m-%d")
	(t                                . "%Y")))

(defun eh-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((type (emms-track-type track))
        (play-count (or (emms-track-get track 'play-count) 0))
        (last-played (or (emms-track-get track 'last-played) '(0 0 0))))
    (if (eq 'file type)
        (format "%5s %3s |-> %-s"
                (emms-last-played-format-date last-played)
                play-count
                (if (and (emms-track-get track 'info-artist)
                         (emms-track-get track 'info-title))
                    (let ((pmin (emms-track-get track 'info-playing-time-min))
                          (psec (emms-track-get track 'info-playing-time-sec))
                          (ptot (emms-track-get track 'info-playing-time))
                          (art  (emms-track-get track 'info-artist))
                          (tit  (emms-track-get track 'info-title)))
                      (cond ((and pmin psec) (format "%s -- %s [%02d:%02d]" art tit pmin psec))
                            (ptot (format  "%s -- %s [%02d:%02d]" art tit (/ ptot 60) (% ptot 60)))
                            (t (format "%s -- %s" art tit))))
                  (replace-regexp-in-string "\\([^\\.]\\)/.*/" "\\1/.../"
                   (file-relative-name (emms-track-name track)
                                       emms-source-file-default-directory)))))))

(defun eh-emms ()
  "Switch to the current emms-playlist buffer, use
emms-playlist-mode and query for a directory tree
to add to the playlist."
  (interactive)
  (if (or (null emms-playlist-buffer)
	  (not (buffer-live-p emms-playlist-buffer)))
      (emms-add-directory-tree emms-source-file-default-directory ))
  (emms-playlist-mode-go))

(defun eh-emms-toggle-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

(defun eh-emms-add-directory ()
  (interactive)
  (call-interactively 'emms-add-directory-tree)
  (emms-playlist-mode-go))

(defun eh-emms-search ()
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))

;; Global keybinding for emms
(global-unset-key (kbd "C-c e"))
(global-set-key (kbd "C-c e x") 'eh-emms)
(global-set-key (kbd "C-c e f") 'emms-play-file)
(global-set-key (kbd "C-c e l") 'emms-play-playlist)
(global-set-key (kbd "C-c e d") 'emms-play-directory-tree)

(global-set-key (kbd "C-c e a") 'eh-emms-add-directory)

(global-set-key (kbd "C-c e e") 'eh-emms-toggle-playing)
(global-set-key (kbd "C-c e q") 'emms-stop)


(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e o") 'emms-show)

(global-set-key (kbd "C-c e h") 'emms-shuffle)

(global-set-key (kbd "C-c e r")   'emms-toggle-repeat-track)
(global-set-key (kbd "C-c e R")   'emms-toggle-repeat-playlist)

(global-set-key (kbd "C-c e s u") 'emms-score-up-playing)
(global-set-key (kbd "C-c e s d") 'emms-score-down-playing)
(global-set-key (kbd "C-c e s o") 'emms-score-show-playing)

;; playlist-mode-map
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "/") 'eh-emms-search)
(define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
(define-key emms-playlist-mode-map (kbd "C-<right>") (lambda () (interactive) (emms-seek +10)))
(define-key emms-playlist-mode-map (kbd "C-<left>") (lambda () (interactive) (emms-seek -10)))
(define-key emms-playlist-mode-map (kbd "C-<right>") (lambda () (interactive) (emms-seek +60)))
(define-key emms-playlist-mode-map (kbd "C-<left>") (lambda () (interactive) (emms-seek -60)))
(define-key emms-playlist-mode-map (kbd "S u") 'emms-score-up-file-on-line)
(define-key emms-playlist-mode-map (kbd "S d") 'emms-score-down-file-on-line)
(define-key emms-playlist-mode-map (kbd "S o") 'emms-score-show-file-on-line)
(define-key emms-playlist-mode-map (kbd "S l") 'emms-score-less-tolerant)
(define-key emms-playlist-mode-map (kbd "S m") 'emms-score-more-tolerant)
(define-key emms-playlist-mode-map (kbd "S t") 'emms-score-set-tolerance)
(define-key emms-playlist-mode-map (kbd "S s") 'emms-score-show-playing)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-emms.el ends here
