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

(setq emms-source-file-default-directory "~/Music")

(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
(setq emms-track-description-function 'eh-emms-info-track-description)

(when (fboundp 'emms-cache)
  (emms-cache 1))

(setq emms-player-list
      '(emms-player-mpg321
        emms-player-ogg123
        emms-player-mplayer))

(setq emms-info-asynchronously nil)

;; use faster finding facility if you have GNU find
(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)

(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "正在播放: %s")

;; mode line format
(setq emms-mode-line-format "[ %s "
      emms-playing-time-display-format "%s ]")

(setq global-mode-string
      '("" emms-mode-line-string " " emms-playing-time-string))

(defun eh-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((type (emms-track-type track)))
    (cond ((eq 'file type)
           (file-relative-name (emms-track-name track)
                               emms-source-file-default-directory))
          ((eq 'url type)
           (emms-format-url-track-name (emms-track-name track)))
          (t (concat (symbol-name type)
                     ": " (emms-track-name track))))))

(defun eh-emms ()
  "Switch to the current emms-playlist buffer, use
emms-playlist-mode and query for a directory tree
to add to the playlist."
  (interactive)
  (if (or (null emms-playlist-buffer)
	  (not (buffer-live-p emms-playlist-buffer)))
      (emms-add-directory-tree emms-source-file-default-directory ))
  (emms-playlist-mode-go))


;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-emms.el ends here
