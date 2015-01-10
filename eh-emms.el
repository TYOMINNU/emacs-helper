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
(require 'dired)
(require 'emms-setup)
(require 'chinese-pyim-pinyin)
(emms-devel)
(emms-default-players)
(require 'emms-info-libtag)
(when (fboundp 'emms-cache) (emms-cache 1))

;; EMMS 目录
(setq emms-source-file-default-directory "~/Music")
(setq emms-directory "~/Music/.emms/")
(setq emms-history-file "~/Music/.emms/history")
(setq emms-cache-file "~/Music/.emms/cache")
(setq emms-stream-bookmarks-file "~/Music/.emms/streams")
(setq emms-score-file "~/Music/.emms/scores")

                                        ;设定EMMS主模式为Playlist模式
(setq emms-playlist-default-major-mode 'emms-playlist-mode)

                                        ;修复播放完后的BUG
(setq emms-player-next-function 'emms-next)

;; 设定音轨初始化信息
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)

;; 关闭EMMS信息异步模式
(setq emms-info-asynchronously nil)

;; 设定EMMS启动列表循环播放
(setq emms-repeat-playlist t)

;; 排序方法: 艺术家 -> 专辑 -> 序号
(setq emms-playlist-sort-function
      'emms-playlist-sort-by-natural-order)

;; 使用Gnu find查找文件
(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)

;; 在minibuffer中显示播放信息(emms-show)
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "正在播放: [%s]")

;;设置Mode-line的显示方式
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

;; 显示歌词
(emms-lyrics 1)
(setq emms-lyrics-display-on-modeline t)


;; Function used to format track
(setq emms-track-description-function (lambda (track) (concat " " (eh-emms-make-track-description track))))

;; 设置Playlist的显示方式
(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%H:%M")
        (604800                           . "%H:%M")
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m-%d")
        (t                                . "%Y")))

(defun eh-emms-make-track-description (track)
  "Return a description of the current track."
  (let ((track-type (emms-track-type track))
        (play-count (or (emms-track-get track 'play-count) 0))
        (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
        (name (emms-track-name track))
        (pmin (emms-track-get track 'info-playing-time-min))
        (psec (emms-track-get track 'info-playing-time-sec))
        (ptot (emms-track-get track 'info-playing-time))
        (title (emms-track-get track 'info-title))
        (artist (emms-track-get track 'info-artist))
        (album (emms-track-get track 'info-album)))
    (if (eq 'file track-type)
        (format "%5s %3s |-> %-s"
                (emms-last-played-format-date last-played)
                play-count
                (cond ((and pmin psec) (format "%s %s -- %s [%02d:%02d]" artist album title pmin psec))
                      (ptot (format  "%s %s -- %s [%02d:%02d]" artist album title (/ ptot 60) (% ptot 60)))
                      (t (format "%s %s -- %s" artist album  title)))))))


;; Function used to get music tags, for example IDv2.3!
(setq emms-info-functions '(eh-emms-info-libtag eh-emms-info-add-pinyin-alias))

(defun eh-emms-info-libtag (track)
  (when (and (eq 'file (emms-track-type track))
             (string-match
              "\\.\\([Mm][Pp]3\\|[oO][gG][gG]\\|[fF][lL][aA][cC]\\|[sS][pP][xX]\\)\\'"
              (emms-track-name track)))
    (let ((info-list
           (split-string (file-relative-name
                          (emms-track-name track)
                          emms-source-file-default-directory) "/" t)))
      (emms-track-set track 'info-artist (if (> (length info-list) 1) (nth 0 info-list) "未知艺术家"))
      (emms-track-set track 'info-album  (if (> (length info-list) 2) (nth 1 info-list) "杂项"))
      (emms-track-set track 'info-title (car (reverse info-list))))
    (with-temp-buffer
      (when (string= "0"
                     (format "%s" (let ((coding-system-for-read 'utf-8))
                                    (call-process emms-info-libtag-program-name
                                                  nil '(t nil) nil
                                                  (emms-track-name track)))))
        (goto-char (point-min))
        ;; Crush the trailing whitespace
        (while (re-search-forward "[[:space:]]+$" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
          (let ((name (intern-soft (match-string 1)))
                (value (match-string 2)))
            (when (> (length value)
                     0)
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
                                  (string-to-number value)
                                value))))
          (forward-line 1))))))

(defun eh-emms-info-add-pinyin-alias (track)
  "Add pinyin alias to the track"
  (when (and (featurep 'chinese-pyim-pinyin)
             (eq 'file (emms-track-type track)))
    (emms-track-set track 'info-artist-alias (pyim-hanzi2pinyin (emms-track-get track 'info-artist) t))
    (emms-track-set track 'info-album-alias (pyim-hanzi2pinyin (emms-track-get track 'info-album) t))
    (emms-track-set track 'info-title-alias (pyim-hanzi2pinyin (emms-track-get track 'info-title) t))))

;;; 设置EMMS 浏览器
;; 默认显示方式为: 显示所有
(emms-browser-set-filter (assoc "EVERYTHING" emms-browser-filters))
;; filter: 显示所有
(emms-browser-make-filter "EVERYTHING" 'ignore)
;; filter: 只显示文件
(emms-browser-make-filter "ALL-FILES" (emms-browser-filter-only-type 'file))
;; filter: 最近一个星期播放的
(emms-browser-make-filter "LAST-WEEK" (emms-browser-filter-only-recent 7))
;; filter: 最近一个月都没有播放的
(emms-browser-make-filter "LAST-MONTH-NOT-PLAYED" (lambda (track) (not (funcall (emms-browser-filter-only-recent 30) track))))
;; EMMS 浏览器, 删除文件不提醒
(put 'emms-browser-delete-files 'disabled nil)

;; 设置 emms buffer 显示格式
(setq emms-browser-info-artist-format "* %n")
(setq emms-browser-info-album-format  "  - %n")
(setq emms-browser-info-title-format  "    ♪. %n")
(setq emms-browser-playlist-info-title-format "%n")

;; 自定义emms-browser-add-tracks, 禁止在playlist文件中添加
;; artist行 和 album行，同时使emacs-browser-playlist-*-*-format
;; 中 "%i"位置符失效
;;
;; 注: emms-browser-playlist-info-artist-format
;;     emms-browser-playlist-info-album-format
;;     两个变量设置在这里不起作用

(defun eh-emms-browser-add-tracks ()
  "Add all tracks at point.
Return the previous point-max before adding."
  (interactive)
  (let ((first-new-track (with-current-emms-playlist (point-max)))
        (bdata (emms-browser-bdata-at-point)))
    (eh-emms-browser-playlist-insert-bdata bdata)
    (run-hook-with-args 'emms-browser-tracks-added-hook
                        first-new-track)
    first-new-track))

(defun eh-emms-browser-playlist-insert-bdata (bdata)
  "Add all tracks in BDATA to the playlist."
  (let ((type (emms-browser-bdata-type bdata)))
    ;; recurse or add tracks
    (dolist (item (emms-browser-bdata-data bdata))
      (if (not (eq type 'info-title))
          (eh-emms-browser-playlist-insert-bdata item)
        (emms-browser-playlist-insert-track bdata)))))

(defun eh-emms-browser-make-name (entry type)
  "Override `emms-browser-make-name'. Return a name for ENTRY, used for making a bdata object."
  (let ((key (car entry))
        (track (cadr entry))
        artist title) ;; only the first track
    (cond
     ((eq type 'info-title)
      (eh-emms-make-track-description track))
     (t key))))

(advice-add 'emms-browser-make-name :override #'eh-emms-browser-make-name)

;; 快捷函数
(defun eh-emms-toggle-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

(defun eh-emms-search ()
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))

(defun eh-emms ()
  (interactive)
  (if (or (null emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      (let ((playlist (concat
                       (file-name-as-directory emms-source-file-default-directory)
                       "default.playlist")))
        (if (not (file-readable-p playlist))
            (eh-emms-add-directory-tree)
          (emms-add-playlist playlist))))
  (emms-playlist-mode-go))

(defun eh-emms-add-directory-tree ()
  (interactive)
  (emms-add-directory-tree
   (ido-read-directory-name
    "Add directory tree:"
    emms-source-file-default-directory)))

(defun eh-emms-add-file ()
  (interactive)
  (let ((file (ido-read-file-name
               "Add directory tree:"
               emms-source-file-default-directory)))
    (cond
     ((string-match "\\.\\(m3u\\|pls\\)\\'" file)
      (emms-add-playlist file))
     (t (emms-add-file file)))))

(defun eh-emms-browser-search-by-names ()
  (interactive)
  (emms-browser-search '(info-artist info-artist-alias info-title info-title-alias info-album info-album-alias)))

(defun eh-emms-browser-search-by-artist ()
  (interactive)
  (emms-browser-search '(info-artist info-artist-alias)))

(defun eh-emms-browser-search-by-title ()
  (interactive)
  (emms-browser-search '(info-title info-title-alias)))


;; Global keybinding for emms
(global-unset-key (kbd "C-c e"))
(global-set-key (kbd "C-c e e") 'eh-emms)
(global-set-key (kbd "C-c e d") 'eh-emms-add-directory-tree)
(global-set-key (kbd "C-c e f") 'eh-emms-add-file)

(global-set-key (kbd "C-c e SPC") 'eh-emms-toggle-playing)
(global-set-key (kbd "C-c e q") 'emms-stop)

(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e o") 'emms-show)

(global-set-key (kbd "C-c e h") 'emms-shuffle)
(global-set-key (kbd "C-c e H") 'emms-sort)

(global-set-key (kbd "C-c e r")   'emms-toggle-repeat-track)
(global-set-key (kbd "C-c e R")   'emms-toggle-repeat-playlist)

(global-set-key (kbd "C-c e s u") 'emms-score-up-playing)
(global-set-key (kbd "C-c e s d") 'emms-score-down-playing)
(global-set-key (kbd "C-c e s o") 'emms-score-show-playing)

;; browser mode map
(define-key emms-browser-mode-map (kbd "SPC") 'emms-browser-next-non-track)
(define-key emms-browser-mode-map (kbd "<return>") (lambda ()
                                                     (interactive)
                                                     (eh-emms-browser-add-tracks)
                                                     (message "Add current track to playlist")))
(define-key emms-browser-mode-map (kbd "C-SPC") 'emms-browser-next-non-track)
(define-key emms-browser-mode-map (kbd "<tab>") 'emms-browser-toggle-subitems)
(define-key emms-browser-mode-map (kbd "o") 'emms-playlist-mode-go)
(define-key emms-browser-mode-map (kbd "w") 'emms-browser-show-LAST-WEEK)
(define-key emms-browser-mode-map (kbd "a") 'emms-browser-show-EVERYTHING)
(define-key emms-browser-mode-map (kbd "m") 'emms-browser-show-LAST-MONTH-NOT-PLAYED)
(define-key emms-browser-mode-map (kbd "s s") 'eh-emms-browser-search-by-names)
(define-key emms-browser-mode-map (kbd "s a") 'eh-emms-browser-search-by-artist)
(define-key emms-browser-mode-map (kbd "s t") 'eh-emms-browser-search-by-title)

;; playlist-mode-map
(define-key emms-playlist-mode-map (kbd "o") 'emms-browser-show-LAST-WEEK)
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
