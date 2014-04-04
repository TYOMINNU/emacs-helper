;;; eh-mode-line.el --- Tumashu's emacs configuation

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

;; 简化mode-line的显示(use setq-default to set it for /all/ modes)
(setq eh-mode-line-coding-format
      '(:eval
        (let* ((code (upcase (symbol-name buffer-file-coding-system)))
               (eol-type (coding-system-eol-type buffer-file-coding-system))
               (eol-string (if (eq 0 eol-type) "UNIX"
			     (if (eq 1 eol-type) "DOS"
			       (if (eq 2 eol-type) "MAC"
				 "???")))))
	  (if (string-match-p eol-string code)
	      code
	    (concat code " " eol)))))

(setq-default mode-line-format
	      (list
	       '(:eval (if buffer-read-only
			   (propertize "只读  "
				       'face 'font-lock-type-face
				       'help-echo "Buffer is read-only")
			 (if (buffer-modified-p)
			     (propertize "更改  "
					 'face 'font-lock-warning-face
					 'help-echo "Buffer has been modified")
			   (propertize (format-time-string "%H:%M ")
				       'help-echo
				       (concat (format-time-string "%c; ")
					       (emacs-uptime "Uptime:%hh"))))))
	       ;; buffer name
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name)))
	       ;; line and column
	       (propertize "(%02l,%02c) " 'face 'font-lock-type-face)
	       ;; position and size
	       (propertize "%p/%I " 'face 'font-lock-constant-face)
	       ;; current major mode
	       '(:eval (propertize "[%m] " 'face 'font-lock-string-face
				   'help-echo (symbol-name buffer-file-coding-system)))
	       ;; current minor-mode, emms song info or sdcv translation
	       '(:eval (if (= 0 (length eh-sdcv-mode-line-string))
			   (if emms-player-playing-p
			       (list emms-mode-line-string " " emms-playing-time-string)
			     (list "( " eh-mode-line-coding-format  minor-mode-alist " )"))
			 eh-sdcv-mode-line-string))
	       ;; show: -------
	       " %-"))

(provide 'eh-mode-line)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-mode-line.el ends here
