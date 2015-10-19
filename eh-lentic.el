;;; eh-lentic.el --- Tumashu's basic emacs configuation

;; Copyright (c) 2011 2015, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.3
;; Package-Requires: ((starter-kit "2.0.2"))

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

(use-package lentic
  :ensure nil
  :config

  (use-package lentic-org
    :ensure nil
    :config

    (defclass lentic-org2el-configuration
      (lentic-unmatched-chunk-configuration
       lentic-uncommented-chunk-configuration)
      ())

    (defmethod lentic-clone
      ((conf lentic-org2el-configuration)
       &optional start stop length-before
       start-converted stop-converted)
      ;; do everything else to the buffer
      (m-buffer-with-markers
          ((first-line
            (m-buffer-match-first-line
             (lentic-this conf)))
           (header-one-line
            (m-buffer-match
             (lentic-this conf)
             "^[*] +\\(\\w*\\)$"
             :begin (cl-cadar first-line)))
           (special-lines
            (-concat first-line header-one-line)))
        ;; check whether we are in a special line -- if so widen the change extent
        (let* ((start-in-special
                (when (and start
                           (m-buffer-in-match-p
                            special-lines start))
                  (m-buffer-at-line-beginning-position
                   (lentic-this conf)
                   start)))
               (start (or start-in-special start))
               (start-converted
                (if start-in-special
                    (m-buffer-at-line-beginning-position
                     (lentic-that conf)
                     start-converted)
                  start-converted))
               (stop-in-special
                (when (and stop
                           (m-buffer-in-match-p
                            special-lines stop))
                  (m-buffer-at-line-end-position
                   (lentic-this conf)
                   stop)))
               (stop (or stop-in-special stop))
               (stop-converted
                (if stop-in-special
                    (m-buffer-at-line-end-position
                     (lentic-that conf)
                     stop-converted)
                  stop-converted))
               (clone-return
                (call-next-method conf start stop length-before
                                  start-converted stop-converted))
               (c1 (m-buffer-replace-match
                    (m-buffer-match
                     (lentic-that conf)
                     "^;; ;;; ")
                    ";;; ")))
          (if (or start-in-special stop-in-special c1)
              nil
            clone-return))))

    (defmethod lentic-convert
      ((conf lentic-org2el-configuration)
       location)
      (let ((converted (call-next-method conf location)))
        (m-buffer-with-current-position
            (oref conf :this-buffer)
            location
          (beginning-of-line)
          (if (looking-at "[*] +\\w*$")
              (- converted 1)
            converted))))

    (defmethod lentic-invert
      ((conf lentic-org2el-configuration))
      (lentic-m-oset
       (lentic-el2org-init)
       :that-buffer
       (lentic-this conf)))

    (defun lentic-org2el-and-el2org-oset (conf)
      (lentic-m-oset
       conf
       :this-buffer (current-buffer)
       :comment ";; "
       :comment-stop "#\\\+BEGIN_SRC emacs-lisp.*"
       :comment-start "#\\\+END_SRC"))

;;;###autoload
    (defun lentic-org2el-init ()
      (lentic-org2el-and-el2org-oset
       (lentic-org2el-configuration
        "lb-org2el"
        :lentic-file
        (concat
         (file-name-sans-extension
          (buffer-file-name))
         ".el"))))

    (add-to-list 'lentic-init-functions
                 'lentic-org2el-init)

    (defclass lentic-el2org-configuration
      (lentic-unmatched-chunk-configuration
       lentic-commented-chunk-configuration)
      ())

    (defmethod lentic-invert
      ((conf lentic-el2org-configuration))
      (lentic-m-oset
       (lentic-org2el-init)
       :delete-on-exit t
       :that-buffer (lentic-this conf)))

;;;###autoload
    (defun lentic-el2org-init ()
      (lentic-org2el-and-el2org-oset
       (lentic-el2org-configuration
        "lb-el2org"
        ;; we don't really need a file and could cope without, but org mode assumes
        ;; that the buffer is file name bound when it exports. As it happens, this
        ;; also means that file saving is possible which in turn saves the el file
        :lentic-file
        (concat
         (file-name-sans-extension
          (buffer-file-name))
         ".org"))))

    (add-to-list 'lentic-init-functions
                 'lentic-el2org-init)
    ))

;;;###autoload(require 'eh-lentic)
(provide 'eh-lentic)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-lentic.el ends here
