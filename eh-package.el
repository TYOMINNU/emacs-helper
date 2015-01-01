(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(setq package-unsigned-archives '("gnu"))

(defun eh-save-package-list ()
  (interactive)
  (let ((file (concat (file-name-directory
                       (locate-library "eh-package.el"))
                      "eh-packages-list.el"))
        (pkg-list (delete-dups package-activated-list)))
    (when (yes-or-no-p (concat "Do you want to save package list to file: " file "?"))
      (save-excursion
        (with-temp-file file
          (insert (concat ";; Update date: " (format-time-string "%Y-%m-%d") "\n\n"))
          (insert (format "(let ((eh-packages '%s)) (eh-install-packages-1))" pkg-list)))
        (message "Save Success!")))))

(defun eh-install-package ()
  (interactive)
  (let ((file (concat (file-name-directory
                       (locate-library "eh-package.el"))
                      "eh-packages-list.el"))
        (pkg-list (delete-dups package-activated-list)))
    (when (yes-or-no-p (concat "Do you want to install packages listed in file: " file "?"))
      (load file))))

(defun eh-packages-installed-p ()
  (loop for p in eh-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun eh-install-packages-1 ()
  (unless (eh-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p eh-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

(provide 'eh-package)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-package.el ends here
