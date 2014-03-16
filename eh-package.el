(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(defun eh-packages-installed-p ()
  (loop for p in eh-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun eh-install-packages ()
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
