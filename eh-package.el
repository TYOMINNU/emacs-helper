(require 'package)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(locate-library "eh-basic")

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Add in your own as you wish:
;; (defvar eh-packageas-files '("eh-basic"
;;                            "eh-functions"
;;                            "eh-gnus"
;;                            "eh-keybindings"
;;                            "eh-org"
;;                            "eh-complete"
;;                            "eh-misc"))
;;(package-install-file (locate-library "eh-basic"))
;;;;   "A list of packages to ensure are installed at launch.")
;;(dolist (p eh-packages-files)
;;    (package-install-file (locate-library p)))

(provide 'eh-package)






