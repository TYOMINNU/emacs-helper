;; (require 'eh-package)
;; (insert (format "\n\n(let ((eh-packages '%s)) \n (eh-install-packages))" package-activated-list))

(let ((eh-packages '(ace-jump-buffer dash ace-jump-mode ace-jump-mode auctex audio-notes-mode auto-shell-command popwin deferred cdlatex ess-R-data-view ess popup ctable ess-R-object-popup ess popup ess-smart-underscore ess find-file-in-project flx-ido flx gntp google-contacts oauth2 grep+ idle-highlight-mode ido-hacks ido-ubiquitous ido-vertical-mode magit git-rebase-mode git-commit-mode mc-extras multiple-cursors multiple-cursors oauth2 package+ paredit phi-search plantuml-mode auto-complete popup popup-kill-ring pos-tip popup popwin pos-tip projectile pkg-info epl dash s recentf-ext request-deferred request deferred s smex yagist yasnippet))) 
 (eh-install-packages))


