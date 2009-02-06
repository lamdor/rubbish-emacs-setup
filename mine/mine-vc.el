;; Git Integration
(add-path "site-lisp/git/")
(require 'git)
(require 'vc-git)
(require 'git-blame)
(add-to-list 'vc-handled-backends 'GIT)

;; Magit
(add-path "site-lisp/magit/")
(autoload 'magit-status "magit" "use magit git magic" t)

(provide 'mine-vc)