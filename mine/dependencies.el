;; eeio
(add-path "site-lisp/eieio")
(require 'eieio)

;; Semantic
(add-path "site-lisp/semantic/")
(setq semantic-load-turn-everything-on t)
(require 'semantic-load)

;; speedbar
(add-path "site-lisp/speedbar/")
(require 'speedbar)

;; Git Integration
(add-path "site-lisp/git/")
(require 'git)
(require 'vc-git)
(require 'git-blame)
(add-to-list 'vc-handled-backends 'GIT)

;; yasnippet
(add-path "site-lisp/yasnippet-0.5.4/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-root "site-lisp/yasnippet-0.5.4/snippets/"))

;; Misc Files
(add-path "el/")
(require 'find-recursive)
(require 'tail)
(require 'keywiz)
(require 'unit-test)

;; ido-mode
(require 'ido)
(ido-mode t)
