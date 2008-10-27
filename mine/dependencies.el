;; Git Integration
(add-path "site-lisp/git/")
(require 'git)
(require 'vc-git)
(require 'git-blame)
(add-to-list 'vc-handled-backends 'GIT)

;; Magit
(add-path "site-lisp/magit/")
(autoload 'magit-status "magit" "use magit git magic" t)

;; Gist
(add-path "site-lisp/gist/")
(require 'gist)
  
;; Color theme
(add-path "site-lisp/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(load-file (concat emacs-root "el/color-theme-twilight.el"))
(color-theme-twilight)

;; yasnippet
(add-path "site-lisp/yasnippet-0.5.6/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-root "site-lisp/yasnippet-0.5.6/snippets/"))

;; nxml-mode
(load-file (concat emacs-root "site-lisp/nxml-mode-20041004/rng-auto.el"))
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
(setq nxml-slash-auto-complete-flag t)

;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

;; Misc Files
(add-path "el/")
(require 'find-recursive)
(require 'tidy)
(require 'tail)
(require 'keywiz)
(require 'unit-test)

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(autoload 'yaml-mode "yaml-mode" "Major mode for editting yaml files" t)

;; ido-mode
(require 'ido)
(ido-mode t)
