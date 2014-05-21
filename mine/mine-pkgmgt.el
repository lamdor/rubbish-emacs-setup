;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; use-package
;; https://github.com/jwiegley/use-package

(setq use-package-dir (concat user-emacs-directory "/lisp/use-package"))
(if (not (file-exists-p use-package-dir))
    (error "use-package is missing; git submodule init; git submodule update"))
(add-to-list 'load-path use-package-dir)
(require 'use-package)

;; general

(use-package diminish
  :ensure t
  :config
  (progn
    (eval-after-load 'eldoc '(diminish 'eldoc-mode))
    (eval-after-load 'flyspell '(diminish 'flyspell-mode))
    (diminish 'abbrev-mode)))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package ag
  :ensure t
  :bind ("C-M-s" . ag)
  :config (setq ag-highlight-search t))

(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

(use-package scratch
  :ensure t)

;; organiziation/presenation/sharing

(use-package org
  :ensure t
  :pre-load
  (unless (package-activate 'org '(20140519))
    (package-install 'org)))

(use-package org-reveal)

(use-package htmlize
  :ensure t)

(use-package gist
  :ensure t)

;; project

(use-package projectile
  :ensure t
  :idle (projectile-global-mode)
  :diminish projectile-mode
  :bind ("C-c p a" . projectile-ag))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)
         ("C-x G" . magit-blame-mode))
  :diminish magit-auto-revert-mode
  :config
  (progn (add-hook 'magit-log-edit-mode-hook '(lambda () (flyspell-mode t)))
         (add-hook 'magit-log-edit-mode-hook
                   '(lambda ()
                      (set (make-local-variable 'whitespace-style) '(face lines-tail))
                      (set (make-local-variable 'whitespace-line-column) 72)
                      (whitespace-mode t)))))

;; text editing
(use-package enclose
  :ensure t
  :idle (enclose-global-mode t)
  :diminish enclose-mode
  :config (remhash "'" enclose-table))

(use-package wrap-region
  :ensure t
  :idle (wrap-region-global-mode t)
  :diminish wrap-region-mode)

(use-package mark-multiple
  :ensure t
  :bind (("C-x r t" . inline-string-rectangle)
         ("C-<" . mark-previous-like-this)
         ("C->" . mark-next-like-this)
         ("C-*" . mark-all-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package browse-kill-ring
  :ensure t
  :bind ("C-c y" . browse-kill-ring))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (progn
    (add-hook 'lisp-mode '(lambda () (highlight-parentheses-mode t)))
    (add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-parentheses-mode t)))))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (progn
    (add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
    (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :idle (yas/global-mode t)
  :config
  (progn
    (setq yas-snippet-dirs (remove "~/.emacs.d/snippets" yas-snippet-dirs))
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/custom/snippets")
    (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))))

;; colors

(use-package monokai-theme
  :ensure t)

;; langs

(use-package markdown-mode
  :ensure t)

(use-package ruby-end
  :ensure t)

(use-package coffee-mode
  :ensure t
  :config (setq coffee-tab-width 2))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package scala-mode2
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode))
  :config
  (progn
    (add-hook 'scala-mode-hook '(lambda ()
                             (c-subword-mode t)))
    (setq scala-indent:align-parameters t)
    (setq scala-indent:align-forms t)))

(use-package sbt-mode
  :ensure t
  :config
  (add-hook 'sbt-mode-hook '(lambda ()
                              (setq compilation-skip-threshold 2)
                              (local-set-key (kbd "C-a") 'comint-bol)
                              (local-set-key (kbd "M-RET") 'comint-accumulate))))

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (setq haskell-program-name "ghci -isrc")))

(use-package ruby-mode
  :mode (("Vagrantfile" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Berksfile" . ruby-mode)))

(provide 'mine-pkgmgt)
