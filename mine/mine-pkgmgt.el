;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(package-refresh-contents)

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
             (if (not (package-activate 'org '(20140519)))
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
             :config (diminish 'projectile-mode)
             :bind ("C-c p a" . projectile-ag))

(use-package magit
             :ensure t
             :bind (("C-x g" . magit-status)
                    ("C-x G" . magit-blame-mode))
             :config
             (progn (add-hook 'magit-log-edit-mode-hook '(lambda () (flyspell-mode t)))
                    (add-hook 'magit-log-edit-mode-hook
                              '(lambda ()
                                 (set (make-local-variable 'whitespace-style) '(face lines-tail))
                                 (set (make-local-variable 'whitespace-line-column) 72)
                                 (whitespace-mode t)))))

;; text editing

(use-package mark-multiple)
(use-package expand-region)
(use-package browse-kill-ring)
(use-package enclose)
(use-package wrap-region)
(use-package highlight-parentheses)
(use-package paredit)
(use-package yasnippet)

;; colors

(use-package monokai-theme)

;; langs

(use-package markdown-mode)
(use-package ruby-end)
(use-package coffee-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package scala-mode2)
(use-package sbt-mode)
(use-package pomodoro)
(use-package haskell-mode)
(use-package ruby-mode
             :mode (("Vagrantfile" . ruby-mode)
                    ("Rakefile" . ruby-mode)
                    ("Gemfile" . ruby-mode)
                    ("Berksfile" . ruby-mode)))

(provide 'mine-pkgmgt)
