;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; use-package
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; general

(use-package diminish
  :ensure t
  :config
  (progn
    (eval-after-load 'eldoc '(diminish 'eldoc-mode))
    (eval-after-load 'flyspell '(diminish 'flyspell-mode))
    (diminish 'abbrev-mode)
    (diminish 'subword-mode)))

(use-package ag
  :ensure t
  :bind ("C-M-s" . ag)
  :config (setq ag-highlight-search t))

(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind  (("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-mini)
          ("C-x C-b" . helm-mini)
          ("C-x C-f" . helm-find-files)
          ("C-x C-i" . helm-semantic-or-imenu)
          ("M-x" . helm-M-x)
          ("C-c h" . helm-command-prefix))
  :config (progn
            (require 'helm-config)
            (setq helm-quick-update t
                  helm-split-window-in-side-p t
                  helm-buffers-fuzzy-matching t
                  helm-move-to-line-cycle-in-source t
                  helm-ff-search-library-in-sexp t
                  helm-ff-file-name-history-use-recentf t)
            (define-key helm-map (kbd "C-z")  'helm-select-action)
            (add-hook 'eshell-mode-hook
                      #'(lambda ()
                          (define-key eshell-mode-map (kbd "TAB") 'helm-esh-pcomplete)
                          (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))
            (helm-mode t)))


(use-package helm-descbinds
  :ensure t
  :bind (("C-M-? b" . helm-descbinds)))

(use-package wgrep-helm
  :ensure t)

(use-package helm-swoop
  :ensure t)

(use-package scratch
  :ensure t)

(use-package edit-server
  :ensure t
  :config (progn
            (add-to-list 'edit-server-url-major-mode-alist '("github\\.com" . gfm-mode))
            (add-to-list 'edit-server-url-major-mode-alist '("trello\\.com" . gfm-mode))
            (add-to-list 'edit-server-url-major-mode-alist '("reddit\\.com" . markdown-mode))
            (add-hook 'edit-server-edit-mode-hook
                      '(lambda () (set-frame-position (selected-frame) 360 200)))
            (add-hook 'edit-server-edit-mode-hook 'beginning-of-buffer)
            (add-hook 'edit-server-done-hook 'ns-raise-chrome)
            (edit-server-start)))

(use-package gmail-message-mode
  :ensure t)

;; organiziation/presenation/sharing

(use-package org
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package gist
  :ensure t)

;; project

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (progn
            (define-key projectile-command-map (kbd "a") 'projectile-ag)
            (setq projectile-completion-system 'helm)
            (projectile-global-mode)))

(use-package helm-projectile
  :ensure t
  :config (progn
            (setq projectile-switch-project-action 'helm-projectile)
            (helm-projectile-on)
            (define-key projectile-command-map (kbd "a") 'projectile-ag)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-x C-g" . magit-status)
         ("C-x G" . magit-blame))
  :config
  (progn (add-hook 'magit-log-edit-mode-hook '(lambda () (flyspell-mode t)))
         (autoload 'magit-blame-mode "magit-blame" nil t nil)
         (add-hook 'git-commit-mode-hook
                   '(lambda ()
                      (set (make-local-variable 'whitespace-style) '(face lines-tail))
                      (set (make-local-variable 'whitespace-line-column) 72)
                      (whitespace-mode t)))
         (add-to-list 'clean-buffer-list-kill-never-regexps "^\\*magit:")
         (setq magit-revert-buffers t)
         (setq magit-branch-arguments nil) ;do NOT want --track
         (setq magit-push-arguments '("--set-upstream"))))

(use-package magit-gh-pulls
  :ensure t
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;; text editing

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (progn
            (require 'smartparens-config)
            (add-hook 'smartparens-enabled-hook '(lambda () (smartparens-strict-mode t)))
            (sp-use-smartparens-bindings)
            (define-key sp-keymap (kbd "M-<backspace>") nil)
            (add-hook 'eshell-mode-hook 'smartparens-mode)
            (smartparens-global-mode t)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)))

(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c C-SPC" . ace-jump-mode)))

(use-package ace-jump-zap
  :ensure ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas-snippet-dirs (remove "~/.emacs.d/snippets" yas-snippet-dirs))
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/custom/snippets")
    (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
    (yas-global-mode t)))

;; colors

(use-package monokai-theme
  :ensure t)

;; tools

(use-package restclient
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

;; langs

(use-package flycheck
  :ensure t
  :config (progn
            (diminish 'flycheck-mode " Φ")
            (setq flycheck-standard-error-navigation nil)
            (global-flycheck-mode)))

(use-package markdown-mode
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
            (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
            (setq markdown-reference-location 'end)))

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
    (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

    (setq haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-suggest-hoogle-imports nil ;; 'cabal install hoogle' fails
          haskell-process-log t
          haskell-process-type 'cabal-repl
          haskell-interactive-mode-eval-mode 'haskell-mode
          haskell-process-show-debug-tips nil)

    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-bring)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
    (eval-after-load 'haskell-cabal '(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))))

(use-package flycheck-haskell
  :ensure t
  :config
  (eval-after-load
      'flycheck '(add-hook 'flycheck-hode-hook #'flycheck-haskell-setup)))

(use-package ruby-mode
  :mode (("Vagrantfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Berksfile$" . ruby-mode)))

(use-package go-mode
  :ensure t
  :config (progn
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'go-mode-hook (lambda ()
                                      (local-set-key (kbd "M-.") #'godef-jump)))))

(use-package terraform-mode
  :ensure t
  :config (setq terraform-indent-level 2))

;; misc

(use-package ox-reveal
  :ensure t)

(provide 'mine-pkgmgt)
         
(use-package helm-ag
  :ensure t)

