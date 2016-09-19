(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ))

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; use-package
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; to force package.el to read new pins from use-package blocks
;; https://github.com/jwiegley/use-package/issues/255
(advice-add 'use-package-pin-package
            :after #'(lambda (of &rest args) (package-read-all-archive-contents)))

(setq use-package-always-ensure t)

;; general

;; newest gh.el has caused problems with auth
(use-package gh
  :pin melpa-stable)

(use-package diminish
  :config
  (progn
    (eval-after-load 'eldoc '(diminish 'eldoc-mode))
    (eval-after-load 'flyspell '(diminish 'flyspell-mode))
    (diminish 'abbrev-mode)
    (diminish 'subword-mode)))

(use-package keyfreq
  :config
  (progn (setq keyfreq-excluded-commands
               '(self-insert-command
                 keyboard-quit
                 abort-recursive-edit
                 forward-char
                 backward-char
                 previous-line
                 isearch-printing-char
                 sp-backward-delete-char
                 org-self-insert-command
                 org-agenda-next-line
                 helm-next-line
                 orgtbl-self-insert-command
                 magit-section-forward
                 magit-section-backward
                 next-line))
         (keyfreq-mode t)
         (keyfreq-autosave-mode t)))

(use-package ag
  :bind ("C-M-s" . ag)
  :config (setq ag-highlight-search t))

(use-package switch-window
  :bind ("C-x o" . switch-window))

(use-package multi-term)

(use-package scratch)

(use-package edit-server
  :config (progn
            (add-to-list 'edit-server-url-major-mode-alist '("github\\.com" . gfm-mode))
            (add-to-list 'edit-server-url-major-mode-alist '("trello\\.com" . gfm-mode))
            (add-to-list 'edit-server-url-major-mode-alist '("reddit\\.com" . markdown-mode))
            (add-hook 'edit-server-edit-mode-hook
                      '(lambda () (set-frame-position (selected-frame) 360 200)))
            (add-hook 'edit-server-edit-mode-hook 'beginning-of-buffer)
            (add-hook 'edit-server-done-hook 'ns-raise-chrome)
            (edit-server-start)))

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :config (progn
            (projectile-global-mode)
            (setq projectile-switch-project-action 'projectile-commander)

            (def-projectile-commander-method ?c
              "Run compile in project."
              (call-interactively #'projectile-compile-project))))

;; helm

(use-package helm
  :pin melpa-stable
  :diminish helm-mode
  :bind  (("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-mini)
          ("C-x C-b" . helm-mini)
          ("C-x C-f" . helm-find-files)
          ("C-x C-i" . helm-semantic-or-imenu)
          ("M-x" . helm-M-x)
          ("C-c h" . helm-command-prefix)
          (:map minibuffer-local-map
                ("C-c C-l" . helm-minibuffer-history)))
  :config (progn
            (require 'helm-config)
            (setq helm-quick-update t
                  helm-split-window-in-side-p t
                  helm-buffers-fuzzy-matching t
                  helm-move-to-line-cycle-in-source t
                  helm-ff-search-library-in-sexp t
                  helm-ff-file-name-history-use-recentf t)

            (helm-mode t)

            (add-hook 'eshell-mode-hook
                      (lambda ()
                        (eshell-cmpl-initialize)
                        (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                        (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))

            (eval-after-load 'projectile '(setq projectile-completion-system 'helm))))


(use-package helm-descbinds
  :bind (("C-M-? b" . helm-descbinds)))

(use-package wgrep-helm)

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

(use-package helm-ag)

(use-package helm-company
  :after company
  :bind ((:map company-mode-map
               ("C-:" . helm-company))
         (:map comapny-active-map
               ("C-:" . helm-company))))

(use-package helm-projectile
  :after helm
  :config (progn
            (helm-projectile-on)
            (def-projectile-commander-method ?a
              "Find ag on project."
              (call-interactively 'helm-projectile-ag))))

(use-package helm-open-github)

;; organiziation/presenation/sharing
(use-package gmail-message-mode)

(use-package org)
(use-package org-tree-slide
  :after org
  :bind (:map org-mode-map
              ("<f8>" . org-tree-slide-mode)
              ("S-<f8>" . org-tree-slide-skip-done-toggle)))

(use-package htmlize)
(use-package ox-reveal)
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package gist)

(use-package magit
  :pin melpa-stable
  :bind (("C-x g" . magit-status)
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
         (global-magit-file-mode t)
         (setq magit-revert-buffers t)
         (setq magit-git-executable "git")))

(use-package magit-gh-pulls
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package github-browse-file
  :bind (("C-x M-G" . github-browse-file))
  :config (setq github-browse-file-show-line-at-point t))

;; text editing

(use-package smartparens
  :diminish smartparens-mode
  :config (progn
            (require 'smartparens-config)
            (sp-use-smartparens-bindings)
            (add-hook 'smartparens-enabled-hook 'smartparens-strict-mode)
            (add-hook 'eshell-mode-hook 'smartparens-mode)
            (add-hook 'minibuffer-setup-hook 'smartparens-mode)
            (smartparens-global-mode t)))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)))

(use-package expand-region
  :bind ("M-2" . er/expand-region))

(use-package move-text
  :bind (([M-up] . move-text-up)
         ([M-down] . move-text-down)))

(use-package ace-jump-mode
  :bind (("C-c C-SPC" . ace-jump-mode)))

(use-package ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package company
  :diminish company-mode
  :config (global-company-mode t))

(use-package yasnippet
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("C-<tab>" . yas-expand)
              ("C-TAB" . yas-expand))
  :config
  (progn
    (setq yas-snippet-dirs (remove "~/.emacs.d/snippets" yas-snippet-dirs))
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/custom/snippets")
    (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
    (yas-global-mode t)))

;; colors

(use-package monokai-theme)

;; tools

(use-package docker-tramp)
(use-package docker
  :diminish docker-mode
  :config (docker-global-mode t))

;; langs


(use-package flycheck
  :pin melpa-stable
  :config (progn
            (setq flycheck-standard-error-navigation nil)
            (global-flycheck-mode)))

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config (progn
            (setq markdown-reference-location 'end)

            (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
            (defun markdown-cleanup-org-tables ()
              (save-excursion
                (goto-char (point-min))
                (while (search-forward "-+-" nil t) (replace-match "-|-"))))
            (add-hook 'markdown-mode-hook
                      (lambda()
                        (add-hook 'edit-server-done-hook 'markdown-cleanup-org-tables nil 'make-it-local)
                        (add-hook 'after-save-hook 'markdown-cleanup-org-tables nil 'make-it-local)))))

(use-package coffee-mode
  :config (setq coffee-tab-width 2))

(use-package yaml-mode
  :config (add-hook 'yaml-mode-hook 'company-mode))

(use-package dockerfile-mode)

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode))
  :config
  (progn
    (add-hook 'scala-mode-hook 'company-mode)
    (add-hook 'scala-mode-hook 'c-subword-mode)
    (setq scala-indent:align-parameters t)
    (setq scala-indent:align-forms t)))

(use-package sbt-mode
  :config
  (add-hook 'sbt-mode-hook '(lambda ()
                              (setq compilation-skip-threshold 2)
                              (local-set-key (kbd "C-a") 'comint-bol)
                              (local-set-key (kbd "M-RET") 'comint-accumulate))))

(use-package ensime
  :pin melpa-stable
  :config
  (setq ensime-sbt-command "/usr/local/bin/sbt")
  (setq ensime-use-helm t)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  ;; TODO set ensime sbt prefix to C-c s
  ;; TODO add scala-mode hook for if .ensime file exists
  ;; (setq ensime-sbt-perform-on-save "compile)
  ;; (setq ensime-server-version "0.9.10-SNAPSHOT") ;; to something else if having prolems
  )

;; haskell bits taken mostly from:
;;   https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;;   https://github.com/chrisdone/emacs-haskell-config/tree/stack-mode
(use-package haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
    (add-hook 'haskell-mode-hook 'subword-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (setq
     haskell-complete-module-preferred '("Data.ByteString"
                                         "Data.ByteString.Lazy"
                                         "Data.Conduit"
                                         "Data.Function"
                                         "Data.List"
                                         "Data.Map"
                                         "Data.Maybe"
                                         "Data.Monoid"
                                         "Data.Ord")

     haskell-interactive-mode-eval-mode 'haskell-mode
     haskell-interactive-mode-include-file-name nil

     haskell-process-type 'stack-ghci
     haskell-process-suggest-remove-import-lines t
     haskell-process-auto-import-loaded-modules t
     haskell-process-log t
     haskell-process-reload-with-fbytecode nil
     haskell-process-use-presentation-mode t
     haskell-process-suggest-haskell-docs-imports t
     haskell-process-suggest-remove-import-lines t

     haskell-stylish-on-save nil
     haskell-tags-on-save nil
     )
    (eval-after-load 'haskell-mode
      '(progn
         (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
         (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
         (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
         (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
         (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
         (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
         (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
         (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
         (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))))

(use-package hindent
  :config
  (progn
    (setq hindent-style "gibiansky")
    (eval-after-load 'haskell-mode
      '(progn
         (define-key haskell-mode-map (kbd "C-c i") 'hindent-reformat-decl)
         (define-key haskell-mode-map (kbd "C-c o") 'hindent-reformat-buffer)))))

;; (use-package flycheck-haskell
;;   :ensure t
;;   :config
;;   (eval-after-load
;;       'flycheck '(add-hook 'flycheck-hode-hook #'flycheck-haskell-setup)))

(use-package ruby-mode
  :mode (("Vagrantfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Berksfile$" . ruby-mode))
  :config (progn
            (add-hook 'ruby-mode-hook 'company-mode)
            (setq flycheck-rubocop-lint-only nil)))

(use-package feature-mode
  :config (setq feature-cucumber-command
                "bundle exec cucumber {options} --tags ~@pending {feature}"))

(use-package ruby-guard
  :bind (("C-c C-g" . ruby-guard)))

(use-package inf-ruby)

(use-package rspec-mode
  :config (add-hook 'ruby-mode-hook 'rspec-mode))

(use-package go-mode
  :config (progn
            (add-hook 'go-mode-hook 'company-mode)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'go-mode-hook (lambda ()
                                      (local-set-key (kbd "M-.") #'godef-jump)))
            (let ((oracle-el-file (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el")))
              (if (file-exists-p oracle-el-file)
                  (load-file oracle-el-file)))))

(use-package elixir-mode
  :config (progn
            (add-hook 'elixir-mode-hook 'company-mode)
            (eval-after-load 'smartparens
              '(sp-with-modes '(elixir-mode)
                 (sp-local-pair "fn" "end"
                                :when '(("SPC" "RET"))
                                :actions '(insert navigate))
                 (sp-local-pair "do" "end"
                                :when '(("SPC" "RET"))
                                :post-handlers '(sp-ruby-def-post-handler)
                                :actions '(insert navigate))))))
;; (use-package alchemist)

(use-package protobuf-mode)

(use-package groovy-mode
  :mode ("Jenkinsfile\\'" . groovy-mode))


(use-package terraform-mode
  :mode ("\\.tfstate\\'" . js-mode)
  :config (progn
            (add-hook 'terraform-mode-hook 'company-mode)
            (setq terraform-indent-level 2)))

(provide 'mine-pkgmgt)
