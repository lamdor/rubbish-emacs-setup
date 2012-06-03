;; FIXME(needs deps) (el-get 'sync 'gist)

;; FIXME(needs new github) (el-get 'sync 'yasnippet)

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(setq mine-packages
      '(wrap-region
        enclose
        smex
        full-ack
        undo-tree
        switch-window
        scratch
        ;; color-theme ;; el-get recipe for color-theme doesn't work
        color-theme-zen-and-art
        highlight-parentheses
        paredit
        magit
        scala-mode))

(el-get nil mine-packages)

(provide 'mine-dependencies)
