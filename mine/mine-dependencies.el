;; FIXME(needs deps) (el-get 'sync 'gist)

;; FIXME(needs new github) (el-get 'sync 'yasnippet)

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(setq el-get-sources nil)

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
        scala-mode
        markdown-mode
        ace-jump-mode))

(el-get 'wait (append mine-packages
                      (mapcar 'el-get-source-name el-get-sources)))

(provide 'mine-dependencies)
