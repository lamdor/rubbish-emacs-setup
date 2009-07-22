;; Color theme
(add-path "site-lisp/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(load-file (concat emacs-root "el/color-theme-twilight.el"))
(color-theme-twilight)

(global-hl-line-mode t)

(require 'highlight-parentheses)
(highlight-parentheses-mode t)

(provide 'mine-pretty)
