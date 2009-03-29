;; I like highlighted parens
(show-paren-mode 1)

;; Look Pretty
(global-hl-line-mode -1)
(transient-mark-mode t)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode -1)

(provide 'mine-pretty-lite)