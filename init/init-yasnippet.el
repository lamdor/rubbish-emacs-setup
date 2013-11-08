(yas/global-mode t)

(diminish 'yas-minor-mode)

(setq yas-snippet-dirs (remove "~/.emacs.d/snippets" yas-snippet-dirs))
(add-to-list 'yas-snippet-dirs "~/.emacs.d/custom/snippets")
(yas/reload-all)
