;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(setq markdown-command "Markdown.pl")

(add-hook 'markdown-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-l") 'markdown-insert-link)))

(provide 'mine-markdown)