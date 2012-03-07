;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(setq markdown-command "Markdown.pl")

(defadvice markdown-cycle (around yas/try-expand-first activate)
  "Try to expand a snippet before point, then call markdown-cycle as usual"
  (let ((yas/fallback-behavior nil))
    (unless (and (interactive-p)
                 (yas/expand))
      ad-do-it)))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-l") 'markdown-insert-link)))

(provide 'mine-markdown)