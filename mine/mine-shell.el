(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start shell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'shell)
(add-hook 'shell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-x m") 'switch-to-other-buffer)))

(setq dirtrack-list '("^\\([/~][^ ]*\\) \\((.+) \\)?\n-> " 1 t))

(add-hook 'shell-mode-hook
          '(lambda ()
             (dirtrack-mode 1)))

(setq comint-use-prompt-regexp t)
(setq shell-prompt-pattern "^-> ")

(provide 'mine-shell)
