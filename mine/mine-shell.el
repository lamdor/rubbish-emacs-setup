(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start shell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'shell)

(setq dirtrack-list '("^\\([^ ]*\\) .*\n->" 1 t))

(add-hook 'shell-mode-hook
          '(lambda ()
             (dirtrack-mode 1)))

(provide 'mine-shell)
