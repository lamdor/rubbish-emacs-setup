(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start shell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'shell)

(add-hook 'shell-mode-hook
          '(lambda ()
             (setq dirtrack-list '("^\\([^ ]*\\) ?.*" 1 t))
             (shell-dirtrack-mode -1)
             (dirtrack-mode t)))

(provide 'mine-shell)
