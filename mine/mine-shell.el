(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start shell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'shell)

(provide 'mine-shell)
