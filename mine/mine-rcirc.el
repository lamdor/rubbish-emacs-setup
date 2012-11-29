(add-hook 'rcirc-mode-hook 'turn-on-flyspell)
(add-hook 'rcirc-mode-hook (lambda () (rcirc-track-minor-mode t)))
(add-hook 'rcirc-mode-hook (lambda () (rcirc-omit-mode)))

(setq rcirc-fill-flag nil
      rcirc-notify-timeout 2
      rcirc-notify-message "%s: %s"
      rcirc-buffer-maximum-lines 2000)

(provide 'mine-rcirc)
