(add-hook 'rcirc-mode-hook 'turn-on-flyspell)
(add-hook 'rcirc-mode-hook (lambda () (rcirc-track-minor-mode t)))
(add-hook 'rcirc-mode-hook (lambda () (rcirc-omit-mode)))

(defun mine-rcirc-shut-up ()
  (interactive)
  (rcirc-track-minor-mode -1)
  (remq 'rcirc-activity-string global-mode-string))

(setq rcirc-fill-flag nil
      rcirc-notify-timeout 2
      rcirc-notify-message "%s: %s"
      rcirc-buffer-maximum-lines 2000)

(provide 'mine-rcirc)
