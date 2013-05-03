(global-set-key [f8] 'pomodoro-start-or-void)

(add-hook 'pomodoro-start-hook 'mine-rcirc-shut-up)
(add-hook 'pomodoro-void-hook 'rcirc-track-minor-mode)
(add-hook 'pomodoro-finished-hook 'rcirc-track-minor-mode)

