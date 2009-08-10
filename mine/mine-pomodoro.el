(require 'pomodoro)

(add-hook 'pomodoro-finished-hook '(lambda ()
                                     (growl-message "Pomodoro Finished" 2)))

(global-set-key (kbd "C-c p s") 'pomodoro-start)
(global-set-key (kbd "C-c p v") 'pomodoro-void)

(provide 'mine-pomodoro)
