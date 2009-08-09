(add-hook 'pomodoro-finished-hook '(lambda ()
                                     (growl-message "Pomodoro Finished" 2)))

(provide 'mine-pomodoro)
