(require 'timer)

(defvar pomodoro-current-timer)

(defun pomodoro-finished ()
  (let ((pomdoro-buffer (get-buffer-create "*Pomodoro*")))
    (switch-to-buffer pomdoro-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert "Pomodoro Finished")
    (toggle-read-only t)))

;; (timer-until pomodoro-current-timer (current-time))

(defun pomodoro-start ()
  "Start a pomodoro timer"
  (interactive)
  (setq pomodoro-current-timer
        (run-at-time "5 seconds" nil 'pomodoro-finished))
  (message "Pomodoro started"))

(defun pomodoro-void ()
  "Stops the current pomodoro timer"
  (interactive)
  (cancel-timer pomodoro-current-timer)
  (message "Pomodoro voided"))

(provide 'pomodoro)