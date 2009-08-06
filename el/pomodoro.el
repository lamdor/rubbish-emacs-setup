(require 'timer)

(defvar pomodoro-current-timer nil)

(defvar pomodoro-duration-seconds (* 25 60))

(defun pomodoro-format-time-difference (seconds)
  (let ((absolute-seconds (abs seconds)))
    (format "%s%02d:%02d" (if (< 0 seconds) "+" "-")
            (/ absolute-seconds 60)
            (% absolute-seconds 60))))

(defvar pomodoro-display-string nil)
(defun pomodoro-display-update ()
  (setq pomodoro-display-string
        (if pomodoro-current-timer
            (format "Pomodoro: %s"
                    (pomodoro-format-time-difference
                     (timer-until pomodoro-current-timer (current-time))))
          ""))
  (sit-for 0))

(defvar pomodoro-display-timer nil)
(define-minor-mode pomodoro-display-mode
  :global t :group 'pomodoro
  (and pomodoro-display-timer (cancel-timer pomodoro-display-timer))
  (setq pomodoro-display-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (if pomodoro-display-mode
      (progn
        (or (memq 'pomodoro-display-string global-mode-string)
            (setq global-mode-string
                  (append global-mode-string '(pomodoro-display-string))))
        (setq pomodoro-display-timer
              (run-at-time t 1
                           'pomodoro-display-update)))
    (and pomodoro-display-timer
         (cancel-timer pomodoro-display-timer))))

(defun pomodoro-start-display ()
  (pomodoro-display-mode t))

(defun pomodoro-display-message (msg)
  (let ((pomodoro-buffer (get-buffer-create "*Pomodoro*")))
    (switch-to-buffer pomodoro-buffer)
    (delete-other-windows)
    (toggle-read-only -1)
    (erase-buffer)
    (insert msg)
    (toggle-read-only t)))

(defun pomodoro-finished ()
  (cancel-timer pomodoro-current-timer)
  (setq pomodoro-current-timer nil)
  (pomodoro-display-message "Pomodoro Finished"))

(defun pomodoro-start ()
  "Start a pomodoro timer"
  (interactive)
  (setq pomodoro-current-timer
        (run-at-time (time-add (current-time) (seconds-to-time pomodoro-duration-seconds))
                     nil
                     'pomodoro-finished))
  (pomodoro-start-display)
  (message "Pomodoro started"))

(defun pomodoro-void ()
  "Stops the current pomodoro timer"
  (interactive)
  (cancel-timer pomodoro-current-timer)
  (setq pomodoro-current-timer nil)
  (message "Pomodoro voided"))

(provide 'pomodoro)