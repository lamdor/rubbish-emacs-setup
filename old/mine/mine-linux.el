(setq mine-normal-font "Monaco 13")
(setq mine-big-font "Monaco 20")

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq ack-executable "ack-grep")
(setq scala-interpreter "~/tools/scala-2.8.1.final/bin/scala")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)


(provide 'mine-linux)