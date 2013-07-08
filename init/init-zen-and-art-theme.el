(setq frame-background-mode 'dark)
(load-theme 'zen-and-art t)

(defun reset-zen-and-art-mode-line-colors ()
  (interactive)
  (set-face-attribute 'mode-line nil :background "grey40" :foreground "white"))
