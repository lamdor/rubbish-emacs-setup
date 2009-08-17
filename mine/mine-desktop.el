(setq desktop-path (list emacs-root))
(desktop-save-mode t)

(defun mine-save-desktop ()
  (interactive)
  (desktop-save (car desktop-path) nil))

(run-at-time t 300 'mine-save-desktop)

(provide 'mine-desktop)