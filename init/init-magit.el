(autoload 'magit-status "magit" nil t)
(setq magit-remote-ref-format 'remote-slash-branch)

(global-set-key (kbd "C-M-g") 'magit-status)
