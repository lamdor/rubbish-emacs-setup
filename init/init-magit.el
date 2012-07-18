(autoload 'magit-status "magit" nil t)
(setq magit-remote-ref-format 'remote-slash-branch)

(global-set-key (kbd "C-M-g") 'magit-status)

(add-hook 'magit-log-edit-mode-hook '(lambda () (flyspell-mode t)))

(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
             (set (make-local-variable 'whitespace-style) '(face lines-tail))
             (set (make-local-variable 'whitespace-line-column) 72)
             (whitespace-mode t)))
