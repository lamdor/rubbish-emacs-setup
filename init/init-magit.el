(autoload 'magit-status "magit" nil t)
(setq magit-remote-ref-format 'remote-slash-branch)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-blame-mode)

(add-hook 'magit-log-edit-mode-hook '(lambda () (flyspell-mode t)))

(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
             (set (make-local-variable 'whitespace-style) '(face lines-tail))
             (set (make-local-variable 'whitespace-line-column) 72)
             (whitespace-mode t)))

(setq magit-wazzup-only-branches t)

(defun mine-magit-hub-browse-commit ()
  (interactive)
  (let ((sha (magit-section-action (item info)
                  ((commit) info))))
   (start-process "hub-browse-commit" nil
                  "hub" "browse" "--"
                  (format "commit/%s" sha))))
