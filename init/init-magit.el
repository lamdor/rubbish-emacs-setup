(autoload 'magit-status "magit" nil t)
(setq magit-remote-ref-format 'remote-slash-branch)

;; only necessary in magit > 1.2
;; (add-hook 'magit-status-mode-hook
;;           '(lambda ()
;;              (set-face-attribute 'magit-item-highlight nil
;;                       :background "black")))

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
