;; Key Bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-j" 'speedbar)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-unset-key "\C-z")

;; Setup Environmental Variables
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)
(setq custom-file (concat emacs-root "/mine/customizations.el"))

;; Auto revert files
(global-auto-revert-mode 1)

;; Don't make backups
(setq make-backup-files nil)
(setq version-control nil)

;; Look Pretty
(global-hl-line-mode 1)

;; Miscallaneous Things
(mouse-wheel-mode t)
(setq visible-bell t)
;; (setq-default show-trailing-whitespace t)
(put 'downcase-region 'disabled nil)
