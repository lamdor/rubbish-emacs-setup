;; Key Bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)
(global-unset-key "\C-z")

;; Setup Environmental Variables
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)
(setq custom-file (concat emacs-root "/mine/customizations.el"))

;; Put backup files in a specific dir
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

;; Look Pretty
(global-hl-line-mode 1)

;; Miscallaneous Things
(mouse-wheel-mode t)
(put 'downcase-region 'disabled nil)
