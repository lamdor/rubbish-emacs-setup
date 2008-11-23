;; Key Bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x C-M-f") 'ido-find-file-in-tag-files)
(global-set-key "\C-x\C-j" 'speedbar)
(global-set-key "\M-\." 'ido-find-tag)
(global-set-key "\C-a" 'beginning-of-line-or-back-to-indention)
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-c C-a") 'autotest-switch)
(global-set-key (kbd "C-k") 'kill-to-end-or-join)
(global-set-key (kbd "C-M-g") 'magit-status)

(global-unset-key "\C-z")

(case system-type
  ('windows-nt (load "windows.el"))
  ('darwin (load "macosx.el")))


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
