;; I like highlighted parens
(show-paren-mode 1)

;; ido setup
(setq ido-enable-prefix nil
         ido-enable-flex-matching t
         ido-create-new-buffer 'always
         ido-use-filename-at-point t
         ido-max-prospects 10)

;; Setup Environmental Variables
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)

;; Auto revert files
(global-auto-revert-mode 1)

;; Always use subwords to to move aroudn
(c-subword-mode t)

;; Don't make backups
(setq make-backup-files nil)
(setq version-control nil)

;; Look Pretty
(global-hl-line-mode 1)
(transient-mark-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Misc Aliases
(defalias 'qrr 'query-replace-regexp)

;; Midnight mode to clean up old buffers
(require 'midnight)

;; Miscallaneous Things
(mouse-wheel-mode t)
(setq visible-bell t)

(provide 'mine-misc)