;; use uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(winner-mode t)

(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)

;; Auto revert files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;; Always use subwords to to move around
(global-subword-mode t)

(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-RET") 'dired-external-open)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Window split behavior (see mine-bindings for these to get set)
;; taken from https://www.youtube.com/watch?list=UUlT2UAbC6j7TqOWurVhkuHQ&v=nKCKuRuvAOw
(defun mine-split-window-vertically-last-buffer (prefix)
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

(defun mine-split-window-horizontally-last-buffer (prefix)
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

;; Tramp Optimizations
(setq tramp-default-method "ssh")

;; Indentation
(setq-default indent-tabs-mode nil)
(electric-indent-mode t)

;; Backups
(setq version-control nil)
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; search
(setq search-upper-case t)

;; Allow to be able to select text and start typing or delete
(delete-selection-mode t)

;; delete trailing whitespace on save
(defvar mine-delete-trailing-whitespace t)
(make-variable-buffer-local 'mine-delete-trailing-whitespace)

(defun mine-leave-whitespace-in-buffer ()
  (interactive)
  (setq mine-delete-trailing-whitespace nil))

(add-hook 'before-save-hook '(lambda () (if mine-delete-trailing-whitespace (delete-trailing-whitespace))))

(setq delete-trailing-lines t)

(toggle-indicate-empty-lines t)

(ansi-color-for-comint-mode-on)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode t)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode nil))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Misc Aliases
(defalias 'qrr 'query-replace-regexp)

;; Midnight mode to clean up old buffers
(require 'midnight)
(setq clean-buffer-list-delay-special 3600)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

(setq clean-buffer-list-kill-regexps '("^.*$"))

(add-hook 'emacs-lisp-mode-hook '(lambda () (eldoc-mode t)))

;; Miscallaneous Things
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode t))
(setq ring-bell-function (lambda () (message "*beep*")))

(setq vc-handled-backends nil)

(setq js-indent-level 2)

;; auto revert logs by tail
;; (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Save point position between sessions
(require 'saveplace)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq-default save-place t)

;; gpg
(setq epa-pinentry-mode 'loopback)

;; ssh sudo
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

;; server
(require 'server)
(unless (server-running-p) (server-start))

(setq browse-url-browser-function 'browse-url-default-browser)

(provide 'mine-builtin)
