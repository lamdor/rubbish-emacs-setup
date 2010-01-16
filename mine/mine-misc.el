;; ido-mode
(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; ido setup
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point t
      ido-max-prospects 10
      ido-show-dot-for-dired t)

;; use uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Setup Environmental Variables
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)

;; Auto revert files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Always use subwords to to move aroudn
(c-subword-mode t)

;; Use soft tabs
(setq-default indent-tabs-mode nil)

;; Don't make backups
(setq make-backup-files nil)
(setq version-control nil)

;; Allow to be able to select text and start typing or delete
(delete-selection-mode t)

;; delete trailing whitespace on save
(setq mine-delete-trailing-whitespace t)
(add-hook 'before-save-hook '(lambda () (if mine-delete-trailing-whitespace (delete-trailing-whitespace))))

;; auto indentation of yanked/pasted text
(setq major-modes-to-auto-indent-yanked-text '(emacs-lisp-mode
                                               clojure-mode
                                               c-mode
                                               c++-mode
                                               objc-mode
                                               ruby-mode))

(defun yank-and-indent ()
  (interactive)
  (yank)
  (call-interactively 'indent-region))

;; Misc Aliases
(defalias 'qrr 'query-replace-regexp)

;; Midnight mode to clean up old buffers
(require 'midnight)

;; Miscallaneous Things
(mouse-wheel-mode t)
(setq visible-bell t)

(provide 'mine-misc)
