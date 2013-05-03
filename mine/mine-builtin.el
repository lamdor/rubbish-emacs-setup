;; ido-mode
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

;; ido setup
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-show-dot-for-dired nil)

;; use uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(winner-mode t)

;; Setup Environmental Variables
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)

;; Auto revert files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;; Buffer selection setup
(setq bs-configurations
      '(("all" nil nil nil nil nil)
        ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
        ("dired" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'dired-mode)))) nil)
        ("eshell" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'eshell-mode)))) nil)
        ("rcirc" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'rcirc-mode)))) nil)
        ("magit" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'magit-status-mode)))) nil)
        ("sbt" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (string-prefix-p "*sbt:" (buffer-name buf))))) nil)
        ("bub" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (string-prefix-p "*bub " (buffer-name buf))))) nil)
        ("sql" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (and
              (not (eq major-mode 'sql-mode))
              (not (eq major-mode 'sql-interactive-mode))))) nil)))

(setq bs-mode-font-lock-keywords
  (list
   ; Headers
   (list "^[ ]+\\([-M].*\\)$" 1 font-lock-keyword-face)
   ; Boring buffers
   (list "^\\(.*\\*.*\\*.*\\)$" 1 font-lock-comment-face)
   ; Dired buffers
   '("^[ .*%]+\\(Dired.*\\)$" 1 font-lock-type-face)
   ; Modified buffers
   '("^[ .]+\\(\\*\\)" 1 font-lock-warning-face)
   ; Read-only buffers
   '("^[ .*]+\\(\\%\\)" 1 font-lock-variable-name-face)))

;; Always use subwords to to move around
(if (fboundp 'subword-mode)
    (subword-mode t)
  (c-subword-mode t))

(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-RET") 'dired-external-open)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Tramp Optimizations
(setq tramp-default-method "ssh")

;; Use soft tabs
(setq-default indent-tabs-mode nil)

;; Backups
(setq version-control nil)
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; Allow to be able to select text and start typing or delete
(delete-selection-mode t)

;; delete trailing whitespace on save
(setq mine-delete-trailing-whitespace t)
(defun mine-leave-whitespace-in-buffer ()
  (interactive)
  (make-variable-buffer-local 'mine-leave-whitespace)
  (setq mine-delete-trailing-whitespace nil))
(add-hook 'before-save-hook '(lambda () (if mine-delete-trailing-whitespace (delete-trailing-whitespace))))


;; auto indentation of yanked/pasted text
(setq major-modes-to-auto-indent-yanked-text '(emacs-lisp-mode
                                               clojure-mode
                                               c-mode
                                               c++-mode
                                               objc-mode
                                               scala-code
                                               ruby-mode))

(defun yank-and-indent ()
  (interactive)
  (yank)
  (call-interactively 'indent-region))

;; Misc Aliases
(defalias 'qrr 'query-replace-regexp)

;; Midnight mode to clean up old buffers
(require 'midnight)

(add-hook 'emacs-lisp-mode-hook '(lambda () (eldoc-mode t)))

;; Miscallaneous Things
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode t))
(setq visible-bell t)

;; Protobuf files are like c
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . c-mode))

;; auto revert logs by tail
;; (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Save point position between sessions
(require 'saveplace)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq-default save-place t)

(provide 'mine-builtin)
