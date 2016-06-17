(setq eshell-path-env (getenv "PATH"))

(setq eshell-aliases-file (concat user-emacs-directory "custom/eshell/alias"))

(setq eshell-prefer-lisp-functions nil)

;; eshell buffer name

(defun mine-eshell-rename-buffer-pwd ()
  (let ((pwd (eshell/pwd)))
    (rename-buffer
     (concat "*"
             "eshell " "<" pwd ">"
             "*")
     t)))

(add-hook 'eshell-directory-change-hook 'mine-eshell-rename-buffer-pwd)

;; eshell switching

(defun mine-eshell-buffer-p (buffer)
  (and (eq 'eshell-mode (buffer-local-value 'major-mode buffer))
       buffer))

(defun mine-get-eshell-buffers ()
  (delq nil (mapcar 'mine-eshell-buffer-p (buffer-list))))

(defun mine-eshell-create ()
  (interactive)
  (eshell t)
  (mine-eshell-rename-buffer-pwd))

(defun mine-eshell-find-best-match (dir)
  (let ((pwd (expand-file-name dir))
        (best-match-buffer nil))
    (dolist (b (mine-get-eshell-buffers) best-match-buffer)
      (let* ((b-pwd       (with-current-buffer b (expand-file-name ".")))
             (b-match     (string-match b-pwd pwd))
             (b-match-end (and b-match (match-end 0)))
             (best-match-pwd (and best-match-buffer (with-current-buffer best-match-buffer (expand-file-name "."))))
             (best-match     (and best-match-buffer (string-match best-match-pwd pwd)))
             (best-match-end (and best-match-buffer (and best-match (match-end 0)))))
        (if (or (and (eq nil best-match-end)
                     b-match-end)
                (and b-match-end
                     (> b-match-end best-match-end)))
            (setq best-match-buffer b))))))

(defun mine-eshell-switch-to-closest-or-create (create-new)
  (if create-new
      (mine-eshell-create)
    (let ((best-match-buffer (mine-eshell-find-best-match default-directory)) )
      (if best-match-buffer
          (switch-to-buffer best-match-buffer)
        (mine-eshell-create)))))

(defun mine-fullscreen-eshell (&optional create-new)
  (interactive "P")
  (if create-new
      (mine-eshell-switch-to-closest-or-create create-new)
    (if (eq 'eshell-mode (buffer-local-value 'major-mode (current-buffer)))
        (jump-to-register :before-eshell-fullscreen)
      (progn
        (window-configuration-to-register :before-eshell-fullscreen)
        (mine-eshell-switch-to-closest-or-create nil)
        (delete-other-windows)))))

(defun mine-switch-to-last-eshell-buffer ()
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (switch-to-buffer (cadr (buffer-list)))
    (let ((last-used (car (mine-get-eshell-buffers))))
      (if last-used
          (switch-to-buffer last-used)
        (mine-fullscreen-eshell)))))

(global-set-key (kbd "C-c t") 'mine-fullscreen-eshell)
(global-set-key (kbd "C-M-t") 'mine-switch-to-last-eshell-buffer)

(add-hook 'eshell-mode-hook #'(lambda () (setq eshell-path-env (getenv "PATH"))))

;; Prompt

(setq eshell-highlight-prompt nil)

(setq eshell-prompt-function
      (function
       (lambda ()
         (propertize

          ;; the actual text of the prompt
          (concat (abbreviate-file-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " $ "))

          ;; the color of the prompt
          'face (if (or
                     (not eshell-last-command-name)
                     (not eshell-last-command-status)
                     (string-match "#<\\(Lisp object\\|function .*\\)>"
                                   eshell-last-command-name)
                     (= eshell-last-command-status 0))
                    '(:foreground "#A6E22E")
                  '(:foreground "Red"))))))

(custom-set-faces '(eshell-prompt ((t (:foreground "Purple" :bold t)))))

;; jumping with eshell-smart (plan 9 terminal)

(require 'em-smart)
(setq eshell-review-quick-commands t)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)

;; commands

(defalias 'ff 'find-file)
(defalias 'fff 'find-file-other-window)

(defun eshell/d (&optional dir)
  (interactive)
  (let ((dir (or dir default-directory)))
    (dired dir)))

(defun eshell/ansi (&rest program)
  (interactive)
  (ansi-term "/bin/zsh" (format "*ansi-term <%s>*" (expand-file-name default-directory))))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(setq eshell-visual-commands
      '("htop"
        "vi"
        "tmux"
        "screen"
        "top"
        "less"))

;; bindings

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-c C-o") 'browse-url-at-point)))


(provide 'mine-eshell)
