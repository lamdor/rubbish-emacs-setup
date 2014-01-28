(require 'magit)

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

(defun mine-magit-create-topic-branch (name)
    (interactive (list (read-string "Topic branch: ")))
    (magit-create-branch name "master")
    (magit-merge "origin/master"))

(define-key magit-mode-map (kbd "T") 'mine-magit-create-topic-branch)

(defun mine-magit-git-sweep ()
  (interactive)
  (mine-command-line-tool "git-cleanup-and-prune"))

(define-key magit-mode-map (kbd "W") 'mine-magit-git-sweep)

(defun hub-pull-request ()
  (interactive)
  (async-shell-command "EDITOR=/usr/local/bin/emacsclient hub pull-request" "*hub pull-request*"))

(define-key magit-mode-map (kbd "H") 'hub-pull-request)
