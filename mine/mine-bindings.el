(global-set-key "\C-x\C-m" 'execute-extended-command)

;; buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; frame/window
(global-set-key "\C-x\C-j" 'speedbar)
(global-set-key (kbd "C-c C-a") 'autotest-switch)
(global-set-key (kbd "C-x p") 'other-previous-window)

;; file navigation
(global-set-key (kbd "C-x C-M-f") 'ido-find-file-in-tag-files)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key "\M-\." 'ido-find-tag)
(global-set-key "\C-x\C-i" 'ido-imenu)

;; Text Editting
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-a" 'beginning-of-line-or-back-to-indention)
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-k") 'kill-to-end-or-join)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c o") 'indent-buffer)

;; Use regex searches
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
 
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
 
;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

(global-set-key (kbd "C-M-g") 'magit-status)

;; annoying suspend
(global-unset-key "\C-z")

;; Stupid MacOSX handing of backquote
(defun insert-backquote ()
  (interactive)
  (insert "`"))
(global-set-key (kbd "C-`") 'insert-backquote)


(provide 'mine-bindings)

