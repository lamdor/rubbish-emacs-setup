;; move help
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-M-?") 'help-command)

;; buffers
(global-set-key [remap list-buffers] 'bs-show)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; frame/window
(global-set-key (kbd "C-x 7") 'swap-windows)
(global-set-key (kbd "C-x 9") 'toggle-window-split)
(global-set-key (kbd "C-M-9") 'winner-undo)
(global-set-key (kbd "C-M-0") 'winner-redo)

;; file navigation
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key [remap ido-find-file-read-only] 'ido-recentf-open)
(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;; Text Editting
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key [remap move-beginning-of-line] 'beginning-of-line-or-back-to-indention)
(global-set-key [remap newline] 'newline-and-indent)
(global-set-key [remap open-line] 'open-line-and-indent)
(global-set-key [remap kill-line] 'kill-to-end-or-join)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c o") 'indent-buffer)
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Use regex searches
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)
(global-set-key [remap isearch-forward-regexp] 'ack)
(global-set-key [remap isearch-backward-regexp] 'isearch-backward)
(global-set-key [remap query-replace] 'query-replace-regexp)

(global-set-key (kbd "C-x M-`") 'previous-error)

; annoying suspend
(global-unset-key (kbd "C-z"))

(provide 'mine-bindings)
