;; buffers
(global-set-key [remap list-buffers] 'bs-show)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; frame/window
(global-set-key (kbd "C-x 7") 'swap-windows)
(global-set-key (kbd "C-x 9") 'toggle-window-split)

;; file navigation
(global-set-key (kbd "C-x C-M-f") 'ack-find-file)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key [remap ido-find-file-read-only] 'ido-recentf-open)
(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;; Text Editting
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key [remap move-to-beginning-of-line] 'beginning-of-line-or-back-to-indention)
(global-set-key [remap newline] 'newline-and-indent)
(global-set-key [remap open-line] 'open-line-and-indent)
(global-set-key [remap kill-line] 'kill-to-end-or-join)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c o") 'indent-buffer)

;; Use regex searches
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)
(global-set-key [remap isearch-forward-regexp] 'isearch-forward)
(global-set-key [remap isearch-backward-regexp] 'isearch-backward)
(global-set-key [remap query-replace] 'query-replace-regexp)

;; smex keybindings
(global-set-key [remap execute-extended-command] 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-m") 'smex)

(global-set-key (kbd "C-x M-`") 'previous-error)

(global-set-key (kbd "C-M-g") 'magit-status)

(global-set-key (kbd "C-c t") 'mine-multi-shell-next)
(global-set-key (kbd "C-c T") 'multi-shell-new)
(global-set-key (kbd "C-c M-t") 'mine-multi-shell-switch-dir)

;; annoying suspend
(global-unset-key (kbd "C-z"))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(provide 'mine-bindings)

