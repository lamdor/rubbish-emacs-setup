(global-set-key (kbd "<f1>") 'ns-toggle-fullscreen)

;; buffers
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; frame/window
(global-set-key (kbd "C-x 7") 'swap-windows)
(global-set-key (kbd "C-x 9") 'toggle-window-split)

;; file navigation
(global-set-key (kbd "C-x C-M-f") 'ack-find-file)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "M-.") 'ido-find-tag)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;; Text Editting
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-a") 'beginning-of-line-or-back-to-indention)
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "C-k") 'kill-to-end-or-join)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c o") 'indent-buffer)

;; Use regex searches
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; smex keybindings
(global-set-key (kbd "M-x") 'smex)
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

