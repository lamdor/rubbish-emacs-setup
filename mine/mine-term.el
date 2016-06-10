;; term buffer name

(defun mine-term-rename-buffer-pwd (&optional dir)
  (if (not (eq 'term-mode (buffer-local-value 'major-mode (current-buffer))))
      (mine-switch-to-last-term-buffer))
  (rename-buffer
   (concat "*"
           "terminal " "<" (or dir default-directory) ">"
           "*")
   t))

(add-hook 'term-exec-hook 'mine-term-rename-buffer-pwd)

;; term switching

(defun mine-term-buffer-p (buffer)
  (and (eq 'term-mode (buffer-local-value 'major-mode buffer))
       buffer))

(defun mine-get-term-buffers ()
  (delq nil (mapcar 'mine-term-buffer-p (buffer-list))))

(defun mine-term-create ()
  (interactive)
  (multi-term)
  (mine-term-rename-buffer-pwd))

(defun mine-term-find-best-match (dir)
  (let ((pwd (expand-file-name dir))
        (best-match-buffer nil))
    (dolist (b (mine-get-term-buffers) best-match-buffer)
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

(defun mine-term-switch-to-closest-or-create (create-new)
  (if create-new
      (mine-term-create)
    (let ((best-match-buffer (mine-term-find-best-match default-directory)) )
      (if best-match-buffer
          (switch-to-buffer best-match-buffer)
        (mine-term-create)))))

(defun mine-fullscreen-term (&optional create-new)
  (interactive "P")
  (if create-new
      (mine-term-switch-to-closest-or-create create-new)
    (if (eq 'term-mode (buffer-local-value 'major-mode (current-buffer)))
        (jump-to-register :before-term-fullscreen)
      (progn
        (window-configuration-to-register :before-term-fullscreen)
        (mine-term-switch-to-closest-or-create nil)
        (delete-other-windows)))))

(defun mine-last-buffer-not-minibuffer ()
  (let (last-used (cadr (buffer-list)))
    (if (eq 'minibuffer-inactive-mode (buffer-local-value 'major-mode last-used))
        (caddr (buffer-list))
      last-used)))

(defun mine-switch-to-last-term-buffer-or-back ()
  (interactive)
  (if (eq major-mode 'term-mode)
      (switch-to-buffer (mine-last-buffer-not-minibuffer))
    (let ((last-used (car (mine-get-term-buffers))))
      (if last-used
          (switch-to-buffer last-used)
        (mine-fullscreen-term)))))

(global-set-key (kbd "C-c t") 'mine-fullscreen-term)
(global-set-key (kbd "C-M-t") 'mine-switch-to-last-term-buffer-or-back)

(provide 'mine-term)
