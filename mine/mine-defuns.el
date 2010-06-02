(defun beginning-of-line-or-back-to-indention ()
  (interactive)
  "This goes to back to indention or if already there beginning of line"
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
	(beginning-of-line))))

(defun kill-to-end-or-join ()
  (interactive)
  "This will either kill to the end of the line or if already there join it with the next line"
  (if (equal (point) (point-at-eol))
      (save-excursion
	(next-line)
	(delete-indentation)
	)
    (kill-line)))

(defun open-line-and-indent ()
  (interactive)
  "Opens a line and and indents"
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))

(defun other-previous-window ()
  (interactive)
  (other-window -1))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun clear-unit-test-from-mode-line ()
  (interactive)
  (mapcar (lambda (buffer)
            (with-current-buffer buffer
              (show-test-none)))
          (remove-if 'minibufferp (buffer-list))))

(defun growl-message (msg &optional priority)
  "Sends a message to grow"
  (interactive "sMessage: ")
  (call-process "growlnotify" nil nil nil
                "-m" msg
                "-p" (format "%s" (if priority priority 0))))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun kill-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(defun tail ()
  "tail the file loaded within the current buffer"
  (interactive)
  (auto-revert-tail-mode))

(defun mine-mysql (user password host database)
  (let ((sql-user user)
        (sql-password password)
        (sql-server host)
        (sql-database database)
        (sql-buffer-name (concat "*SQL*:" database ":" host)))
    (when (not (get-buffer sql-buffer-name))
      (call-interactively 'sql-mysql)
      (rename-buffer sql-buffer-name))
    (delete-other-windows)
    (switch-to-buffer sql-buffer-name)
    (split-window-vertically)
    (find-file (concat (getenv "HOME") "/Documents/sql-scripts/" database "_" host ".sql"))))

(provide 'mine-defuns)
