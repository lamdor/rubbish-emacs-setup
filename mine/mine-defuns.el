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

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun clear-unit-test-from-mode-line ()
  (interactive)
  (mapcar (lambda (buffer)
            (with-current-buffer buffer
              (show-test-none)))
          (remove-if 'minibufferp (buffer-list))))

(defun move-to-pending ()
  "Moves marked files in dired buffer to pending and creates pending links for them in the inbox.org file"
  (interactive)
  (dolist (file-to-move (mapcar (function car) (dired-map-over-marks
                                                (cons (dired-get-filename) (point)) nil)))
    (let ((file-name (file-name-nondirectory file-to-move))
          (org-capture-link-is-already-stored t))
      (message "Moving file %s to pending" file-name)
      (org-store-link-props :annotation (org-make-link-string (concat "pending:" file-name) file-name))
      (org-capture nil "l")
      (rename-file file-to-move (concat "~/Desktop/Pending/" file-name) t)))
  (revert-buffer))

(defun move-marked-dired-files (destination-dir)
  (dolist (file-to-move (mapcar (function car) (dired-map-over-marks
                                                (cons (dired-get-filename) (point)) nil)))
    (let ((file-name (file-name-nondirectory file-to-move)))
      (message "Moving file %s to %s" file-name destination-dir)
      (rename-file file-to-move (concat destination-dir file-name) t)))
  (revert-buffer))

(defun move-to-documents ()
  (interactive)
  (move-marked-dired-files "~/Documents/"))

(defun move-to-private ()
  (interactive)
  (move-marked-dired-files "~/Private/"))

(defun dired-reveal (file)
  "Reveals the file inside of a dired buffer"
  (let* ((full-file (expand-file-name file))
         (dir (file-name-directory full-file)))
    (dired dir)
    (dired-goto-file full-file)))

(defun dired-mac-open ()
  "Invoke xdg-open on the file at point"
  (interactive)
  (call-process "open" nil 0 nil (expand-file-name (dired-file-name-at-point))))

(defun dired-xdg-open ()
  "Invoke xdg-open on the file at point"
  (interactive)
  (call-process "xdg-open" nil 0 nil (expand-file-name (dired-file-name-at-point))))

(defun dired-external-open ()
  "Opens the file at point in an external viewer"
  (interactive)
  (case system-type
    ('darwin (dired-mac-open))
    ('gnu/linux (dired-xdg-open))))

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

(defun toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun mine-sql (method user password host database root-sql-script-dir)
  (let ((sql-user user)
        (sql-password password)
        (sql-server host)
        (sql-database database)
        (sql-buffer-name (concat "*SQL*:" database ":" host)))
    (when (not (get-buffer sql-buffer-name))
      (call-interactively method)
      (rename-buffer sql-buffer-name))
    (delete-other-windows)
    (switch-to-buffer sql-buffer-name)
    (split-window-vertically)
    (find-file (concat root-sql-script-dir database "_" host ".sql"))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun ido-recentf-open ()
  "Use `ido-completing-read` to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun comint-clear-buffer-or-region ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(provide 'mine-defuns)
