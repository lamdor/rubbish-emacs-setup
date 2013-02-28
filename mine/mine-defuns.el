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

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

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

(defun mine-sql (product sql-user sql-password sql-server sql-database root-sql-script-dir)
  (let* ((sql-text-buffer (find-file (concat root-sql-script-dir sql-database "_" sql-server ".sql")))
         (new-name (concat sql-user "@" sql-database "." sql-server))
         (sqli-buffer (if sql-buffer (progn (split-window) sql-buffer) (sql-product-interactive product new-name))))
    (switch-to-buffer sql-text-buffer nil t)
    (set (make-local-variable 'sql-buffer) sqli-buffer)
    (switch-to-buffer sqli-buffer nil t)))

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

(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
  
  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files)) 
            (buffer-file-name (nth 2 marked-files)))))

(defun mine-command-line-tool (command &optional history where working-dir)
  (let* ((rest-of-command (read-string (concat command " ") nil history))
         (command-with-args (append (split-string command) (split-string rest-of-command)))
         (args (cdr command-with-args))
         (command (or where (car command-with-args)))
         (name (mapconcat 'identity command-with-args " "))
         (buffer-name (concat "*" name "*"))
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (if working-dir (cd working-dir))
    (apply 'make-comint-in-buffer name buffer command nil args)))

(require 'dbus)
(eval-after-load 'rcirc
  '(defun-rcirc-command np (whatever)
     "Now playing in spotify"
     (interactive "i")
     (let* ((metadata (dbus-get-property
                       :session
                       "org.mpris.MediaPlayer2.spotify"
                       "/org/mpris/MediaPlayer2"
                       "org.mpris.MediaPlayer2.Player"
                       "Metadata"))
            (artist (caaar (cdr (assoc "xesam:artist" metadata))))
            (title (caar (cdr (assoc "xesam:title" metadata))))
            (album (caar (cdr (assoc "xesam:album" metadata))))
            (uri (caar (cdr (assoc "xesam:url" metadata))))
            (uri-parts (split-string uri ":"))
            (url (concat "http://open.spotify.com/" (cadr uri-parts) "/" (caddr uri-parts))))
       (rcirc-send-message process target
                           (concat "Spotify: " artist " - " title " (" album ") -- " url)))))

(provide 'mine-defuns)
