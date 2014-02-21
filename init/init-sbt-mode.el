(require 'sbt-mode)

(defun mine-set-sbt-keys-if-possible ()
  (if (sbt:find-root)
      (progn
        (local-set-key (kbd "C-c s s") 'mine-sbt-switch)
        (local-set-key (kbd "C-c s c") 'mine-sbt-compile)
        (local-set-key (kbd "C-c s k") 'mine-sbt-console)
        (local-set-key (kbd "C-c s l") 'mine-sbt-last)
        (local-set-key (kbd "C-c s r") 'mine-sbt-run)
        (local-set-key (kbd "C-c s t") 'mine-sbt-test)
        (local-set-key (kbd "C-c s o") 'mine-sbt-test-only-current-test)
        (local-set-key (kbd "C-c s p") 'mine-sbt-publish))))

(defun mine-sbt-find-or-create-buffer ()
  (or (get-buffer (sbt:buffer-name))
      (sbt-start)))

(defun mine-sbt-switch ()
  (interactive)
  (let ((sbt-buffer (mine-sbt-find-or-create-buffer)))
    (if (eq (current-buffer) sbt-buffer)
        (if (one-window-p)
            (switch-to-buffer (other-buffer))
          (other-window 1))
      (if (get-buffer-window sbt-buffer)
          (other-window 1)
        (switch-to-buffer sbt-buffer)))))

(defun mine-sbt-compile (do-test-compile)
  (interactive "P")
  (if do-test-compile
      (sbt-command "test:compile")
    (sbt-command "compile")))

(defun mine-sbt-console ()
  (interactive)
  (sbt-command "console"))

(defun mine-sbt-last ()
  (interactive)
  (sbt-run-previous-command))

(defun mine-sbt-run ()
  (interactive)
  (sbt-command "re-start"))

(defun mine-sbt-test ()
  (interactive)
  (sbt-command "test"))

(defun mine-sbt-test-only-current-test ()
  (interactive)
  (sbt-command (concat "test-only " (mine-sbt-current-test-in-buffer))))

(defun mine-sbt-current-test-in-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let* ((pkg-name (progn
                       (re-search-forward "package ")
                       (filter-buffer-substring (point) (point-at-eol))))
           (test-name (progn
                        (re-search-forward "\\(object\\|class\\) ")
                        (filter-buffer-substring
                         (point)
                         (progn
                           (re-search-forward " ")
                           (forward-char -1)
                           (point))))))
      (concat pkg-name "." test-name))))

(defun mine-sbt-publish (do-publish)
  (interactive "P")
  (if do-publish
      (sbt-command "publish")
    (sbt-command "publish-local")))

(add-hook 'scala-mode-hook 'mine-set-sbt-keys-if-possible)
(add-hook 'dired-mode-hook 'mine-set-sbt-keys-if-possible)
(add-hook 'text-mode-hook 'mine-set-sbt-keys-if-possible)
(add-hook 'magit-mode-hook 'mine-set-sbt-keys-if-possible)
(add-hook 'conf-unix-mode-hook 'mine-set-sbt-keys-if-possible)
(add-hook 'sbt-mode-hook 'mine-set-sbt-keys-if-possible)

(add-hook 'sbt-mode-hook '(lambda ()
                            (setq compilation-skip-threshold 2)
                            (local-set-key (kbd "C-a") 'comint-bol)
                            (local-set-key (kbd "M-RET") 'comint-accumulate)))

