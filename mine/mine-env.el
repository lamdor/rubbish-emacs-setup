(defun mine-env-set ()
  (interactive)
  (let* ((cmd (format "%s -l -i -c env" (getenv "SHELL")))
         (env-big-str (shell-command-to-string cmd))
         (lines (split-string env-big-str "\n")))
    (dolist (line lines)
      (unless (= 0 (length line))
        (let* ((tokens (split-string line "="))
               (name (car tokens))
               (value (mapconcat 'identity (cdr tokens) "=")))
          (setenv name value)
          (when (string= name "PATH")
            (setq exec-path (split-string value ":"))))))))

(mine-env-set)

(setenv "EDITOR" "emacsclient")

(provide 'mine-env)
