;; Magit
(add-path "site-lisp/magit/")
(autoload 'magit-status "magit" "use magit git magic" t)

(setq vc-follow-symlinks t)

(setq magit-remote-ref-format 'remote-slash-branch)

;; (defun ask-for-jira-line (line)
;;   (interactive "sJIRA Line? ")
;;   line)

;; (add-hook 'magit-log-edit-mode-hook '(lambda ()
;;                                        (let ((jira-line (call-interactively 'ask-for-jira-line)))
;;                                          (unless (string= "" jira-line)
;;                                            (insert "

;; " jira-line)
;;                                            (goto-char (point-min))))))

(provide 'mine-vc)