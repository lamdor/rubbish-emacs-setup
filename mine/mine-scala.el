;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(setq scala-interpreter "/usr/local/bin/scala")

(add-path "site-lisp/sbt")
(autoload 'sbt "sbt" "sbt shell" t)
(autoload 'sbt-switch "sbt" "sbt shell switch" t)

(autoload 'mvn-keys "mvn" "Sets Maven Keys" t)
(setq sbt-use-ui t)

(defun scala-run-scala-sbt ()
  (interactive)
  (let ((sbt-path (sbt-find-path-to-project)))
    (if (equal "/" sbt-path)
        (scala-run-scala "scala")
      (progn
        (cd sbt-path)
        (scala-run-scala "sbt console")))))

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-a") 'sbt-switch)
             (mvn-keys)

             (scala-electric-mode t)
             (local-set-key (kbd "RET") 'newline-and-indent)
             (c-subword-mode t)

             (local-set-key (kbd "C-c C-l") 'scala-load-file)
             (local-set-key (kbd "C-c C-r") 'scala-eval-region)
             (local-set-key (kbd "C-M-x") 'scala-eval-definition)
             (local-set-key (kbd "C-c C-s") 'scala-run-scala-sbt)
             (local-set-key (kbd "C-c C-z") 'scala-switch-to-interpreter)

             (define-key scala-mode-map (kbd "<f1>") nil) ;; I don't like speedbar popping up
             ))

(add-hook 'scala-mode-hook 'turn-on-wrap-region-mode)

(provide 'mine-scala)

