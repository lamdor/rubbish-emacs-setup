;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(setq scala-interpreter "/usr/local/bin/scala")

(add-path "site-lisp/sbt")
(autoload 'sbt "sbt" "sbt shell" t)
(autoload 'sbt-switch "sbt" "sbt shell switch" t)

(add-path "site-lisp/ensime/dist/elisp")
(autoload 'ensime-scala-mode-hook "ensime" "ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode)." t)
(autoload 'ensime-config-gen "ensime" "Generate an ensime config." t)

(setq sbt-use-ui t)
(add-hook 'scala-mode-hook
	  '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)
             (c-subword-mode t)
             (local-set-key (kbd "C-c C-l") 'scala-load-file)
             (local-set-key (kbd "C-c C-r") 'scala-eval-region)
             (local-set-key (kbd "C-M-x") 'scala-eval-definition)

             (define-key scala-mode-map (kbd "<f1>") nil) ;; I don't like speedbar popping up

             (ensime-scala-mode-hook)
             (define-key ensime-mode-map (kbd "C-c C-a") 'ensime-sbt-switch)))

(add-hook 'scala-mode-hook 'turn-on-wrap-region-mode)
(add-hook 'scala-mode-hook 'turn-on-enclose-mode)



(provide 'mine-scala)

