;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(autoload 'scala-mode-feature-install
  "scala-mode-feature" "Add features for scala-mode")
(autoload 'scala-mode-feature-electric-install
  "scala-mode-feature-electric" "Scala Electric Mode")

(setq scala-interpreter "/usr/local/scala/current/bin/scala")

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (scala-mode-feature-install)
	     (scala-mode-feature-electric-install)
	     (scala-electric-mode t)
	     (local-set-key (kbd "RET") 'newline-and-indent)
	     (local-set-key (kbd "C-c C-z") 'scala-run-scala)))

(provide 'mine-scala)

