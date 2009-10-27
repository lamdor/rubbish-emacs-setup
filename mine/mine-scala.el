;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(setq scala-interpreter "/usr/local/bin/scala")

(add-path "site-lisp/sbt")
(autoload 'sbt "sbt" "sbt shell" t)
(autoload 'sbt-swith "sbt" "sbt shell switch" t)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-a") 'sbt-switch)
             (scala-electric-mode t)
             (local-set-key (kbd "RET") 'newline-and-indent)
             (c-subword-mode t)))

(provide 'mine-scala)

