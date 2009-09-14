;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(setq scala-interpreter "/usr/local/bin/scala")

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (scala-electric-mode t)
             (c-subword-mode t)))

(provide 'mine-scala)

