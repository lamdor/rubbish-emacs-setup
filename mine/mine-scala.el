;; scala
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(setq scala-interpreter "/usr/local/bin/scala")

(load-file (concat emacs-root "el/sbt.el"))

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (scala-electric-mode t)
             (local-set-key (kbd "RET") 'newline-and-indent)
             (c-subword-mode t)))

(provide 'mine-scala)

