(add-path "site-lisp/slime")
(require 'slime)

;; clojure slime setup
(add-path "site-lisp/slime/contrib")
(slime-setup '(slime-repl
               slime-c-p-c
               slime-fuzzy))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))

(provide 'mine-slime)
