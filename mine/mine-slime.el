(add-path "site-lisp/slime")
(require 'slime)

;; clojure slime setup
(add-path "site-lisp/slime/contrib")
(slime-setup '(slime-repl
               slime-c-p-c
               slime-fuzzy))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))

(add-path "site-lisp/swank-clojure")
(setq swank-clojure-jar-path "/usr/local/Cellar/clojure/1.0.0/clojure-1.0.0.jar") ; needed for swank-clojure to be happy
(require 'swank-clojure)

(provide 'mine-slime)
