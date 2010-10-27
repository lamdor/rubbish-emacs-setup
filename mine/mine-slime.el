(add-path "site-lisp/slime")
(require 'slime)

;; clojure slime setup
(add-path "site-lisp/slime/contrib")
(slime-setup '(slime-repl
               slime-c-p-c
               slime-fuzzy))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))

(add-path "site-lisp/swank-clojure")
(require 'swank-clojure)

(setq swank-clojure-classpath
      (list "/usr/local/Cellar/clojure/1.2.0/clojure.jar"
            (concat emacs-root "site-lisp/swank-clojure/swank-clojure-1.2.0.jar")))

;; slime implementations
(setq slime-lisp-implementations
      (list
       (list 'clojure (swank-clojure-cmd) :init 'swank-clojure-init)))

(provide 'mine-slime)
