;; Clojure

(autoload 'paredit-mode "paredit" "Minore module for psuedo-structually editting lisp code." t)

(add-path "site-lisp/clojure-mode")
(require 'clojure-auto)
(require 'clojure-paredit)

(add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))

(add-path "site-lisp/slime")
(require 'slime)

(add-path "site-lisp/swank-clojure")
(setq swank-clojure-jar-path "~/.m2/repository/jvm/clojure/clojure-lang/1.0-SNAPSHOT/clojure-lang-1.0-SNAPSHOT.jar")
(setq swank-clojure-extra-classpaths (list "~/code/watch/clojure-contrib/src/"))
(require 'swank-clojure-autoload)