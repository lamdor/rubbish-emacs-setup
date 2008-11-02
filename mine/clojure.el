;; Clojure

(autoload 'paredit-mode "paredit" "Minore module for psuedo-structually editting lisp code." t)

(add-path "site-lisp/clojure-mode")
(require 'clojure-auto)
(require 'clojure-paredit)

(add-hook clojure-mode-hook (lambda () (paredit-mode t)))
