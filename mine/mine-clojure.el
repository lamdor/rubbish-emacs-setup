;; Clojure
(add-path "site-lisp/clojure-mode")
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(autoload 'clojure-test-mode "clojure-test-mode" "A minor mode for running Clojure tests." t)
(autoload 'clojurescript-mode "clojurescript-mode" "A major mode for ClojureScript" t)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook (lambda () (clojure-test-mode t)))

(add-path "site-lisp/elein")
(autoload 'elein-swank "elein" "Launch lein swank and connect slime to it.  Interactively, aPREFIX means launch a standalone swank session without a project." t)
(autoload 'elein-run-cmd "elein" "Run 'lein ARGS' using `compile' in the project root directory." t)

;; clojure-test-mode
;;(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;;(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;;(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

(provide 'mine-clojure)
