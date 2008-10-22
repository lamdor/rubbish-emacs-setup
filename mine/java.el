;; Settings for Java

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 1)
	    (c-toggle-syntactic-indentation 1)))


(autoload 'mvn-keys "mvn" "Sets Maven Keys")

(add-hook 'java-mode-hook (lambda () (mvn-keys)))

(defun load-jde ()
  (interactive)
  "Load up JDE and all of its heathen dependencies"
(load-file
   (concat emacs-root "site-lisp/cedet-1.0pre4/common/cedet.el"))
  (add-path "site-lisp/elib-1.0")
  (add-path "site-lisp/jde-2.3.5.1/lisp")
  (require 'jde))
