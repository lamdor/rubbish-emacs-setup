;; Settings for Java

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-toggle-electric-state 1)
	    (c-toggle-auto-newline 1)
	    (c-toggle-syntactic-indentation 1)))


(autoload 'mvn-keys "mvn" "Sets Maven Keys")

(add-hook 'java-mode-hook (lambda () (mvn-keys)))