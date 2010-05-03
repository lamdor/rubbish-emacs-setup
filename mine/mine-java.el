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

  (require 'jde)

  (jde-set-variables
   '(jde-build-function (quote (jde-mvn-build)))
   '(jde-compile-option-command-line-args
     (quote ("-Xlint:all" "-Xlint:-serial"))))

  (setq jde-gen-buffer-boilerplate nil)

  (add-hook 'jde-mode-hook
	    (lambda ()
	      (jde-set-variables
	       '(jde-build-function (quote (jde-mvn-build)))
	       '(jde-compile-option-command-line-args
		 (quote ("-Xlint:all" "-Xlint:-serial"))))

;;; 	      (with-pom ()
;;; 		(jde-mvn-set-jde-variables :include-dependency-sources))

	      ;; Map C-c C-v C-t to run current test
	      (define-key jde-mode-map [remap jde-jdb-menu-debug-applet]
		'jde-mvn-build-run-test)))

	      ;; Map C-c C-v C-t to run current test
	      ;; (substitute-key-definition 'jde-jdb-menu-debug-applet 'jde-mvn-build-run-test jde-mode-map)))

  ;; (define-key jde-mode-map [remap 'jde-jdb-menu-debug-applet]
  ;;     'jde-mvn-build-run-test)

;;;   (with-pom ()
;;;     (jde-mvn-set-jde-variables :include-dependency-sources))

  )

(provide 'mine-java)
