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

(defmacro mine-swank-clojure-implementation (name &optional extra-classpath java-path clojure-jar extra-vm-args)
  `(let ((swank-clojure-jar-path ,(or clojure-jar "/usr/local/Cellar/clojure/1.0.0/clojure-1.0.0.jar"))
         (swank-clojure-extra-classpaths (cons (concat emacs-root "mine/clojure/") ,(or extra-classpath `(list "~/code/watch/clojure-contrib/src/"))))
         (swank-clojure-java-path ,(or java-path "java"))
         (swank-clojure-extra-vm-args ,extra-vm-args))
     (list ',name (swank-clojure-cmd) :init 'swank-clojure-init)))

;; slime implementations
(setq slime-lisp-implementations
      (list
       (mine-swank-clojure-implementation clojure-with-contrib)
       (mine-swank-clojure-implementation clojure-for-pragprog-programming-clojure
                                          (list "~/code/watch/clojure-contrib/src/"
                                                "~/code/learning/clojure/programming-clojure/code"
                                                "~/code/learning/clojure/shcloj-code/code"
                                                "~/.m2/repository/ant/ant/1.6.5/ant-1.6.5.jar"))
       (mine-swank-clojure-implementation clojure-with-apple-java-1.6
                                          (list "~/code/watch/clojure-contrib/src/")
                                          "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java")
       (mine-swank-clojure-implementation clojure-for-processing
                                          (list "~/code/watch/clojure-contrib/src/"
                                                "/Users/luke/code/watch/clj-processing/src/"
                                                "/Applications/Processing.app/Contents/Resources/Java/core.jar"
                                                "/Applications/Processing.app/Contents/Resources/Java/libraries/opengl/library/gluegen-rt.jar"
                                                "/Applications/Processing.app/Contents/Resources/Java/libraries/opengl/library/jogl.jar"
                                                "/Applications/Processing.app/Contents/Resources/Java/libraries/opengl/library/opengl.jar")
                                          "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java"
                                          nil
                                          (list "-Djava.library.path=/Applications/Processing.app/Contents/Resources/Java/libraries/opengl/library"))
       (mine-swank-clojure-implementation clojure-basic '())
       (mine-swank-clojure-implementation clojure-latest-basic '() nil "/Users/luke/code/watch/clojure/clojure.jar")
       (mine-swank-clojure-implementation clojure-latest-with-contrib
                                          (list "~/code/watch/clojure-contrib/src/")
                                          nil
                                          "/Users/luke/code/watch/clojure/clojure.jar")))

(provide 'mine-slime)
