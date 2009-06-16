;; Needed for JDEE
(setenv "JAVA_HOME" "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/")
;;(setenv "JAVA_HOME" "/Library/Java/Home")
(setenv "JAVACMD" "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java")

(setq ns-command-modifier (quote meta))
(if (functionp 'ns-set-background-alpha)
    (ns-set-background-alpha 0.9))

(provide 'mine-macosx)
