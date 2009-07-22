;; Needed for JDEE
(setenv "JAVA_HOME" "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/")
;;(setenv "JAVA_HOME" "/Library/Java/Home")
(setenv "JAVACMD" "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java")

(setq ns-command-modifier (quote meta))
(set-frame-font "Monaco-14")
(if (functionp 'ns-set-background-alpha)
    (ns-set-background-alpha 0.9))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(provide 'mine-macosx)
