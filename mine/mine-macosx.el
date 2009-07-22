;; Needed for JDEE
(setenv "JAVA_HOME" "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/")
;;(setenv "JAVA_HOME" "/Library/Java/Home")
(setenv "JAVACMD" "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java")

(setq ns-command-modifier (quote meta))
(set-frame-font "Monaco-14")

(set-frame-parameter (selected-frame) 'alpha '(90 80))
(add-to-list 'default-frame-alist '(alpha 90 80))

(defun toggle-transparency ()
   (interactive)
   (if (/=
        (cadr (find 'alpha (frame-parameters nil) :key #'car))
        100)
       (set-frame-parameter nil 'alpha '(100 100))
     (set-frame-parameter nil 'alpha '(90 80))))

(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(provide 'mine-macosx)
