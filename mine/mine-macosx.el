(setq ns-command-modifier (quote meta))

(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(add-to-list 'after-make-frame-functions
             '(lambda (f)
                (if (display-graphic-p f) ;; is a graphical frame
                    (ns-raise-emacs))))

(provide 'mine-macosx)
