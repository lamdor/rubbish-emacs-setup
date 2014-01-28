(setq ns-command-modifier (quote meta))

(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(add-to-list 'after-make-frame-functions
             '(lambda (f)
                (if (display-graphic-p f) ;; is a graphical frame
                    (ns-raise-emacs))))

(defun ns-raise-chrome ()
  (ns-do-applescript "tell application \"Google Chrome\" to activate"))

(defadvice org-capture-finalize (after switch-back-to-chrome activate)
  "Advise org-capture-finalize to switch back to Chrome if a capture frame"
  (if (equal mine-capture-frame-name (frame-parameter nil 'name))
      (ns-raise-chrome)))

(provide 'mine-macosx)
