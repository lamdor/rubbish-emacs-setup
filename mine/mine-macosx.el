(setq ns-command-modifier (quote meta))

(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(add-to-list 'after-make-frame-functions
             '(lambda (f)
                (if (display-graphic-p f) ;; is a graphical frame
                    (ns-raise-emacs))))

(defun mine-hide-emacs-frame (&optional frame)
  (interactive)
  (ns-do-hide-emacs))

(add-to-list 'delete-frame-functions 'mine-hide-emacs-frame)

(provide 'mine-macosx)
