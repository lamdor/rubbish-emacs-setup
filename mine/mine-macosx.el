;; used in emacs-mac forc
(setq mac-command-modifier (quote meta))
(setq mac-option-modifier (quote meta))

;; might be in older emacs versions or plain emacs
(setq ns-command-modifier (quote meta))
(setq ns-option-modifier (quote meta))

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(add-to-list 'after-make-frame-functions
             '(lambda (f)
                (if (display-graphic-p f) ;; is a graphical frame
                    (ns-raise-emacs))))

(defun ns-raise-chrome ()
  (interactive)
  (ns-do-applescript "tell application \"Google Chrome\" to activate"))

(provide 'mine-macosx)
