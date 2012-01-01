(autoload 'paredit-mode "paredit" "Minor module for psuedo-structually editting lisp code." t)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (eldoc-mode t)))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (slime-mode t)))

(add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))

(provide 'mine-lisp)
