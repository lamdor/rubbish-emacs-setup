(add-hook 'lisp-mode '(lambda () (highlight-parentheses-mode t)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-parentheses-mode t)))

(add-hook 'emacs-lisp-mode-hook '(lambda () (eldoc-mode t)))

(add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))

(provide 'mine-lisp)