;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-rebind-eol-bol-keys nil)

;; (add-hook 'js-mode '(lambda ())
;;           (local-set-key (kbd "RET" js-insert-and-indent)))

(setq js-indent-level 2)
(add-hook 'js-mode-hook 'turn-on-wrap-region-mode)
(add-hook 'js-mode-hook 'turn-on-enclose-mode)

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-path "site-lisp/couchdb-relax")
(autoload 'relax "relax" "Time to relax" t)

(add-path "site-lisp/coffee-mode")
(autoload 'coffee-mode "coffee-mode" "Coffe mode" t)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(setq coffee-tab-width 2)
(setq coffee-js-mode 'js-mode)

(add-hook 'coffee-mode-hook '(lambda ()
                               (set (make-local-variable 'tab-width) 2)
                               (local-set-key (kbd "C-c C-b") 'coffee-compile-buffer)
                               (local-set-key (kbd "C-c C-r") 'coffee-compile-region)))
(add-hook 'coffee-mode-hook 'turn-on-wrap-region-mode)
(add-hook 'coffee-mode-hook 'turn-on-enclose-mode)

(provide 'mine-javascript)
