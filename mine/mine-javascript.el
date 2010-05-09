;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-rebind-eol-bol-keys nil)

;; (add-hook 'js-mode '(lambda ())
;;           (local-set-key (kbd "RET" js-insert-and-indent)))

(setq js-indent-level 2)
(add-hook 'js-mode-hook 'turn-on-wrap-region-mode)

(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(provide 'mine-javascript)
