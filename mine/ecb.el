;; ECB (Emacs Code Browser)
(add-path "site-lisp/ecb-2.32/")
(require 'ecb-autoloads)

(setq ecb-source-path (quote ("~/code/learning/bank" "~/code" )))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(ecb-wget-setup (quote cons))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
