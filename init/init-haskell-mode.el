;; (setq haskell-mode-hook nil)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(setq haskell-program-name "ghci -isrc")

;; (eval-after-load "haskell-mode"
;;     '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
;; (eval-after-load "haskell-cabal"
;;     '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
