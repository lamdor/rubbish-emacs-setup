(add-path "site-lisp/haskell-mode-2.8.0")

(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell programs." t)
(autoload 'switch-to-haskell "inf-haskell" "Show the inferior-haskell buffer.  Start the process if needed." t)
(autoload 'haskell-cabal-mode "haskell-cabal" "Major mode for Cabal package description files." t)

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook '(lambda ()
                                (local-set-key "\r" 'newline)))
(add-hook 'haskell-mode-hook '(lambda ()
                                (local-set-key (kbd "C-o") 'open-line)))
(add-hook 'haskell-mode-hook '(lambda ()
                                (enclose-remove-encloser "'")
                                (enclose-remove-encloser "|")))

(defun mine-haskell-indent-buffer ()
  (interactive)
  (haskell-indent-align-guards-and-rhs (point-min) (point-max)))

(add-hook 'haskell-mode-hook '(lambda ()
                                (local-set-key (kbd "C-c o") 'mine-haskell-indent-buffer)))

(add-hook 'haskell-mode-hook 'enclose-mode)
(add-hook 'haskell-mode-hook 'wrap-region-mode)
(add-hook 'haskell-mode-hook 'subword-mode)

(provide 'mine-haskell)