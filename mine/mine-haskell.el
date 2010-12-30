(add-path "site-lisp/haskell-mode-2.8.0")

(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell programs." t)
(autoload 'switch-to-haskell "inf-haskell" "Show the inferior-haskell buffer.  Start the process if needed." t)

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(provide 'mine-haskell)