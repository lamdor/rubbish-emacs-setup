;; Gist
(add-path "site-lisp/gist/")
(require 'gist)
  
;; yasnippet
(add-path "site-lisp/yasnippet-0.5.6/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-root "site-lisp/yasnippet-0.5.6/snippets/"))

;; Misc Files
(add-path "el/")
(require 'find-recursive)
(require 'tidy)
(require 'tail)
(require 'keywiz)
(require 'unit-test)

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(autoload 'yaml-mode "yaml-mode" "Major mode for editting yaml files" t)

;; unit-test
(require 'unit-test)

;; typing-of-emacs
(setq toe-starting-time-per-word 20)
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; twittering-mode
(setq twittering-username "lukeamdor")
(autoload 'twittering-mode "twittering-mode" "Twittering mode" t)

;; htmlize
(autoload 'htmlize-file "htmlize" "HTMLize a file" t)

(provide 'mine-dependencies)