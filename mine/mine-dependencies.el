;; Gist
(add-path "site-lisp/gist/")
(require 'gist)

;; yasnippet
(add-path "site-lisp/yasnippet-0.6.1c")
(require 'yasnippet)
(setq yas/root-directories
      (list (concat emacs-root "site-lisp/yasnippet-0.6.1c/snippets/")
            (concat emacs-root "site-lisp/cucumber-mode/snippets/")
            (concat emacs-root "mysnippets/")))
(mapc 'yas/load-directory yas/root-directories)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/no-prompt))
(yas/global-mode)

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

;; mustache-mode
(autoload 'tpl-mode "mustache-mode" "Major mode for editting mustache/CTemplate files" t)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . tpl-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . tpl-mode))

;; unit-test
(require 'unit-test)

;; wrap-region
(add-path "site-lisp/wrap-region")
(require 'wrap-region)

;; enslose
(add-path "site-lisp/enclose")
(require 'enclose)

;; typing-of-emacs
(setq toe-starting-time-per-word 20)
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; twittering-mode
(setq twittering-username "rubbish")
(autoload 'twittering-mode "twittering-mode" "Twittering mode" t)

;; htmlize
(autoload 'htmlize-file "htmlize" "HTMLize a file" t)

;; smex
(add-path "site-lisp/smex")
(require 'smex)
(eval-after-load "init.el" '(smex-initialize))
(smex-auto-update)

;; ack
;; (autoload 'ack "ack" "Search using ack" t)
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(provide 'mine-dependencies)
