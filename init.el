;; Dependecies

;; eeio
(add-to-list 'load-path "/Users/luke/.emacs.d/site-lisp/eieio/")
(require 'eieio)

;; Semantic
(add-to-list 'load-path "/Users/luke/.emacs.d/site-lisp/semantic/")
(setq semantic-load-turn-everything-on t)
(require 'semantic-load)

;; speedbar
(add-to-list 'load-path "/Users/luke/.emacs.d/site-lisp/speedbar/")
(require 'speedbar)

;; ECB (Emacs Code Browser)
(add-to-list 'load-path "/Users/luke/.emacs.d/site-lisp/ecb-2.32/")
(require 'ecb-autoloads)

;; Git Integration
(add-to-list 'load-path "/Users/luke/.emacs.d/site-lisp/git/")
(require 'git)
(require 'vc-git)
(require 'git-blame)
(add-to-list 'vc-handled-backends 'GIT)

;; Misc Files
(add-to-list 'load-path "/Users/luke/.emacs.d/el/")
(require 'snippet)
(require 'find-recursive)
(require 'ruby-electric)
(require 'inf-ruby)
(require 'rdebug)
(require 'ruby-mode)
(require 'ruby-style)
(require 'autotest)
(require 'tail)
(require 'keywiz)
(require 'unit-test)

;; Key Bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-region)

;; Ruby Setup
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; Flymake Ruby
(require 'flymake)
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))

;; Erlang Support
(add-to-list 'load-path "/Users/luke/.emacs.d/site-lisp/erlang/")
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)


;; Setup Environmental Variables
(if window-system (ns-grabenv "/bin/bash" 
			      "source ~/.bash_login"))
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)

;; Put backup files in a specific dir
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

;; Look Pretty
(global-hl-line-mode 1)
(mouse-wheel-mode t)