(require 'ruby-electric)
(require 'rdebug)
(require 'ruby-style)
(require 'autotest)

;; Ruby Setup
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; inferior ruby support
(autoload 'run-ruby "inf-ruby" "Runs an inferior ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda() (inf-ruby-keys)))

(add-hook 'ruby-mode-hook '(lambda() (local-set-key "\r" 'ruby-reindent-then-newline-and-indent)))

(defun run-jruby ()
  (interactive)
    (run-ruby "/usr/local/jruby/current/bin/jruby -S irb --inf-ruby-mode"))

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
