;; Ruby Dependencies
(add-path "site-lisp/rvm")
(require 'rvm)
(rvm-use-default)

(add-path "site-lisp/rinari/")

(require 'rdebug)
(require 'ruby-style)
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(autoload 'run-ruby "inf-ruby" "Runs an inferior ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(require 'rcodetools)

;; Rinari setup
(setq rinari-tags-file-name "TAGS")
(setq rinari-major-modes
      (list 'find-file-hook
            'mumamo-after-change-major-mode-hook
            'dired-mode-hook
            'shell-mode-hook
            'magit-mode-hook))
(require 'rinari)
(add-path "site-lisp/rinari/util")
(require 'ruby-compilation-rspec)

(define-key rinari-minor-mode-map (kbd "C-c ; r") nil)
(define-key rinari-minor-mode-map (kbd "C-c ; r r") 'rinari-rake)
(define-key rinari-minor-mode-map (kbd "C-c ; r j") 'rinari-jrake)
(define-key rinari-minor-mode-map (kbd "C-c ' r") nil)
(define-key rinari-minor-mode-map (kbd "C-c ' r r") 'rinari-rake)
(define-key rinari-minor-mode-map (kbd "C-c ' r j") 'rinari-jrake)

;; autotest setup
(autoload 'autotest "autotest" "Run autotest" t)
(setq autotest-use-ui t)

(defun autotest-rspec ()
  "Runs autotest as rspec enabled"
  (interactive)
  (setq autotest-command "RSPEC=true autotest")
  (autotest)
  (setq autotest-command "autotest"))

(defun autotest-rspec-with-features ()
  "Runs autotest as rspec and cucumber features enabled"
  (interactive)
  (setq autotest-command "AUTOFEATURE=true RSPEC=true autotest")
  (autotest)
  (setq autotest-command "autotest"))


;; File Types
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("config\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile" . ruby-mode))

;; Hooks
(add-hook 'ruby-mode-hook '(lambda() (inf-ruby-keys)))
(add-hook 'ruby-mode-hook
          '(lambda ()
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))
(add-hook 'ruby-mode-hook 'turn-on-wrap-region-mode)
(add-hook 'ruby-mode-hook 'turn-on-enclose-mode)
(enclose-add-encloser "|" "|")
(add-path "site-lisp/ruby-end")
(require 'ruby-end)

(defadvice ruby-indent-command (around yas/try-expand-first activate)
  "Try to expand a snippet before point, then call ruby-indent-command as usual"
  (let ((yas/fallback-behavior nil))
    (unless (and (interactive-p)
                 (yas/expand))
      ad-do-it)))

;; Key Bindings (since ruby in already required, i can just change the keymap)
(define-key ruby-mode-map "\r" 'reindent-then-newline-and-indent)
(define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)
(define-key ruby-mode-map (kbd "C-x M-t") 'xmp)
(define-key ruby-mode-map (kbd "M-?") 'rct-complete-symbol)

;; Ri setup
(setq ri-repl-executable "ri_repl")

;; Misc
(defun run-jruby ()
  (interactive)
    (run-ruby "/usr/local/bin/jruby -S irb --inf-ruby-mode" "jruby"))

(defun rinari-jrake (&optional task edit-cmd-args)
  "Run rinari-rake with jrake"
  (interactive)
  (let ((ruby-compilation-executable-rake "jrake"))
      (rinari-rake task edit-cmd-args)))

;; Flymake Flymake
(require 'flymake)
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

;; Cucumber (perhaps not totally ruby related)
(add-path "site-lisp/cucumber-mode/")
(autoload 'feature-mode "feature-mode" "Major mode for editing plain text stories" t)
(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))
(add-hook 'feature-mode-hook '(lambda () (local-set-key (kbd "C-c C-a") 'autotest-switch)))

;; Gem Open Command
(defun gem-open (gem-name)
  (interactive "Mgem: ")
  (shell-command (concat "gem open " gem-name " &")))

(provide 'mine-ruby)
