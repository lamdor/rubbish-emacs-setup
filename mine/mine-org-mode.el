(add-path "site-lisp/org-mode/lisp")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; configuration
(setq org-directory "~/org/")
(defun my-org-file (file)
  (concat org-directory file))

(setq org-completion-use-ido t)
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
(setq org-blank-before-new-entry nil)
(setq org-startup-folded 'content)

(setq org-enforce-todo-dependencies t)

(setq org-log-into-drawer "LOGBOOK")
(setq org-log-done 'time)

(setq org-refile-use-outline-path t)
(setq org-refile-targets
      '(("learning-gtd.org" :maxlevel . 2)
        ("learning-someday-maybe.org" :level . 1)))

(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-files (list (my-org-file "learning-gtd.org")))

(setq org-todo-keywords 
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))) 
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=%s")))

;; remember-mode setup
(add-path "site-lisp/remember-mode")
(autoload 'remember "remember" nil t)
(org-remember-insinuate)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n %i\n %a" "learning-gtd.org" "Inbox")
        ("Someday/Maybe" ?s "* %?\n %i" "learning-someday-maybe.org" "Someday/Maybe")))

;; misc helpers
(defun gtd ()
  (interactive)
  (find-file (my-org-file "learning-gtd.org")))

;; key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-cg" 'gtd)
(global-set-key "\C-cl" 'org-store-link)

(provide 'mine-org-mode)