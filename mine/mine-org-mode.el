(add-path "site-lisp/org-mode/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-hide-leading-stars t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-log-into-drawer "LOGBOOK")
(setq org-log-done 'time)

(setq org-todo-keywords 
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))) 
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=%s")))

(provide 'mine-org-mode)