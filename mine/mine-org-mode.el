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
      '(("gtd.org" :maxlevel . 2)
        ("someday-maybe.org" :level . 1)))


(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-deadline-warning-days 2)
(setq org-agenda-ndays 1)
(setq org-agenda-files (list (my-org-file "gtd.org")))
(setq org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("A" "Action List"
         ((agenda "")
          (alltodo))
         ((org-agenda-todo-ignore-deadlines t)
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-with-date t)
          (org-agenda-sorting-strategy '(tag-up))))))

(setq org-todo-keywords 
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))) 
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=%s")))

;; remember-mode setup
(add-path "site-lisp/remember-mode")
(autoload 'remember "remember" nil t)
(org-remember-insinuate)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n %i\n %a" "gtd.org" "Inbox")
        ("Inbox" ?i "* %?" "gtd.org" "Inbox")
        ("Misc Task" ?m "* TODO %? %^g\n" "gtd.org" "Misc Tasks")
        ("Someday/Maybe" ?s "* %?\n %i" "someday-maybe.org" "Someday/Maybe")
        ("Remember To Checkbook" ?c "* TODO %? :@desk:\n" "gtd.org" "Checkbook")))

;; navagation helpers
(defun gtd ()
  (interactive)
  (find-file (my-org-file "gtd.org")))

(defun gtd-switch-to-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*"))

(defun gtd-someday-maybe ()
  (interactive)
  (find-file (my-org-file "someday-maybe.org")))

(defun gtd-jump ()
  (interactive)
  (find-file (my-org-file "gtd.org"))
  (org-goto))

;; key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-cj" 'org-clock-goto)
(global-set-key "\C-cl" 'org-store-link)

(global-set-key (kbd "C-c g g") 'gtd)
(global-set-key (kbd "C-c g a") 'gtd-switch-to-agenda)
(global-set-key (kbd "C-c g j") 'gtd-jump)
(global-set-key (kbd "C-c g m") 'gtd-someday-maybe)


;; for a popup window for remember mode
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "remember" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the rememeber frame"
  (if (equal "remember" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-remember
;; splits the window.
(add-hook 'remember-mode-hook
          'delete-other-windows)

(defun make-remember-frame ()
  "Create a new frame and run org-remember."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 10)))
  (select-frame-by-name "remember")
  (org-remember))

(provide 'mine-org-mode)