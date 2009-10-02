(add-path "site-lisp/org-mode/lisp")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; configuration
(setq org-directory "~/org/")
(defun my-org-file (file)
  (concat org-directory file))

;; automatically save org buffers
(run-at-time t 3600 'org-save-all-org-buffers)

;; dislplay configuration
(setq org-completion-use-ido t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-tags-column -92
      org-blank-before-new-entry nil
      org-startup-folded 'content)

;; todo configuration
(setq org-enforce-todo-dependencies t
      org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-use-fast-todo-selection t)

;; logging configuration
(setq org-log-into-drawer "LOGBOOK"
      org-log-done 'time)

;; link configuration
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=%s")
        ("pending" . "file:///Users/luke/Desktop/Pending/%s")
        ("dump" . "file:///Users/luke/Documents/Dump/%s")
        ("bcredmine" . "https://redmine.bigcreek.com/issues/show/%s")))

;; refiling configuration
(setq org-refile-use-outline-path nil
      org-refile-targets
      '(("gtd.org" :maxlevel . 3)
        ("someday-maybe.org" :level . 1)))

;; agenda configuration
(setq org-agenda-dim-blocked-tasks 'invisible
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-deadline-warning-days 2
      org-agenda-ndays 1
      org-agenda-files (list (my-org-file "gtd.org"))
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("A" "Action List"
         ((agenda "")
          (alltodo))
         ((org-agenda-todo-ignore-deadlines t)
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-with-date t)
          (org-agenda-sorting-strategy '(tag-up))))))

;; export the tags that I want exported

(setq mine-batch-export-tags '("@errands"
                               "@groceries"
                               "@phone"
                               "@home"))

(dolist (export-tag mine-batch-export-tags)
  (add-to-list 'org-agenda-custom-commands
               (list export-tag (format "Exported Actions (%s)" export-tag)
                     (list (list 'tags-todo export-tag))
                     '((org-agenda-todo-ignore-deadlines t)
                       (org-agenda-todo-ignore-scheduled t)
                       (org-agenda-todo-ignore-with-date t)
                       (org-agenda-with-colors nil)
                       (org-agenda-sorting-strategy '(tag-up)))
                     (my-org-file (format "export/%s.txt" export-tag)))))


(setq org-agenda-exporter-settings
      '((htmlize-output-type 'font)))

(defun mine-batch-export-agenda-views ()
  (interactive)
  ;; (gtd)
  (org-batch-store-agenda-views))

;; remember-mode setup
(add-path "site-lisp/remember-mode")
(autoload 'remember "remember" nil t)
(org-remember-insinuate)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n %i\n %a" "gtd.org" "Inbox")
        ("Inbox" ?i "* %?" "gtd.org" "Inbox")
        ("Misc Task" ?m "* TODO %? %^g\n" "gtd.org" "Misc Tasks")
        ("Misc ISEBA Task" ?b "* TODO %? :@bigcreek:\n" "gtd.org" "ISEBA")
        ("Someday/Maybe" ?s "* %?\n %i" "someday-maybe.org" "Someday/Maybe")
        ("Remember To Checkbook" ?c "* TODO %? on %t :@desk:\n" "gtd.org" "Mine Checkbook")))

;; navagation helpers
(defun gtd ()
  (interactive)
  (find-file (my-org-file "gtd.org")))

(defun gtd-switch-to-agenda ()
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer "*Org Agenda*")
        (org-fit-agenda-window))
    (org-agenda nil "A")))

(defun gtd-someday-maybe ()
  (interactive)
  (find-file (my-org-file "someday-maybe.org")))

(defun gtd-jump ()
  (interactive)
  (find-file (my-org-file "gtd.org"))
  (org-goto))

(defun gtd-pomodori ()
  (interactive)
  (find-file (my-org-file "pomodori.org")))

;; key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-cj" 'org-clock-goto)
(global-set-key "\C-cl" 'org-store-link)

(global-set-key (kbd "C-c g g") 'gtd)
(global-set-key (kbd "C-c g a") 'gtd-switch-to-agenda)
(global-set-key (kbd "C-c g p") 'gtd-pomodori)
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

;; Colors

(custom-set-faces
 '(outline-1 ((t (:foreground "#D6B163" :bold t))))
 '(outline-2 ((t (:foreground "#A5F26E" :bold t))))
 '(outline-3 ((t (:foreground "#B150E7" :bold nil))))
 '(outline-4 ((t (:foreground "#529DB0" :bold nil))))
 '(outline-5 ((t (:foreground "#CC7832" :bold nil))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-agenda-date ((t (:inherit font-lock-type-face))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
 '(org-scheduled-today ((t (:foreground "#ff6ab9" :italic t))))
 '(org-scheduled-previously ((t (:foreground "#d74b4b"))))
 '(org-upcoming-deadline ((t (:foreground "#d6ff9c"))))
 '(org-warning ((t (:foreground "#8f6aff" :italic t))))
 '(org-date ((t (:inherit font-lock-constant-face))))
 '(org-tag ((t (:inherit font-lock-comment-delimiter-face))))
 '(org-hide ((t (:foreground "#191919"))))
 '(org-todo ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
 '(org-done ((t (:background "DarkGreen" :foreground "white" :box (:line-width 1 :style released-button)))))
 '(org-column ((t (:background "#222222"))))
 '(org-column-title ((t (:background "DarkGreen" :foreground "white" :bold t :box (:line-width 1 :style released-button))))))

(provide 'mine-org-mode)
