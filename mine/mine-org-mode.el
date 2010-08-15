(add-path "site-lisp/org-mode/lisp")
(require 'org)
(add-to-list 'org-modules 'org-habit)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; configuration
(if (not (boundp 'org-directory))
    (setq org-directory "~/org"))
(defun my-org-file (file)
  (concat org-directory "/" file))

;; files

(setq mine-org-files
      (split-string (save-excursion
                      (find-file (my-org-file "AGENDA_FILES"))
                      (buffer-string))))

(setq mine-outside-org-files ())

(setq org-agenda-files (append
                        (mapcar 'my-org-file mine-org-files)
                        mine-outside-org-files))

;; automatically save org buffers
(run-at-time t 300 'org-save-all-org-buffers)

(setq org-global-properties
      '(("Effort_ALL" . "0 1 2 3 5 8 13 21")))

;; display configuration
(setq org-completion-use-ido t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-tags-column -92
      org-blank-before-new-entry nil
      org-startup-folded 'content
      org-columns-default-format "%75ITEM %TODO %Effort{+} %TAGS")

;; allow yasnippet
(add-hook 'org-mode-hook
          '(lambda ()
             (org-set-local 'yas/trigger-key [tab])
             (define-key yas/minor-mode-map [tab] 'yas/expand)))

;; todo configuration
(setq org-enforce-todo-dependencies t
      org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "INPROGRESS(i!)" "WATCH(a)" "REVIEW(r!)" "|" "DONE(d!)" "CANCELED(c)"))
      org-use-fast-todo-selection t
      org-default-priority 85)

;; tag configuration
(setq org-tag-alist
      '((:startgroup . nil)
        ("@mac" . ?m)
        ("@development" . ?d)
        ("@bigcreek" . ?b)
        ("@email" . ?e)
        ("@home" . ?h)
        ("@phone" . ?p)
        ("@desk" . ?a)
        ("@read" . ?d)
        ("@errands" . ?r)
        ("@groceries" . ?g)
        ("@geo" . ?o)
        ("@kate" . ?k)
        (:endgroup . nil)))

;; logging configuration
(setq org-log-into-drawer "LOGBOOK"
      org-log-done 'time)

;; link configuration
(setq org-confirm-shell-link-function 'y-or-n-p)
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=%s")
        ("pending" . "shell:showinfinder \"/Users/luke/Desktop/Pending/%s\"")
        ("outbox" . "shell:showinfinder \"/Users/luke/Desktop/Outbox/%s\"")
        ("dump" . "file:///Users/luke/Documents/Dump/%s")
        ("geojira" . "https://jira.geolearning.com/browse/%s")
        ("reveal" . "shell:showinfinder \"%s\"")))

;; refiling configuration
(setq org-refile-use-outline-path nil
      org-refile-targets (append
                          (mapcar '(lambda (orgf)
                                     (append (list orgf) '(:maxlevel . 3))) mine-org-files)
                          '(("someday-maybe.org" :level . 1))))

;; agenda configuraion
(setq org-agenda-search-headline-for-time nil
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-ndays 1
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-deadlines nil
      org-deadline-warning-days 3
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-sorting-strategy '(tag-up time-up priority-down)
      org-agenda-compact-blocks t
      org-agenda-tags-column -92
      org-habit-show-habits-only-for-today nil
      org-habit-preceding-days 20
      org-habit-following-days 3
      org-habit-graph-column 55)

(setq org-agenda-custom-commands
      '(("@" . "Outside Contexts")
        ("@e" "@errands" tags-todo "@errands")
        ("@g" "@groceries" tags-todo "@groceries")
        ("@p" "@phone" tags-todo "@phone")
        ("@h" "@home" tags-todo "@home")
        ("g" "Action List"
         ((agenda)
          (todo "TODO|INPROGRESS"))
         ((org-agenda-todo-ignore-with-date t)))
        ("f" "Flat Action List" todo "TODO|INPROGRESS")
        ("i" "In Progress Items" todo "INPROGRESS")
        ("w" "WAIT Items" todo "WAIT")
        ("A" "A Priority TODOs" tags-todo "PRIORITY=\"A\"+TODO=\"TODO\"")
        ("B" "B Priority TODOs" tags-todo "PRIORITY=\"B\"+TODO=\"TODO\"")
        ("C" "C Priority TODOs" tags-todo "PRIORITY=\"C\"+TODO=\"TODO\"")
        ("N" "No Priority TODOs" tags-todo "PRIORITY=\"\"+TODO=\"TODO\"")))

;; org-mobile setup
(setq org-mobile-directory (concat (getenv "HOME") "/Dropbox" "/MobileOrg"))
(setq org-mobile-inbox-for-pull (my-org-file "from-mobile.org"))
(setq org-mobile-agendas 'custom)

(autoload 'org-mobile-push "org-mobile" "Push the state of the org files to org-mobile-directory" t)
(autoload 'org-mobile-pull "org-mobile" "Pull the contents of org-mobile-capture-file" t)

;; org publish projects

(setq org-publish-project-alist
      '(("absoluterubbish.net"
         :components ("org-absoluterubbish.net" "org-posts-absoluterubbish.net" "org-posts-absoluterubbish.net-static"))
        ("org-absoluterubbish.net"
         :base-directory "~/code/absoluterubbish.net/org/"
         :base-extension "org"
         :publishing-directory "~/code/absoluterubbish.net"
         :publishing-function org-publish-org-to-html
         :recursive t
         :body-only t)
        ("org-posts-absoluterubbish.net"
         :base-directory "~/code/absoluterubbish.net/org-posts/"
         :base-extension "org"
         :publishing-directory "~/code/absoluterubbish.net/_posts"
         :publishing-function org-publish-org-to-html
         :recursive t
         :body-only t)
        ("org-posts-absoluterubbish.net-static"
         :base-directory "~/code/absoluterubbish.net/org-posts/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/code/learning/absoluterubbish.net/_posts"
         :publishing-function org-publish-attachment
         :recursive t)))

;; remember-mode setup
(add-path "site-lisp/remember-mode")
(autoload 'remember "remember" nil t)
(org-remember-insinuate)
(setq org-remember-templates
      '(("Inbox" ?i "* %?" "inbox.org" "Inbox")
        ("Misc Task" ?m "* TODO %? %^g\n" "misc-tasks.org" "Misc Tasks")
        ("Misc Task (In Progress)" ?p "* INPROGRESS %? %^g\n" "misc-tasks.org" "Misc Tasks")
        ("Someday/Maybe" ?s "* %?\n %i" "someday-maybe.org" "Someday/Maybe")
        ("Watch" ?w "* WATCH %? :@mac:\n\t:SCHEDULED: %^{When to remind}t\n" "watch.org" "Watchlist/Reminders")
        ("Remember To Checkbook" ?c "* TODO remember %? on %t :@desk:\n" "financial.org" "Mine Checkbook")))

;; navagation helpers
(defun gtd ()
  (interactive)
  (find-file (my-org-file "gtd.org")))

(defun gtd-agenda ()
  (interactive)
  (if (and (equal (buffer-name (current-buffer))
                  "*Org Agenda*")
           (equal org-agenda-name
                  "Action List"))
      (switch-to-buffer (other-buffer))
    (if (get-buffer "*Org Agenda*")
        (progn
          (switch-to-buffer "*Org Agenda*")
          (delete-other-windows)
          (if (not (equal org-agenda-name "Action List"))
              (org-agenda nil "g")))
      (progn
        (org-agenda nil "g")
        (delete-other-windows)))))

(defun gtd-find-inbox ()
  (interactive)
  (if (equal (buffer-name (current-buffer))
             "inbox.org")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "inbox.org")))

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

(global-set-key (kbd "C-c g g") 'gtd-agenda)
(global-set-key (kbd "C-c g a") 'gtd-switch-to-agenda)
(global-set-key (kbd "C-c g p") 'gtd-pomodori)
(global-set-key (kbd "C-c g i") 'gtd-find-inbox)
(global-set-key (kbd "C-c g j") 'gtd-jump)
(global-set-key (kbd "C-c g s") 'gtd-someday-maybe)

(define-key org-mode-map (kbd "C-c C-,") 'org-priority)
(define-key org-agenda-mode-map (kbd "C-c C-,") 'org-agenda-priority)


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
