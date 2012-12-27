(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-directory "~/notes")

;; The structure of this follows the Org manual: http://orgmode.org/org.html

;; Introduction
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Structure
(setq org-blank-before-new-entry nil
      org-startup-indented t
      org-startup-folded 'content)

;; Hyperlinks
(setq org-confirm-shell-link-function 'y-or-n-p)

;; TODO Items
(setq org-enforce-todo-dependencies t
      org-todo-keywords '((sequence "TODO(t)" "WAIT(w!/!)" "STARTED(s!)" "REVIEW(r!)" "DELEGATED(l!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-todo-keyword-faces '(("TODO" . org-warning)
                               ("STARTED" . (:foreground "yellow"))
                               ("REVIEW" . (:background "blue" :foreground "white" :weight bold)))
      org-default-priority 85
      org-log-done 'time)

;; Tags
(setq org-tags-column -84)

;; Properties and Columns
(setq org-use-property-inheritance t
      org-columns-default-format "%69ITEM %TODO %Effort{+} %TAGS")

;; Dates and Times
(global-set-key (kbd "C-c C-x C-x") 'org-clock-in-last)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(setq org-clock-idle-time 15)

;; Capture - Refile - Archive
(setq org-capture-templates
      `(("c" "Capture to Inbox" entry
         (file+headline ,(concat org-directory "/todo.org") "Inbox")
         "* %?\n")
        ("l" "Capture Link to Inbox" entry
         (file+headline ,(concat org-directory "/todo.org") "Inbox") 
         "* %c %?\n")
        ("s" "Capture Link and Selection to Inbox" entry
         (file+headline ,(concat org-directory "/todo.org") "Inbox") 
         "* %c %?\n%i\n")))

(setq mine-capture-frame-name "Capture")

(add-hook 'org-capture-mode-hook '(lambda ()
                                    (if (equal mine-capture-frame-name (frame-parameter nil 'name))
                                        (delete-other-windows))))

(defun mine-clean-up-org-capture-frame (&optional frame)
  (delete-frame frame))

(defun mine-set-capture-frame-parameters (frame)
  (modify-frame-parameters frame `((width . 87)
                                   (height . 12)
                                   (name . ,mine-capture-frame-name)))
  (set-frame-position frame 360 200))

(defun mine-make-org-capture-frame ()
  "Create a new org-capture frame"
  (interactive)
  (let ((f (make-frame)))
    (mine-set-capture-frame-parameters f)
    (select-frame f)
    (condition-case err (org-capture)
      (error (mine-clean-up-org-capture-frame f)))))

(defadvice org-switch-to-buffer-other-window (around dont-split-in-capture-frame activate)
  (if (equal mine-capture-frame-name (frame-parameter nil 'name))
      (switch-to-buffer (ad-get-arg 0))
    ad-do-it))

(defadvice org-capture-finalize (after delete-capture-frame-if-necessary activate)
  "Advise org-capture-finalize to delete the frame if a capture frame"
  (if (equal mine-capture-frame-name (frame-parameter nil 'name))
      (mine-clean-up-org-capture-frame)))

(global-set-key "\C-cc" 'org-capture)

(require 'org-protocol)

(defadvice org-protocol-capture (before make-capture-frame activate)
  (mine-set-capture-frame-parameters (selected-frame)))

(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-targets '((nil . (:maxlevel . 2))))

;; Agenda vieww
(setq org-agenda-files (list (concat org-directory "/todo.org"))
      org-agenda-search-headline-for-time nil
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-ndays 5
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-deadlines nil
      org-deadline-warning-days 5
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-sorting-strategy '(tag-up time-up todo-state-down priority-down)
      org-agenda-compact-blocks t
      org-agenda-tags-column -84
      org-agenda-repeating-timestamp-show-all nil
      org-agenda-start-with-clockreport-mode nil)

(setq org-agenda-custom-commands
      '(("n" "Next actions"
         ((agenda nil ((org-agenda-ndays 1)))
          (alltodo)))))

(setq org-stuck-projects
      '("CATEGORY=\"Projects\"+LEVEL=2" ("TODO") ""))

;; MobileOrg
(autoload 'org-mobile-push "org-mobile" "Push the state of the org files to org-mobile-directory" t)
(autoload 'org-mobile-pull "org-mobile" "Pull the contents of org-mobile-capture-file" t)
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))

;; Miscellaneous
(setq org-completion-use-ido t)
(run-at-time t 300 'org-save-all-org-buffers)
(add-hook 'org-mode-hook 'mine-leave-whitespace-in-buffer)
(add-hook 'org-mode-hook '(lambda () (toggle-truncate-lines nil)))

(defun gtd-find-todo ()
  (interactive)
  (if (equal (buffer-name (current-buffer))
             "todo.org")
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "todo.org")))

(defun gtd-someday-maybe ()
  (interactive)
  (if (equal (buffer-name (current-buffer))
             "someday-maybe.org")
      (switch-to-buffer (other-buffer))
    (find-file (concat org-directory "/someday-maybe.org"))))

(global-set-key (kbd "C-c g g") 'gtd-find-todo)
(global-set-key (kbd "C-c g s") 'gtd-someday-maybe)

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
