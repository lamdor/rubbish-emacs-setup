(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; move to custom file
(setq org-directory "~/notes")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))

;; automatically save org buffers
(run-at-time t 300 'org-save-all-org-buffers)

;; display configuration
(setq org-completion-use-ido t
      org-tags-column -92
      org-startup-folded 'content
      org-columns-default-format "%75ITEM %TODO %Effort{+} %TAGS")
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
          '(lambda () (toggle-truncate-lines nil)))

(setq org-use-property-inheritance t)

;; no extra line at the end
(add-hook 'org-mode-hook 'mine-leave-whitespace-in-buffer)

;; heading configuration
(setq org-blank-before-new-entry nil)

;; todo configuration
(setq org-enforce-todo-dependencies t
      org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "INPROGRESS(i!)" "WATCH(a)" "REVIEW(r!)" "DELEGATED(l!)" "|" "DONE(d!)" "CANCELED(c)"))
      org-default-priority 85)

;; link configuration
(setq org-confirm-shell-link-function 'y-or-n-p)

;; agenda configuraion
(setq org-agenda-search-headline-for-time nil
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-ndays 1
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-deadlines nil
      org-deadline-warning-days 5
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-sorting-strategy '(tag-up time-up todo-state-down priority-down)
      org-agenda-compact-blocks t
      org-agenda-tags-column -92
      org-agenda-repeating-timestamp-show-all nil)

;; (setq org-file-apps
;;       (append org-file-apps '((directory . emacs))))

(autoload 'org-mobile-push "org-mobile" "Push the state of the org files to org-mobile-directory" t)
(autoload 'org-mobile-pull "org-mobile" "Pull the contents of org-mobile-capture-file" t)

(add-hook 'org-clock-in-hook '(lambda () (if (not org-timer-current-timer)
                                             (org-timer-set-timer))))
(add-hook 'org-clock-out-hook '(lambda () (if org-timer-current-timer
                                              (org-timer-cancel-timer))))

;; Keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

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
