(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" default)))
 '(magit-diff-options nil)
 '(safe-local-variable-values (quote ((eval font-lock-add-keywords nil (\` (((\, (concat "(" (regexp-opt (quote ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")) t) "\\_>")) 1 (quote font-lock-variable-name-face))))) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((((class color) (min-colors 89)) (:foreground "#E6DB74" :weight bold))))
 '(org-agenda-date ((t (:inherit font-lock-type-face))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))) t)
 '(org-agenda-done ((t (:inherit org-archived :strike-through t))))
 '(org-column ((t (:background "#222222"))))
 '(org-column-title ((t (:background "DarkGreen" :foreground "white" :bold t :box (:line-width 1 :style released-button)))))
 '(org-date ((t (:inherit font-lock-constant-face))))
 '(org-done ((t (:background "DarkGreen" :foreground "white" :strike-through t :box (:line-width 1 :style released-button)))))
 '(org-headline-done ((t (:inherit org-archived :strike-through t))))
 '(org-hide ((t (:foreground "#191919"))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-scheduled-previously ((t (:foreground "#d74b4b"))))
 '(org-scheduled-today ((t (:foreground "#ff6ab9" :italic t))))
 '(org-tag ((t (:inherit font-lock-comment-delimiter-face))))
 '(org-todo ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
 '(org-upcoming-deadline ((t (:foreground "#d6ff9c"))))
 '(org-warning ((t (:foreground "#8f6aff" :italic t))))
 '(outline-1 ((t (:foreground "#D6B163" :bold t))))
 '(outline-2 ((t (:foreground "#A5F26E" :bold t))))
 '(outline-3 ((t (:foreground "#B150E7" :bold nil))))
 '(outline-4 ((t (:foreground "#529DB0" :bold nil))))
 '(outline-5 ((t (:foreground "#CC7832" :bold nil)))))
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
