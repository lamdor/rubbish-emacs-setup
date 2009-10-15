;; Color theme
(when window-system
  (add-path "site-lisp/color-theme-6.6.0")
  (require 'color-theme)
  (color-theme-initialize)
  (load-file (concat emacs-root "el/color-theme-twilight.el"))
  (color-theme-twilight))

(global-hl-line-mode t)

(require 'highlight-parentheses)
(highlight-parentheses-mode t)
(show-paren-mode t)

(transient-mark-mode t)

;; Remove noise
(global-hl-line-mode nil)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode nil))
(menu-bar-mode nil)

;; display settings
(defun mine-use-normal-font ()
  (interactive)
  (set-frame-parameter (selected-frame) 'font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (add-to-list 'default-frame-alist '(font . "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")))

(defun mine-use-big-font ()
  (interactive)
  (set-frame-parameter (selected-frame) 'font "-apple-Monaco-medium-normal-normal-*-20-*-*-*-m-0-iso10646-1")
  (add-to-list 'default-frame-alist '(font . "-apple-Monaco-medium-normal-normal-*-20-*-*-*-m-0-iso10646-1")))

(defun mine-use-fullscreen ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (add-to-list 'default-frame-alist '(fullscreen . 'fullboth)))

(defun mine-use-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 80))
  (add-to-list 'default-frame-alist '(alpha 90 80)))

(defun mine-use-no-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100)))

(defun mine-toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (mine-use-transparency)
    (mine-use-no-transparency)))

(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(defun mine-normal-display ()
  (interactive)
  (mine-use-normal-font)
  (mine-use-transparency))

(defun mine-pair-display ()
  (interactive)
  (mine-use-big-font)
  (mine-use-no-transparency))

(mine-normal-display)

(provide 'mine-pretty)
