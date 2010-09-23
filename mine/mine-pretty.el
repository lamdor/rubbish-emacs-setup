;; Color theme
(add-path "site-lisp/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(load-file (concat emacs-root "site-lisp/zen-and-art/zen-and-art.el"))
;; (load-file (concat emacs-root "el/color-theme-twilight.el"))
(color-theme-zen-and-art)

(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(require 'highlight-parentheses)
(highlight-parentheses-mode t)
(show-paren-mode t)

(transient-mark-mode t)
(blink-cursor-mode t)

;; Remove noise
;; (global-hl-line-mode t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq mine-normal-font "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-utf8-1")
(setq mine-big-font "-apple-Monaco-medium-normal-normal-*-20-*-*-*-m-0-utf8-1")

;; display settings
(defun mine-use-normal-font ()
  (interactive)
  (set-frame-parameter (selected-frame) 'font mine-normal-font)
  (add-to-list 'default-frame-alist (cons 'font mine-normal-font)))

(defun mine-use-big-font ()
  (interactive)
  (set-frame-parameter (selected-frame) 'font mine-big-font)
  (add-to-list 'default-frame-alist (cons 'font mine-big-font)))

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
  (mine-use-no-transparency))

(defun mine-pair-display ()
  (interactive)
  (mine-use-big-font)
  (mine-use-no-transparency))

(provide 'mine-pretty)
