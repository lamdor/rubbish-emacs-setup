(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(show-paren-mode t)
(transient-mark-mode t)
(blink-cursor-mode t)
(add-to-list 'default-frame-alist '(cursor-color . "gray"))

(add-hook 'minibuffer-setup-hook 'mine-minibuffer-setup-font-bigger)
(defun mine-minibuffer-setup-font-bigger ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.3))))

;; Remove noise
(global-hl-line-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defvar mine-normal-font "Hack 14" "*The main font")
(defvar mine-big-font "Hack 20" "*The font mainly used in pairing and presentation modes")

;; utf8
;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; display settings
(defun mine-use-normal-font ()
  (interactive)
  (set-frame-parameter (selected-frame) 'font mine-normal-font)
  (add-to-list 'default-frame-alist (cons 'font mine-normal-font)))

(defun mine-use-big-font ()
  (interactive)
  (set-frame-parameter (selected-frame) 'font mine-big-font)
  (add-to-list 'default-frame-alist (cons 'font mine-big-font)))

(defun mine-toggle-fullscreen ()
  (interactive)
  (if (frame-parameter (selected-frame) 'fullscreen)
      (set-frame-parameter (selected-frame) 'fullscreen nil)
    (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)))

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
