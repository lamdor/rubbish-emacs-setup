;; Twilight Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Twilight colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
;;
;; And then (color-theme-twilight) to activate it.
;;
;; Several areas still require improvement such as recognition of code that ruby-mode doesn't
;; yet pick up (eg. parent classes), Rails/Merb keywords, or non Ruby code related areas
;; (eg. dired, HTML, etc). Please feel free to customize further and send in any improvements,
;; patches most welcome.
;;
;; MIT License Copyright (c) 2008 Marcus Crafter <crafterm@redartisan.com>
;; Credits due to the excellent TextMate Twilight theme
;;
 
(defun color-theme-twilight ()
  "Color theme by Marcus Crafter, based off the TextMate Twilight theme, created 2008-04-18"
  (interactive)
  (color-theme-install
   '(color-theme-twilight
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#a7a7a7")
      (foreground-color . "#f8f8f8")
      (mouse-color . "sienna1"))
     (default ((t (:background "black" :foreground "white"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "white"))))
     (dired-directory ((t (:foreground "#7587a6"))))
     (dired-face-directory ((t (:foreground "#7587a6"))))
     (dired-face-executable ((t (:foreground "#9b859d"))))
     (dired-executable ((t (:foreground "#9b859d"))))
     (dired-flagged ((t (:foreground "white" :background "#895f83"))))
     (dired-header ((t (:foreground "#cf6a4c"))))
     (dired-mark ((t (:foreground "#8f9d6a"))))
     (dired-marked ((t (:foreground "white" :background "#8f9d6a"))))
     (dired-perm-write ((t (:foreground "#5f5a60"))))
     (dired-symlink ((t (:foreground "#cda869"))))
     (dired-warning ((t (:foreground "white" :background "#562d56"))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-comment-face ((t (:italic t :foreground "#5f5a60"))))
     (font-lock-constant-face ((t (:foreground "#cf6a4c"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#9b703f"))))
     (font-lock-keyword-face ((t (:foreground "#cda869"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#e9c062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "#8f9d6a"))))
     (font-lock-type-face ((t (:foreground "#9b703f"))))
     (font-lock-variable-name-face ((t (:foreground "#7587a6"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#d4d0c8" :foreground "black"))))
     (ido-first-match ((t (:bold t :weight bold))))
     (ido-incomplete-regexp ((t (:foreground "white" :background "#895f83"))))
     (ido-indicator ((t (:background "#cf6a4c" :foreground "#f9ee98" :width condensed))))
     (ido-only-match ((t (:foreground "#cf6a4c"))))
     (ido-subdir ((t (:foreground "#7587a6"))))
     (minibuffer-prompt ((t (:foreground "#8f9d6a"))))
     (region ((t (:background "#27292a"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#202020"))))
     (highline-face ((t (:background "#222222"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (show-paren-match ((t (:background "#7587a6"))))
     (show-paren-mismatch ((t (:background "#9d2115")))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))
)))))
