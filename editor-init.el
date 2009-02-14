;;;; Luke's .emacs file
(setq debug-on-error t)

(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (p)
  (add-to-list 'load-path (concat emacs-root p)))

(add-path "mine")

(require 'mine-defuns)
(require 'mine-customizations)
(require 'mine-misc)
(require 'mine-bindings)


