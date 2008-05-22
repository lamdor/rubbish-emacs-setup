;;;; Luke's .emacs file

(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (p)
  (add-to-list 'load-path (concat emacs-root p)))

(add-path "mine")
(load "dependencies.el")
(load "customizations.el")
(load "ruby.el")
(load "erlang.el")
