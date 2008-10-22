;;;; Luke's .emacs file

(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (p)
  (add-to-list 'load-path (concat emacs-root p)))

(defvar *emacs-load-start* (current-time))

(add-path "mine")
(load "projects.el")
(load "dependencies.el")
(load "misc.el")
(load "customizations.el")
(load "ruby.el")
(load "erlang.el")
(load "java.el")
(load "groovy.el")
(load "projects.el")

(load "cedet.el")

(message "My .emacs loaded in %ds." (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
