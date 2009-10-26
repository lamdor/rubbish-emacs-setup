;;;; Luke's .emacs file
(setq debug-on-error t)

(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (p)
  (add-to-list 'load-path (concat emacs-root p)))

(defvar *emacs-load-start* (current-time))

(add-path "mine")
(require 'mine-projects)
(require 'mine-dependencies)
(require 'mine-misc)
(require 'mine-defuns)
(require 'mine-customizations)
(require 'mine-bindings)
(require 'mine-erc)
(require 'mine-pretty)
(require 'mine-shell)
(require 'mine-vc)
(require 'mine-org-mode)
(require 'mine-pomodoro)

;; Languages
(require 'mine-ruby)
(require 'mine-clojure)
(require 'mine-scala)
(require 'mine-java)
(require 'mine-erlang)
(require 'mine-html-xml)
(require 'mine-javascript)
(require 'mine-groovy)
(require 'mine-slime)
(require 'mine-lisp)
(require 'mine-markdown)

;; system specific loading
(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-macosx)))
;;

(setq debug-on-error nil)

(server-start)

(message "My .emacs loaded in %ds." (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

;; Visit GTD Agenda
(gtd-switch-to-agenda)
