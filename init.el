;;;; Luke's .emacs file

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

;; Languages
(require 'mine-ruby)
(require 'mine-clojure)
(require 'mine-java)
(require 'mine-erlang)
(require 'mine-html-xml)
(require 'mine-groovy)
(require 'mine-lisp)

;; system specific loading
(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-macosx)))
;; 
(message "My .emacs loaded in %ds." (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
