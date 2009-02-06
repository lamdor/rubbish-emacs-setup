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
(require 'mine-vc)
(require 'mine-pretty-lite)

;; system specific loading
(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-macosx)))


