(setq debug-on-error t)

(defvar *emacs-load-start* (current-time))

(add-to-list 'load-path "~/.emacs.d/mine")

(require 'mine-el-get) ;; for pkg mgt
(require 'mine-env)
(require 'mine-misc) ;; builtin deps
(require 'mine-dependencies) ;; external deps
(require 'mine-defuns)
(require 'mine-customizations) ;; emacs built-in customize things
(require 'mine-bindings)
(require 'mine-pretty)
(require 'mine-desktop)

(require 'mine-lisp)

(require 'mine-linux)

(setq debug-on-error nil)

(cd (getenv "HOME"))
(mine-normal-display)
(server-start)

(message "My .emacs loaded in %ds." (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
