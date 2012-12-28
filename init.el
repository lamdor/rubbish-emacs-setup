(add-to-list 'load-path (concat user-emacs-directory "/mine"))


(require 'mine-env)
(require 'mine-builtin) ;; split up
(require 'mine-defuns)
(require 'mine-advice)
(require 'mine-bindings)
(require 'mine-desktop)
(require 'mine-pretty)

(require 'mine-rcirc)

(require 'mine-pkgmgt)

(case system-type
  ('darwin (require 'mine-macosx))
  ('gnu/linux (require 'mine-linux)))

;; load files under custom/*.el
(setq mine-custom-dir (concat user-emacs-directory "/custom/"))
(if (file-exists-p mine-custom-dir)
 (let ((custom-files (directory-files mine-custom-dir t "\.el$")))
  (mapcar 'load-file custom-files)))

(setq custom-file (expand-file-name (concat user-emacs-directory "/customizations.el")))
(load custom-file)

(cd (getenv "HOME"))
(mine-normal-display)
(server-start)
