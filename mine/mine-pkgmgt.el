(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max)) (eval-print-last-sexp))))

(setq el-get-user-package-directory "~/.emacs.d/init")

;; FIXME(needs deps) (el-get 'sync 'gist)

;; FIXME(needs new github) (el-get 'sync 'yasnippet)

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq el-get-sources '(
                       ;; enclose from elpa
                       ;; zen-and-art theme from melpa
                       ))

(setq mine-pkgs-to-install
      (append
       '(;; lisp
         highlight-parentheses
         paredit

         ;; scala
         scala-mode
         ;; ensime

         ;; markdown
         markdown-mode

         ;; misc
         smex
         full-ack
         undo-tree
         wrap-region
         switch-window
         scratch
         magit)
       (mapcar 'el-get-sourcename el-get-sources)))

(el-get 'sync mine-pkgs-to-install)

(provide 'mine-pkgmgt)
