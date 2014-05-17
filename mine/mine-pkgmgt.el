(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(package-initialize t)

(setq el-get-install-skip-emacswiki-recipes t)
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max)) (eval-print-last-sexp))))

(setq el-get-user-package-directory "~/.emacs.d/init")

(setq el-get-sources '(
                       (:name wgrep
                              :description "Writable grep buffer and apply the changes to files"
                              :website "https://github.com/mhayashi1120/Emacs-wgrep"
                              :type github
                              :pkgname "mhayashi1120/Emacs-wgrep")
                        (:name enclose
                               :description "Enclose cursor within punctuation pairs"
                               :type elpa
                               :autoloads nil
                               :prepare (progn
                                          (autoload 'enclose-global-mode "enclose" nil t)
                                          (autoload 'enclose-mode "enclose" nil t)))
                        (:name wrap-region
                               :description "Wrap text with punctation or tag"
                               :type elpa
                               :depends (dash) ;; not depended on within elpa :(
                               :prepare
                               (progn
                                 (autoload 'wrap-region-mode "wrap-region" nil t)
                                 (autoload 'turn-on-wrap-region-mode "wrap-region" nil t)
                                 (autoload 'turn-off-wrap-region-mode "wrap-region" nil t)
                                 (autoload 'wrap-region-global-mode "wrap-region" nil t)))

                       (:name yasnippet
                              :website "https://github.com/capitaomorte/yasnippet.git"
                              :description "YASnippet is a template system for Emacs."
                              :type github
                              :pkgname "capitaomorte/yasnippet"
                              :features "yasnippet"
                              :compile "yasnippet.el")

                       (:name zen-and-art-theme
                              :description "A port of the zen-and-art color theme using the new deftheme format."
                              :type elpa
                              :repo ("melpa" . "http://melpa.milkbox.net/packages/")
                              :post-init (progn
                                           (add-to-list 'custom-theme-load-path default-directory)))
                       (:name monokai-theme
                              :description "A port of the zen-and-art color theme using the new deftheme format."
                              :url "https://github.com/lvillani/el-monokai-theme"
                              :type elpa
                              :post-init (progn
                                           (add-to-list 'custom-theme-load-path default-directory)))

                       (:name elixir-mode
                              :description "Emacs major mode for Elixir"
                              :type elpa
                              :repo ("melpa" . "http://melpa.milkbox.net/packages/")
                              :post-init (progn
                                           (add-to-list 'auto-mode-alist '("\.ex$" . elixir-mode))
                                           (add-to-list 'auto-mode-alist '("\.elixir$" . elixir-mode))))

                       (:name scala-mode2
                              :website "https://github.com/hvesalai/scala-mode2"
                              :type github
                              :pkgname "hvesalai/scala-mode2")
                       (:name sbt-mode
                              :website "https://github.com/hvesalai/sbt-mode"
                              :description "An emacs mode for interacting with scala sbt and projects"
                              :type github
                              :pkgname "hvesalai/sbt-mode")

                       (:name org-mode
                              :website "http://orgmode.org/"
                              :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
                              :type git
                              :url "git://orgmode.org/org-mode.git"
                              :checkout "maint"
                              :info "doc"
                              :build/berkeley-unix `,(mapcar
                                                      (lambda (target)
                                                        (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                                      '("oldorg"))
                              :build `,(mapcar
                                        (lambda (target)
                                          (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                        '("oldorg"))
                              :load-path ("." "lisp" "contrib/lisp"))

                       (:name pomodoro
                              :website "https://github.com/rubbish/pomodoro.el"
                              :description "Run pomodoros"
                              :type github
                              :pkgname "rubbish/pomodoro.el")

                       (:name gh
                              :type github
                              :pkgname "sigma/gh.el"
                              :checkout "7c66958" ;; specific version since the last commit broke gist.el
                              :depends (pcache logito)
                              :description "Github API client libraries"
                              :website "http://github.com/sigma/gh.el")

                       (:name magit ;; more stable magit (from melpa, not from the master branch
                              :website "https://github.com/magit/magit#readme"
                              :description "It's Magit! An Emacs mode for Git."
                              :type elpa
                              :repo ("marmalade" . "http://marmalade-repo.org/packages/"))
                       (:name haskell-mode ;; more stable
                              :website "https://github.com/haskell/haskell-mode"
                              :description "Emacs mode for haskell"
                              :type elpa
                              :repo ("marmalade" . "http://marmalade-repo.org/packages/"))))

(setq mine-pkgs-to-install
      (append
       '(;; extensions
         diminish
         smex
         ag
         switch-window
         scratch
         htmlize
         mark-multiple
         expand-region
         browse-kill-ring
         projectile

         ;; organization/presentations
         deft
         gist
         org-reveal

         ;; langs
         highlight-parentheses
         paredit
         markdown-mode
         ruby-end
         coffee-mode
         yaml-mode
         dockerfile-mode
         clojure-mode cider)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync mine-pkgs-to-install)

(provide 'mine-pkgmgt)
