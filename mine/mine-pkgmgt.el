(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max)) (eval-print-last-sexp))))

(setq el-get-user-package-directory "~/.emacs.d/init")

(setq el-get-sources '((:name enclose
                              :description "Enclose cursor within punctuation pairs"
                              :type elpa
                              :autoloads nil
                              :prepare (progn
                                         (autoload 'enclose-global-mode "enclose" nil t)
                                         (autoload 'enclose-mode "enclose" nil t)))
                       (:name zen-and-art-theme
                              :description "A port of the zen-and-art color theme using the new deftheme format."
                              :type elpa
                              :repo ("melpa" . "http://melpa.milkbox.net/packages/")
                              :post-init (progn
                                           (add-to-list 'custom-theme-load-path default-directory)))
                       (:name restclient
                              :website "https://github.com/pashky/restclient.el"
                              :description "HTTP REST client tool for emacs"
                              :type github
                              :pkgname "pashky/restclient.el"
                              :prepare (progn
                                         (autoload 'restclient-mode "restclient" nil t)
                                         (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))))
                       (:name sbt
                              :website "https://github.com/rubbish/sbt.el"
                              :description "support for running sbt in inferior mode."
                              :type github
                              :pkgname "rubbish/sbt.el"
                              :prepare (add-hook 'scala-mode-hook 'turn-on-sbt-mode))
                       (:name rcirc-color
                              :website "http://emacswiki.org/emacs/rcirc-color.el"
                              :description "color nicks in rcirc"
                              :type github
                              :pkgname "emacsmirror/rcirc-color"
                              :features (rcirc-color))
                       (:name rcirc-auto-away
                              :website "http://www.emacswiki.org/emacs/rcircAutoAway"
                              :type emacswiki)
                       (:name rcirc-notify
                              :website "https://github.com/nicferrier/rcirc-notify"
                              :type elpa
                              :repo ("marmalade" . "http://marmalade-repo.org/packages/")
                              :after (add-hook 'rcirc-mode-hook 'rcirc-notify-add-hooks))
                       (:name evernote
                              :website "http://code.google.com/p/emacs-evernote-mode"
                              :description "Functions for editing Evernote notes directly from Emacs"
                              :type github
                              :pkgname "rubbish/evernote-mode"
                              :prepare (progn
                                         (autoload 'evernote-login "evernote-mode" "Login to Evernote." t)
                                         (autoload 'evernote-create-note "evernote-mode" "Create a note from scratch." t)
                                         (autoload 'evernote-open-note "evernote-mode" "Read an existing note to an Emacs buffer." t)
                                         (autoload 'evernote-search-notes "evernote-mode" "Search notes by query in the minibuffer." t)
                                         (autoload 'evernote-do-saved-search "evernote-mode" "Search notes by using a Saved Search." t)
                                         (autoload 'evernote-write-note "evernote-mode" "Create a new note from the current buffer." t)
                                         (autoload 'evernote-post-region "evernote-mode" "Create a new note containing the selected region." t)
                                         (autoload 'evernote-browser "evernote-mode" "Open Evernote Browser." t)))))

(setq mine-pkgs-to-install
      (append
       '(;; lisp
         highlight-parentheses
         paredit

         ;; scala
         scala-mode
         ;; ensime

         haskell-mode

         ;; ruby
         rvm

         ;; markdown
         markdown-mode

         ;; organization
         deft
         todotxt

         ;; text editting
         undo-tree
         wrap-region
         mark-multiple
         expand-region
         yasnippet

         ;; commands
         smex
         full-ack
         switch-window
         scratch
         gist
         magit
         htmlize)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync mine-pkgs-to-install)

(provide 'mine-pkgmgt)
