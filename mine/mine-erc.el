(setq erc-nick "rubbish")
(setq erc-prompt-for-password nil)
(setq erc-server "irc.freenode.net")
(setq erc-user-full-name "Luke Amdor")

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#ruby" "#jruby" "#rubyonrails" "#clojure" "#scala")))

(setq erc-interpret-mirc-color t)

(setq erc-modules
      '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track))

(provide 'mine-erc)
