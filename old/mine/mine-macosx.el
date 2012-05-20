(global-set-key (kbd "<f1>") 'ns-toggle-fullscreen)
(setq ns-command-modifier (quote meta))

(add-hook 'server-visit-hook '(lambda ()
                                (when window-system
                                 (shell-command "osascript -e 'tell application \"Emacs\" to activate' &" nil)
                                 (delete-other-windows))))

(setq mine-normal-font "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-utf8-1")
(setq mine-big-font "-apple-Monaco-medium-normal-normal-*-20-*-*-*-m-0-utf8-1")

(provide 'mine-macosx)
