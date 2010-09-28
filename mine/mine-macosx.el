(setq ns-command-modifier (quote meta))

(add-hook 'server-visit-hook '(lambda ()
                                (when window-system
                                 (shell-command "osascript -e 'tell application \"Emacs\" to activate' &" nil)
                                 (delete-other-windows))))

(provide 'mine-macosx)
