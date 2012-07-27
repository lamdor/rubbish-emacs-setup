(add-hook 'dired-mode-hook '(lambda ()
                              (if (locate-dominating-file default-directory "Rakefile")
                                  (rinari-minor-mode t))))
