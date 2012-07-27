(add-hook 'scala-mode-hook 'turn-on-sbt-mode)

(add-hook 'magit-status-mode-hook '(lambda ()
                                     (if (sbt-find-path-to-project)
                                         (turn-on-sbt-mode))))

(add-hook 'dired-mode-hook '(lambda ()
                              (if (sbt-find-path-to-project)
                                  (turn-on-sbt-mode))))

(add-hook 'conf-unix-mode-hook '(lambda ()
                                  (if (sbt-find-path-to-project)
                                      (turn-on-sbt-mode))))
