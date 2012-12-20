(setenv "PATH" (concat (getenv "HOME") "/bin" ":"
                       "/usr/local/bin" ":"
                       "/usr/bin" ":"
                       "/bin" ":"
                       "/sbin" ":"))

(setq exec-path (list (concat (getenv "HOME") "/bin")
                      "/usr/local/bin"
                      "/usr/bin"
                      "/bin"
                      "/sbin"))

(setenv "EDITOR" "emacsclient")

(provide 'mine-env)
