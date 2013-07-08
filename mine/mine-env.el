(setenv "PATH" (concat (getenv "HOME") "/bin" ":"
                       "/usr/local/bin" ":"
                       "/usr/bin" ":"
                       "/bin" ":"
                       "/usr/sbin" ":"
                       "/sbin" ":"))

(setq exec-path (list (concat (getenv "HOME") "/bin")
                      "/usr/local/bin"
                      "/usr/bin"
                      "/bin"
                      "/usr/sbin"
                      "/sbin"))

(setenv "EDITOR" "emacsclient")

(provide 'mine-env)
