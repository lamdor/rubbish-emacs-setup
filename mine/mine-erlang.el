;; Erlang Support
(add-path "site-lisp/erlang/")
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

(provide 'mine-erlang)