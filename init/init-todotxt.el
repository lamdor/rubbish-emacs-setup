(require 'todotxt)

(setq todotxt-file (expand-file-name "~/notes/todo.txt"))

(global-set-key [f12] 'todotxt)
(global-set-key (kbd "C-c t") 'todotxt)
(global-set-key (kbd "C-c C-t") 'todotxt)
