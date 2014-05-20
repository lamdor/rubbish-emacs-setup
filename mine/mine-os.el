(require 'cl)

(case system-type
  ('darwin (require 'mine-macosx))
  ('gnu/linux (require 'mine-linux)))

(provide 'mine-os)
