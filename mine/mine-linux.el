(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(setq ack-executable "ack-grep")

(provide 'mine-linux)
