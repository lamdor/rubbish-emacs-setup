(setq evernote-username "lamdor")

(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))

(global-set-key (kbd "C-c e c") 'evernote-create-note)
(global-set-key (kbd "C-c e o") 'evernote-open-note)
(global-set-key (kbd "C-c e s") 'evernote-search-notes)
(global-set-key (kbd "C-c e S") 'evernote-do-saved-search)
(global-set-key (kbd "C-c e w") 'evernote-write-note)
(global-set-key (kbd "C-c e p") 'evernote-post-region)
(global-set-key (kbd "C-c e b") 'evernote-browser)
