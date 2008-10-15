(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy Code")

(autoload 'run-groovy "inf-groovy" "Runs a inferior groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Sets local def keys for inf-groovy")

(add-hook 'groovy-mode-hook '(lambda() (inf-groovy-keys)))

(add-hook 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(setq groovy-home "/usr/local/groovy/current/")