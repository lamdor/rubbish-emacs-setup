;; Html and XML related setup

(autoload 'haml-mode "haml-mode" "Haml mode" t)
(add-to-list 'auto-mode-alist '("\\.haml" . haml-mode))

;; nxml-mode
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
(setq nxml-slash-auto-complete-flag t)

;; nxhtml

(load-file (concat emacs-root "site-lisp/nxhtml-2.08-100425/autostart.el"))
;; nxhtml-global-minor-mode t
(setq mumamo-chunk-coloring 2
      nxhtml-skip-welcome t
      rng-nxml-auto-validate-flag nil)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-nxhtml-mumamo-mode))


(provide 'mine-html-xml)