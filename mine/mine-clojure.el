;; Clojure
(add-path "site-lisp/clojure-mode")
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))

;; clojure-test-mode
;;(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;;(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;;(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

(defun lein-swank ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Not in a Leiningen project."))
    ;; you can customize slime-port using .dir-locals.el
    (shell-command (format "cd %s && lein swank %s &" root slime-port)
                   "*lein-swank*")
    (set-process-filter (get-buffer-process "*lein-swank*")
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" slime-port)
                            (set-process-filter process nil))))
    (message "Starting swank server...")))

(provide 'mine-clojure)
