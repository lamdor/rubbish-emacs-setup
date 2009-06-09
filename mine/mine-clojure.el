;; Clojure

(autoload 'paredit-mode "paredit" "Minore module for psuedo-structually editting lisp code." t)

(add-path "site-lisp/clojure-mode")
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))

(add-path "site-lisp/slime")
(require 'slime)

(add-path "site-lisp/swank-clojure")
(setq swank-clojure-jar-path "/usr/local/clojure/current/clojure-1.0.0.jar")
(setq swank-clojure-extra-classpaths
      (list "~/code/watch/clojure-contrib/src/"
	    "~/code/learning/clojure/programming-clojure/code"
	    "~/code/learning/clojure/shcloj-code/code"
	    "~/.m2/repository/ant/ant/1.6.5/ant-1.6.5.jar"
	    (concat emacs-root "mine/clojure/")))
(require 'swank-clojure-autoload)

;; from Bill Clementson
;; http://bc.tech.coop/blog/081120.html
(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

(add-hook 'slime-connected-hook (lambda ()
				  (interactive)
				  (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
				  (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)))

(provide 'mine-clojure)