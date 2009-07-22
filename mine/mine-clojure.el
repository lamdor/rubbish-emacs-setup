;; Clojure

(autoload 'paredit-mode "paredit" "Minor module for psuedo-structually editting lisp code." t)

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

;; taken from http://github.com/technomancy/emacs-starter-kit/blob/a46328f2ad0cb437bf22aaf42f4f25f53126102f/starter-kit-lisp.el#L104
(defun clojure-project (path)
  "Setup classpaths for a clojure project and starts a new SLIME session.

Kills existing SLIME session, if any."
  (interactive (list
                (ido-read-directory-name
                 "Project root: "
                 (locate-dominating-file default-directory "pom.xml"))))
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (add-to-list 'swank-clojure-extra-vm-args
               (format "-Dclojure.compile.path=%s"
                       (expand-file-name "target/classes/" path)))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (expand-file-name "target/dependency/" path)
        swank-clojure-extra-classpaths
        (append (mapcar (lambda (d) (expand-file-name d path))
                        '("src/" "target/classes/" "test/"))
                (let ((lib (expand-file-name "lib" path)))
                  (if (file-exists-p lib)
                      (directory-files lib t ".jar$"))))
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (save-window-excursion
    (slime)))


;; clojure-test-mode
(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

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