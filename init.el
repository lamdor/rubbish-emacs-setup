;; el-get setup
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") 

(unless (require 'el-get nil t) 
  (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") (goto-char (point-max)) (eval-print-last-sexp)))

(el-get 'sync)


 ;; local sources 
;; (setq el-get-sources 
;;       '(
;; 	(:name magit :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
;; 	(:name asciidoc :type elpa :after (lambda () (autoload 'doc-mode "doc-mode" nil t) (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode)) (add-hook 'doc-mode-hook '(lambda () (turn-on-auto-fill) (require 'asciidoc)))))
;; 	(:name lisppaste :type elpa) (:name emacs-goodies-el :type apt-get)))

;;  (setq my-packages (append '(el-get switch-window vkill google-maps nxhtml xcscope yasnippet) (mapcar 'el-get-source-name el-get-sources))) 

;; (el-get 'sync my-packages)
