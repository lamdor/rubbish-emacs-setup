;; Settings for Java

(autoload 'mvn-keys "mvn" "Sets Maven Keys")

(add-hook 'java-mode-hook (lambda () (mvn-keys)))