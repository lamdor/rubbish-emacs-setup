;; Maven Functionality

(require 'comint)

(defvar mvn-executable "/usr/local/maven/current/bin/mvn" "Maven 2 Executable")

(defun mvn-find-pom (dir)
  (let ((f (expand-file-name "pom.xml" dir))
	(parent (file-truename (expand-file-name ".." dir))))
    (cond ((string= dir parent) nil)
	  ((file-exists-p f) f)
	  (t (mvn-find-pom parent)))))

(defun mvn-command (&rest commands)
  (set-buffer
   (apply
    'make-comint "mvn"
    mvn-executable nil (append (list "-f" (mvn-find-pom default-directory)) commands)))
  (pop-to-buffer "*mvn*"))

(defun mvn-install ()
  (interactive)
  (mvn-command "install"))

(defun mvn-package ()
  (interactive)
  (mvn-command "package"))

(defun mvn-clean-install ()
  (interactive)
  (mvn-command "clean" "install"))

(defun mvn-test-compile ()
  (interactive)
  (mvn-command "test-compile"))

(defun mvn-keys ()
  (local-set-key (kbd "C-c m i") 'mvn-install)
  (local-set-key (kbd "C-c m c i") 'mvn-clean-install)
  (local-set-key (kbd "C-c m p") 'mvn-package)
  (local-set-key (kbd "C-c m t") 'mvn-test-compile))
