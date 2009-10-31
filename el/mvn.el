;; Maven Functionality

(require 'comint)

(defvar mvn-executable "mvn" "Maven 2 Executable")
(defvar mvn-buffer-name "*mvn*")

(defun mvn-find-pom-directory (&optional dir)
  (let ((dir (or dir "."))
        (f (expand-file-name "pom.xml" dir))
	(parent (file-truename (expand-file-name ".." dir))))
    (cond ((string= dir parent) nil)
	  ((file-exists-p f) dir)
	  (t (mvn-find-pom-directory parent)))))

(defun mvn-shell (command)
  (let ((buffer (shell mvn-buffer-name)))
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
    (set (make-local-variable 'comint-output-filter-functions)
         '(ansi-color-process-output
           comint-postoutput-scroll-to-bottom))
    (compilation-shell-minor-mode t)
    (mvn-keys)
    (comint-send-string buffer (concat "cd " (mvn-find-pom-directory) "\n"
                                       command "\n"))))

(defun mvn-command (&rest commands)
  (mvn-shell (apply 'concat
                    (append (list "mvn ") commands))))

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

(defun mvn-switch ()
  (let ((current-buffer-name (buffer-name (current-buffer))))
    (if (eq current-buffer mvn-buffer-name)
        (switch-to-buffer (other-buffer))
      (mvn-shell "ls"))))

(defun mvn-keys ()
  (interactive)
  (local-set-key (kbd "C-c m i") 'mvn-install)
  (local-set-key (kbd "C-c m c i") 'mvn-clean-install)
  (local-set-key (kbd "C-c m p") 'mvn-package)
  (local-set-key (kbd "C-c m t") 'mvn-test-compile)
  (local-set-key (kbd "C-c m m") 'mvn-switch))

(provide 'mvn)
