;; load files under custom/*.el
(setq mine-custom-dir (concat user-emacs-directory "/custom/"))
(defun mine-load-custom-files ()
  (interactive)
  (if (file-exists-p mine-custom-dir)
      (let ((custom-files (directory-files mine-custom-dir t "\.el$")))
        (mapcar 'load-file custom-files))))

(mine-load-custom-files)

(setq custom-file (expand-file-name (concat user-emacs-directory "/custom/customizations.el")))
(if (file-exists-p custom-file)
    (load custom-file))


(provide 'mine-load-custom)
