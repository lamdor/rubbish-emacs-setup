;; These are assorted tools that I use while working on projects

(defun ido-find-file-in-tag-files()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (ido-completing-read "Project Files: "
			  (tags-table-files)
			  nil t))))

(defun ido-read-tag (prompt &optional require-match initial-input)
  (let ((enable-recursive-minibuffers t))
    (visit-tags-table-buffer)
    (ido-completing-read prompt
			 (let ((accum (list)))
			   (mapatoms (lambda (arg) (push (symbol-name arg) accum))
				     (tags-completion-table))
			   accum)
			 nil require-match initial-input)))

(defun ido-find-tag ()
  (interactive)
  (let ((default (symbol-at-point)))
     (find-tag (ido-read-tag (format "Find tag: " default) nil
			 (and default (symbol-name default))))))

(provide 'mine-projects)