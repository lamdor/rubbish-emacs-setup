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