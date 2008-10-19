(defun beginning-of-line-or-back-to-indention ()
  (interactive)
  "This goes to back to indention or if already there beginning of line"
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
	(beginning-of-line))))

(defun kill-to-end-or-join ()
  (interactive)
  (if (equal (point) (point-at-eol))
      (save-excursion
	(next-line)
	(delete-indentation)
      )
    (kill-line)))
