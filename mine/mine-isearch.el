(defun isearch-save-and-exit ()
  "Exit search normally. and save the `search-string' on kill-ring."
  (interactive)
  (isearch-done)
  (isearch-clean-overlays)
  (kill-new isearch-string)
  (pop-to-mark-command))

(define-key isearch-mode-map (kbd "M-w") 'isearch-save-and-exit)

(defun isearch-occur-search-string ()
  (interactive)
  (isearch-done)
  (isearch-clean-overlays)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string
             (regexp-quote isearch-string)))
    (pop-to-mark-command)))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur-search-string)

(provide 'mine-isearch)
