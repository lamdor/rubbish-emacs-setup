(defun add-cedet-path (p)
  (add-path (concat "cedet/" p)))

;; Speedbar support
(add-cedet-path "speedbar-0.14beta4")
(autoload 'speedbar "speedbar" "Start up a speedbar")

(add-cedet-path "semantic-1.4.4")
(add-cedet-path "eieio-0.17")