(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-x m") 'switch-to-other-buffer)))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd))
         "\n"
         "-> ")))

(setq eshell-prompt-regexp "^\\(->\\|mysql>\\|irb(main):.+[>*]\\) ")

(defun eshell/cdc (&rest project)
  (eshell/cd (concat "~/code/" (car project))))

(defun eshell/ack (search-string)
  (ack (concat ack-command " -- " search-string)))

(provide 'mine-eshell)
