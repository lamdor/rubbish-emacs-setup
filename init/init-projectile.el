(projectile-global-mode)

(eval-after-load 'projectile '(diminish 'projectile-mode))

;; switch everything to ag (not ack
(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)


