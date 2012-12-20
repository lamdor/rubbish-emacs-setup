(set-face-background 'mode-line "grey80")
(set-face-background 'powerline-active1 "grey22")
(set-face-background 'powerline-active2 "grey40")
(set-face-background 'powerline-inactive1 "grey11")
(set-face-background 'powerline-inactive2 "grey20")

(setq-default mode-line-format
      '("%e"
        (:eval
         (let* ((active (powerline-selected-window-active))
                (face1 (if active 'powerline-active1
                         'powerline-inactive1))
                (face2 (if active 'powerline-active2
                         'powerline-inactive2))
                (lhs (list
                      (powerline-raw "%*" nil 'l)
;;                      (powerline-buffer-size nil 'l)
                      (powerline-buffer-id nil 'l)

                      (powerline-raw " ")
                      (powerline-arrow-right nil face1)

                      (when (boundp 'erc-modified-channels-object)
                        (powerline-raw erc-modified-channels-object
                                       face1 'l))

                      (powerline-major-mode face1 'l)
                      (powerline-process face1)
                      (powerline-minor-modes face1 'l)
                      (powerline-narrow face1 'l)

                      (powerline-raw " " face1)
                      (powerline-arrow-right face1 face2)

                      (powerline-vc face2)))
                (rhs (list
                      (powerline-raw global-mode-string face2 'r)

                      (powerline-arrow-left face2 face1)

                      (powerline-raw "%4l" face1 'r)
                      (powerline-raw ":" face1)
                      (powerline-raw "%3c" face1 'r)

                      (powerline-arrow-left face1 nil)
                      (powerline-raw " ")

                      (powerline-raw "%6p" nil 'r)

                      (powerline-hud face2 face1))))
           (concat
            (powerline-render lhs)
            (powerline-fill face2 (powerline-width rhs))
            (powerline-render rhs))))))


