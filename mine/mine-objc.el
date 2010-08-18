(c-add-style "mine-obj-c"
             '((c-basic-offset . 4)
               (tab-width . 4)
               (c-comment-only-line-offset . 0)
               (indent-tabs-mode . nil)
               (c-hanging-braces-alist . ((defun-open after)
                                          (defun-close before after)
                                          (class-open after)
                                          (class-close before after)
                                          (namespace-open after)
                                          (inline-open after)
                                          (inline-close before after)
                                          (block-open after)
                                          (block-close . c-snug-do-while)
                                          (extern-lang-open after)
                                          (extern-lang-close after)
                                          (statement-case-open after)
                                          (substatement-open after)))
               (c-cleanup-list . (brace-else-brace
                                  brace-elseif-brace
                                  brace-catch-brace
                                  empty-defun-braces
                                  defun-close-semi
                                  list-close-comma
                                  scope-operator))))

(setq objc-mode-hook nil)
(add-hook 'objc-mode-hook '(lambda ()
                             (c-set-style "mine-obj-c")))
(add-hook 'objc-mode-hook 'turn-on-wrap-region-mode)

(provide 'mine-objc)