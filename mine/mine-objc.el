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



(defun mine-objc-choose-header-mode ()
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m")))
        (if (file-exists-p dot-m-file)
            (objc-mode)
          ))))
(add-hook 'find-file-hook 'mine-objc-choose-header-mode)

(defun mine-objc-toggle-header-and-source nil
  "Toggle between source and header files"
  (interactive)
  (let ((fname buffer-file-name) oname)
    (setq oname
          (cond
           ((string-match "\\.h$" fname) (replace-match ".m" nil nil fname))
           ((string-match "\\.m$" fname) (replace-match ".h" nil nil fname))
           (t fname)))
    (find-file oname)))

(defun xcode:build-and-run ()
 (interactive)
 (do-applescript
  "Tell application \"Xcode\" to Activate
Tell application \"System Events\"
     Tell Process \"Xcode\"
          Key code 36 using (Command down)
     End Tell
End Tell"))

(defun xcode:build ()
 (interactive)
 (do-applescript
  "Tell application \"Xcode\" to Activate
Tell application \"System Events\"
     Tell Process \"Xcode\"
          Key code 11 using (Command down)
     End Tell
End Tell"))

(add-hook 'objc-mode-hook '(lambda ()
                             (c-set-style "mine-obj-c")
                             (c-subword-mode t)
                             (local-set-key (kbd "C-x C-v") 'mine-objc-toggle-header-and-source)
                             (local-set-key (kbd "C-c ;") 'xcode:build)
                             (local-set-key (kbd "C-c '") 'xcode:build-and-run)))
(add-hook 'objc-mode-hook 'turn-on-wrap-region-mode)
(add-hook 'objc-mode-hook 'turn-on-enclose-mode)

(provide 'mine-objc)
