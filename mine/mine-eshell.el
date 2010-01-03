(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-x m") 'switch-to-other-buffer)))


;; start of git-ps1 prompt info
(defun mine-git-cmd (args)
  (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "git " args))))

(defun mine-gitdir ()
  (if (file-exists-p ".git")
      ".git"
    (let ((git-dir-output (mine-git-cmd "rev-parse --git-dir")))
      (if (string-match-p "^fatal" git-dir-output)
          nil
        git-dir-output))))

(defun mine-git-cmd-non-empty-p (args)
  (not (equal ""
              (mine-git-cmd args))))

(defun mine-git-ps1 ()
  (when-let (git-dir (mine-gitdir))
    (let ((branch-name (replace-regexp-in-string "refs/heads/" "" (mine-git-cmd "symbolic-ref HEAD")))
          (merging (file-exists-p (concat git-dir "/MERGE_HEAD")))
          (bisecting (file-exists-p (concat git-dir "/BISECT_LOG")))
          (rebase-i (file-exists-p (concat git-dir "/rebase-merge/interactive")))
          (rebase-m (file-exists-p (concat git-dir "/rebase-merge/head-name")))
          (untracked (mine-git-cmd-non-empty-p "ls-files --others --exclude-standard"))
          (staged (mine-git-cmd-non-empty-p "diff-index --cached --ignore-submodules HEAD"))
          (unstaged (mine-git-cmd-non-empty-p "diff --no-ext-diff --ignore-submodules")))
      (concat "("
              branch-name
              (and merging "|MERGING")
              (and bisecting "|BISECTING")
              (and rebase-i "|REBASE-i")
              (and rebase-m "|REBASE-m")
              (and staged "+")
              (and unstaged "*")
              (and untracked "%")
              ")"))))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd))
         " "
         (mine-git-ps1)
         " "
         "\n"
         "-> ")))

(setq eshell-prompt-regexp "^\\(->\\|mysql>\\|irb(main):.+[>*]\\) ")

(defun eshell/cdc (&rest project)
  (eshell/cd (concat "~/code/" (car project))))

(defun eshell/ack (search-string)
  (ack (concat ack-command " -- " search-string)))

(defun eshell/rake (task)
  (rinari-rake task))

(provide 'mine-eshell)
