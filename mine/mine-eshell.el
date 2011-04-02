(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-x m") 'switch-to-other-buffer)))

(defun mine-cwd-ps1 ()
  (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd)))

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
              ")" ))))

(defun mine-time-ps1 ()
  (format-time-string "[%H:%M:%S]" (current-time)))

(setq eshell-prompt-function
      (lambda ()
        (concat
         ;; (mine-time-ps1)
         ;; " "
         (mine-cwd-ps1)
         ;; " "
         (mine-git-ps1)
         ;; "\n"
         " $ ")))

(setq eshell-prompt-regexp "^\\(->\\|>\\|mysql>\\|irb(main):.+[>*]\\) ")

(defun eshell/cdc (&optional project)
  (eshell/cd (concat "~/code/" project)))

(defun eshell/ack (search-string)
  (ack (concat ack-command " -- " search-string)))

(defun eshell/rake (&optional task)
  (rinari-rake task))

(defun eshell/jrake (&optional task)
  (rinari-jrake task))

;; bookmarking
(defun pcomplete/eshell-mode/bmk ()
  "Completion for `bmk'"
  (pcomplete-here (bookmark-all-names)))

(defun eshell/bmk (&rest args)
  "Integration between EShell and bookmarks.
For usage, execute without arguments."
  (setq args (eshell-flatten-list args))
  (let ((bookmark (car args))
        filename name)
    (cond
     ((eq nil args)
      (format "Usage: bmk BOOKMARK to change directory pointed to by BOOKMARK
    or bmk . BOOKMARK to bookmark current directory in BOOKMARK.
Completion is available."))
     ((string= "." bookmark)
      ;; Store current path in EShell as a bookmark
      (if (setq name (car (cdr args)))
          (progn
            (bookmark-set name)
            (bookmark-set-filename name (eshell/pwd))
            (format "Saved current directory in bookmark %s" name))
        (error "You must enter a bookmark name")))
     (t
       ;; Assume the user wants to go to the path pointed out by a bookmark.
       (if (setq filename (cdr (car (bookmark-get-bookmark-record bookmark))))
           (if (file-directory-p filename)
               (eshell/cd filename)
             ;; TODO: Handle this better and offer to go to directory
             ;; where the file is located.
             (error "Bookmark %s points to %s which is not a directory"
                    bookmark filename))
         (error "%s is not a bookmark" bookmark))))))

(provide 'mine-eshell)
