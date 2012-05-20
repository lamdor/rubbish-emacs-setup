;; balanced.el
;;
;; Time-stamp: <97/06/24 17:45:04 ehilsdal>
;; (update this with M-x time-stamp)

;; Provides a modifier to major modes such that ``parenthesis''-type
;; characters usually stay balanced.  See the documentation-string for
;; ``balanced-on'' for details.  After putting this file somewhere in
;; your load-path, A not-bad thing to do with this package is:

;; (require 'balanced)
;; (add-hook 'scheme-mode-hook 'balanced-on)

;; ------------------------------

;; This file provides the package named ``balanced''.  When
;; compiled, if it contains macros, it should require itself to be
;; loaded before the program is compiled, thus the following require
;; statement.

(provide 'balanced)
(require 'balanced)

;; I want a menu-bar option for toggling balanced mode.

(global-set-key [menu-bar tools balance]
		'("Auto-Balance Parens" . balanced-toggle))

;; I can't believe for-each isn't defined in emacs lisp.

(defun for-each (f ls)
  (if (null ls)
      nil
    (funcall f (car ls))
    (for-each f (cdr ls))))

;; balanced-modes is an association-list of major-mode names to lists
;; of (key binding) pairs.  It is used both to check whether we have
;; reset the keys for a particular mode, as well as used for unsetting
;; the key modifications.

(defvar balanced-modes '()
  "*The table of balanced major modes.  Each entry is of the form

	(major-mode-name (key old-binding) ...)

and is used to reset the major mode when balancing is turned off.")

;; scans through the keymap and finds all the
;; goodies, then sets up the proper keys.

(defun balanced-on ()
  "Changes a number of key bindings in the current major mode:

  * ``open parenthesis''-type keys (the exact keys depend on the major
    mode) insert both opening and closing characters, and ``close
    parenthesis''-type keys simply move to the nearest closing
    character (not necessarily matching the key typed).  
  * Meta-``open'' and ``close'' are rebound to simply insert the
    corresponding character.
  * ``delete-left''-type keys skip over ``close parenthesis''-type
    characters and only delete them when they can delete a matching
    ``open parenthesis''-type character (a delete can be forced by
    giving the ``delete-left''-type key an argument -- i.e. with
    ctrl-u or something).

If the ``open parenthesis''-type keys are given a numeric argument,
they will wrap their open and close around that many items.  For
example, in lisp modes, if the point is at -!- in:  

      (if (null? x) -!-(printf 3) hi)

then ``Esc 2 ('' will result in

      (if (null? x) (-!-(printf 3) hi))"
  (interactive)
  (let ((already-seen (assq major-mode balanced-modes)))
    (and (not already-seen)
	 (let* ((acc '())
		(localmap (current-local-map))
		(backs 
		 (append 
		  (where-is-internal 'backward-delete-char-untabify localmap)
		  (where-is-internal 'backward-delete-char localmap)
		  (where-is-internal 'delete-backward-char localmap)))
		(index 0))
	   (while (< index 256)
	     (let ((syn (char-syntax index)))
	       (and 
		(or (= syn ?\() (= syn ?\)))
		(let* ((mainkey (format "%c" index))
		       (metakey (vector (list 'meta index)))
		       (mainkeyb (local-key-binding mainkey))
		       (metakeyb (local-key-binding metakey)))
		  (local-set-key mainkey 
				 (if (= syn ?\()
				     'balanced-open
				   'balanced-close))
		  (local-set-key metakey 'self-insert-unmeta)
		  (setq acc 
			(cons (list mainkey mainkeyb)
			      (cons (list metakey metakeyb)
				    acc))))))
	     (setq index (+ index 1)))
	   (for-each 
	    (function
	     (lambda (key)
		(let ((keyb (local-key-binding key)))
		  (local-set-key key 'balanced-delete-char)
		  (setq acc (cons (list key keyb) acc)))))
	    backs)
	   (setq balanced-modes
		 (cons (cons major-mode acc) balanced-modes))))))

(defun balanced-off ()
  "Restores major modes which were previously made ``balanced'' 
by the command ``balanced-on'' to its previous ``unbalanced'' 
state"
  (interactive)
  (let ((thing (assq major-mode balanced-modes)))
    (and thing
	 (progn
	   (for-each (function
		      (lambda (x) (local-set-key (car x) (car (cdr x)))))
		     (cdr thing))
	   (setq balanced-modes (delq thing balanced-modes))))))

(defun balanced-toggle ()
  "Toggles ``balanced'' in the current major mode"
  (interactive)
  (let ((thing (assq major-mode balanced-modes)))
    (if thing
	(balanced-off)
      (balanced-on))))

;; Lastly, a few functions needed to be defined.

(if (string-match "XEmacs\\|Lucid" emacs-version)

    (defun balanced-open (arg)
      "Put parens (or whatever) around next ARG sexps."
      (interactive "P")
      (let* ((arg (if arg (prefix-numeric-value arg) 0))
	     (ch last-input-char)
	     (endch (logand (lsh (aref (syntax-table) ch) -8) 255)))
	(if (= (preceding-char) ?\\)
	    (insert ch)
	  (or (zerop arg) (skip-chars-forward " \t"))
	  (insert ch)
	  (save-excursion
	    (or (zerop arg) (forward-sexp arg))
	    (insert endch)))))

  (defun balanced-open (arg)
    "Put parens (or whatever) around next ARG sexps."
    (interactive "P")
    (let* ((arg (if arg (prefix-numeric-value arg) 0))
	   (ch last-input-char)
	   (endch (cdr (aref (syntax-table) ch))))
      (if (= (preceding-char) ?\\)
	  (insert ch)
	(or (zerop arg) (skip-chars-forward " \t"))
	(insert ch)
	(save-excursion
	  (or (zerop arg) (forward-sexp arg))
	  (insert endch)))))
  )

(if (string-match "XEmacs\\|Lucid" emacs-version)
    (defun self-insert-unmeta (arg)
      "Insert the character last typed, stripped of its meta info"
      (interactive "p")
      (let* ((event last-input-event)
	     (ch (logand (event-to-character event t t) 127)))
	(insert ch)))
  (defalias 'self-insert-unmeta (symbol-function 'self-insert-command))
  )

(defun balanced-close ()
  "Just move past the next closing paren, don't reindent."
  (interactive)
  (if (= (preceding-char) ?\\) 
      (insert last-input-char)
    (up-list 1)
    (blink-matching-open)))

(defun balanced-delete-char (arg)
  "Delete a paren pair if we're in the right place, else error.
With an argument, don't error, just delete the paren."
  (interactive "P")
  (cond (arg
	 (backward-delete-char-untabify 1))
	((= (char-syntax (preceding-char)) ?\()
	 (if (not (= (char-syntax (following-char)) ?\)))
	     (error "Can't touch this")
	   (backward-char 1)
	   (delete-char 2)))
	((= (char-syntax (preceding-char)) ?\))
	 (backward-char 1))
	(t (backward-delete-char-untabify 1))))

;; ---- end balanced.el