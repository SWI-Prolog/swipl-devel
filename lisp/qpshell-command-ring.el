;;; SCCS: @(#)89/11/20 qpshell-command-ring.el    1.2
;;; Source not known
;;;

(provide 'shell-command-ring)

(defvar command-ring nil
  "List of previously typed shell commands.")

(defconst command-ring-max 30
  "*Maximum length of kill ring before oldest elements are thrown away.")

(defvar command-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(fset 'command-ring-save 'save-command-in-ring)

(defun save-command-in-ring (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (let ((command-string (buffer-substring beg end)))
    (if (and (> (length command-string) 2)
             (not (string-equal command-string (car command-ring))))
        (progn
          (setq command-ring (cons command-string command-ring))
          (if (> (length command-ring) command-ring-max)
              (setcdr (nthcdr (1- command-ring-max) command-ring) nil))
          (setq this-command 'save-command-in-ring)
          (setq command-ring-yank-pointer command-ring)))))

(defun rotate-command-yank-pointer (arg)
  "Rotate the yanking point in the command ring."
  (interactive "p")
  (let ((length (length command-ring)))
    (if (zerop length)
	(error "Command ring is empty")
      (setq command-ring-yank-pointer
	    (nthcdr (% (+ arg (- length (length command-ring-yank-pointer)))
		       length)
		    command-ring)))))

(defun command-pop (arg)
  "Replace just-yanked shell input with a different input.
This command is allowed only immediately after a  yank  or a  yank-pop.
At such a time, the region contains a stretch of reinserted
previously-killed text.  yank-pop  deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'copy-last-shell-input))
      (error "Previous command was not a yank"))
  (setq this-command 'copy-last-shell-input)
  (let ((before (< (point) (mark))))
    (delete-region (point) (mark))
    (rotate-command-yank-pointer arg)
    (set-mark (point))
    (insert (car command-ring-yank-pointer))
    (delete-char -1)
    (if before (exchange-point-and-mark))))

(defun copy-last-shell-input (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.
With just C-U as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  (rotate-command-yank-pointer (if (listp arg) 0
			 (if (eq arg '-) -1
			   (1- arg))))
  (push-mark (point))
  (insert (car command-ring-yank-pointer))
  (delete-char -1)
  (if (consp arg)
      (exchange-point-and-mark)))


