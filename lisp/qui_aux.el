; /ports/emacs/GNU/el3.1 @(#)qui_aux.el	1.1 11/15/90 
;;;  /ports/home/sitaram/Gnu/qui_emacs/qui_aux.el @(#)qui_aux.el	1.2 11/13/90 
;;;		    Quintus Prolog - GNU Emacs Interface
;;;                         Support Functions
;;;
;;;	            Consolidated by Sitaram Muralidhar
;;;
;;;		           sitaram@quintus.com
;;;		      Quintus Computer Systems, Inc.
;;;			      2 May 1989	   
;;;
;;; This file defines functions that support the Quintus Prolog - GNU Emacs
;;; interface.
;;;
;;;			       Acknowledgements
;;;
;;; This interface was made possible by contributions from Fernando
;;; Pereira and various customers of Quintus Computer Systems, Inc.,
;;; based on code for Quintus's Unipress Emacs interface.
;;; 

;----------------------------------------------------------------------------
; The following routines are used to properly handle message display
;  for the routines which talk to the Emacs interface from Prolog

(defvar *qpmess-buffer* " ")

(defun &qp-message (message)
  (setq *qpmess-buffer* message)
    )

(defun display-any-messages ()
  (if (not (string-equal *qpmess-buffer* ""))
      (message *qpmess-buffer*)
    )
  (sit-for 0)
  (&clear-message)
  )

(defun &clear-message ()
	(setq *qpmess-buffer* "")
    )

(defun &no-message ()
  (setq *qpmess-buffer* "")
  )

(defmacro first-line ()
  (save-excursion (beginning-of-line) (bobp)))

(defmacro last-line ()
  (save-excursion (end-of-line) (eobp)))

(defun skip-prolog-comment (range)
  (let ((current-location (point)))
  (if (save-excursion 
	(beginning-of-line)
	(search-forward "%" current-location t))
      (progn (skip-prolog-%-comment range) t)
    (not (skip-prolog-/*-*/-comment range)))))

(defun skip-prolog-%-comment (range)
  "Skip to the beginning or end of a prolog comment depending
on if the range is before or after the point in"
  (let* ((forward (> (point) range))
	 (line-skip (if forward -1 1))
	 (in-comment t))
    (while (and in-comment (not (bobp)) (not (eobp)))
      (previous-line line-skip)
      (beginning-of-line)
      (setq in-comment (= (following-char) ?%)))))

(defun skip-prolog-/*-*/-comment (range)
"Skip to the beginning or end of a prolog comment depending
on if the range is before or after the point in"
  (let* ((current-point (point))
         (forward (> current-point range)))
      (if forward
	  (if (save-excursion (search-backward "\/*" range t))
	      (not (search-forward "*\/"))
	    t)
	(if (save-excursion (search-forward "*\/" range t))
	    (not (search-backward "\/*"))
	  t))))


(defun beginning-of-clause (&optional arg)
  "Move backward to next beginning-of-clause.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (let ((clause-point (point)) (not-done t) (command-point (point)))
    (while (and not-done (not (bobp)) (not (eobp)))
      (if (and arg (< arg 0))
      (skip-chars-forward " \t\n")
      (skip-chars-backward " \t\n"))
      (if (re-search-backward "^\\S-" nil 'move (or arg 1))
	  (if (= (following-char) ?%)
	      (skip-prolog-%-comment clause-point)
	    (setq not-done (not (skip-prolog-/*-*/-comment clause-point)))))
      (setq clause-point (point)))
    )
  )

(defun end-of-clause (&optional arg)
  "Move forward to next end of prolog clause.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (let ((clause-point (point)) (not-done t) (command-point (point)))
    (while (and not-done (not (bobp)) (not (eobp)))
      (re-search-forward "[^.]\\.\\(\\s-\\)*$" nil 'move (or arg 1))
      (setq not-done (skip-prolog-comment clause-point))
      (setq clause-point (point)))
    (if not-done (progn (goto-char command-point) (beep)))))

(defun mark-clause ()
  (interactive)
  (end-of-clause)
  (set-mark (point))
  (beginning-of-clause)
  (message "Clause marked")
)

(defun kill-clause ()
"Kill the prolog clause that the point in currently in"
  (interactive)
  (mark-clause)
  (kill-region (point) (mark)))
