;;;  SCCS: @(#)89/11/20 qpdelete.el    2.2
;;;		    Quintus Prolog - GNU Emacs Interface
;;;                         Support Functions
;;;
;;;	                   Sitaram Muralidhar
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
;;;
;;; This interface was made possible by contributions from Fernando
;;; Pereira and various customers of Quintus Computer Systems, Inc.,
;;; based on code for Quintus's Unipress Emacs interface.

;;  The functions in this file are:
;;
;   1) qp-backward-kill-character
;   2) qp-backward-kill-word
;   3) qp-kill-character
;   4) qp-kill-word
;   5) qp-kill-lines
;   6) qp-backward-kill-sentence
;   7) qp-kill-sentence
;   8) qp-kill-region
;;  The only one not being handled is ESC C-k - kill sexp,
;;  and is the only key sequence that can delete the prolog prompt.
;;

;---------------------------------------------------------------------------

(defun qp-backward-kill-character (arg)
  (interactive "p")
  (set-mark (point))
  (error-occurred (backward-char arg))
  (qp-kill-region)
)

;---------------------------------------------------------------------------

(defun qp-backward-kill-word (arg)
  (interactive "p")
  (set-mark (point))
  (error-occurred (forward-word (- arg)))
  (qp-kill-region)
)


;---------------------------------------------------------------------------

(defun qp-kill-character (arg)
  (interactive "p")
  (cond ((eobp)
	 (message "Use ^C^D to send an end-of-file signal"))
	(t (set-mark (point))
	   (error-occurred (forward-char arg))
	   (qp-kill-region))
  )
)

;---------------------------------------------------------------------------

(defun qp-kill-word (arg)
  (interactive "p")
  (set-mark (point))
  (error-occurred (forward-word arg))
  (qp-kill-region)
)

;---------------------------------------------------------------------------

(defun qp-kill-lines (arg)
  (interactive "p")
  (set-mark (point))
  (error-occurred (forward-line arg))
  (qp-kill-region)
)

;---------------------------------------------------------------------------

(defun qp-bacward-kill-sentence (arg)
  (interactive "p")
  (set-mark (point))
  (error-occurred (backward-sentence arg))
  (qp-kill-region)
)

;---------------------------------------------------------------------------

(defun qp-kill-sentence (arg)
  (interactive "p")
  (set-mark (point))
  (error-occurred (forward-sentence arg))
  (qp-kill-region)
)


;---------------------------------------------------------------------------
; returns the character position of the newline immediately before the
; current Prolog prompt

(defun &qpfind-ln-start ()
  (save-excursion		
    (goto-char 
      (process-mark (get-buffer-process "*prolog*")))
    (beginning-of-line)
;    (backward-char)
    (setq ans (dot))
  )
  ans
)

;---------------------------------------------------------------------------

(defun qp-kill-region ()
  (interactive)
  (let ((prompt-start (&qpfind-ln-start)))
    (cond ((or (and (>= (dot)
			(process-mark (get-buffer-process "*prolog*")))
		    (>= (mark) 
			(process-mark (get-buffer-process "*prolog*")))
	       )
	       (and (<= (dot) prompt-start)
		    (<= (mark) prompt-start)
	       )
	   )
           (kill-region (mark) (point)))
	  (t (message "Cannot delete the Prolog prompt")
	     (goto-char 
	       (process-mark (get-buffer-process "*prolog*"))))
    )
   )
)
