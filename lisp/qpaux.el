;;; SCCS: @(#)89/11/20 qpaux.el    2.2
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



(qprequire 'mlsupport)

(ml-defun
    (random-time-number  tme ln
	(setq tme (current-numeric-time))
	(setq ln (length tme))
	(ml-substr tme (- ln 3) ln)))


; ----------------------------------------------------------------------
; The "$scratch-file" is used for communication with Prolog.  It should 
; probably have a more random name to minimize collisions with other
; users with the same home directory
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------


(ml-defun (crlf (insert 10)))


(defun agglutinating-charp  (char)
  (or (= char 36) (= char 38) (= char 42) (= char 43) (= char 45) ; |
      (= char 46) (= char 47) (= char 58) (= char 60) (= char 61)
      (= char 62) (= char 63) (= char 64) (= char 92) (= char 94)
      (= char 96) (= char 126) (= char 35)))

(defun layout-char  (char)
  (or (and (>= char 0)
          (<= char 32)
       )
       (= char 127)
    )
)

(ml-defun
    (dotimes n
	(setq n (ml-arg 1))
	(while (not (zerop (> n 0)))
	       (setq n (- n 1))
	       (ml-arg 2))))


(ml-defun (bell-message
	   (ml-message (ml-arg 1))
	   (bell)))

;----------------------------------------------------------------------------
; This routine will wink the cursor around the current region
(ml-defun (wink-region
    (ml-if (pos-visible-in-window-p)
	(sit-for twiddle-interval)
    )
    (exchange-dot-and-mark)
    (ml-if (pos-visible-in-window-p)
	(sit-for twiddle-interval)
    )
    (exchange-dot-and-mark)
    (nothing)
))

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



