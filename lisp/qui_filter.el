; /ports/emacs/GNU/el3.1 @(#)qui_filter.el	1.1 11/15/90 
; /ports/home/sitaram/Gnu/qui_emacs @(#)qui_filter.el	1.5 11/13/90 
;;;		      QUI - GNU Emacs Interface
;;;			  Support Functions
;;;
;;;		  Consolidated by Sitaram Muralidhar
;;;
;;;			 sitaram@quintus.com
;;;		    Quintus Computer Systems, Inc.
;;;			     12 Nov 1990
;;;
;;;    This file defines functions that support the QUI - GNU Emacs
;;;			      interface.

(defvar SIZEOFSIZE           4)
(defvar SIZEOFCOMMAND        8)
(defvar *packet-command*    ""
  "The current command being processed")
(defvar *partial-command*   "")
(defvar *partial-size*      "")
(defvar *current-size*      -1)
(defvar *partial-arg*       "")
(defvar *current-arg*       100)


;;;
;;; begin-reading called from qui-process-filter, checks if the
;;; command can be read (8 chars), if so it figures out the number of
;;; units the rest of the packet should have and calls
;;; process-rest-of-packet. If the command itself is incomplete in
;;; this packet *partial-command* is set to the part-formed command
;;; and *qui-packet-pending* is set indicating that there is more to
;;; come. 
;;; 

(defun begin-reading ( packet )
  (cond ((>= (length packet) SIZEOFCOMMAND)
	 ; Command read in full
	 (setq *packet-command* (substring packet 0 SIZEOFCOMMAND))
	 (setq *partial-command* "")
	 (process-rest-of-packet (substring packet SIZEOFCOMMAND)
				 (command-length *packet-command*)))
	; Do not have full command
	(t (setq *partial-command* packet)     
	   (setq *qui-packet-pending* t))
 )
)

;;;
;;; continue-reading called from qui-process-filter, checks if it is
;;; in the process of reading a command or the remainder of the
;;; packet. If reading a command, it checks to see if it has the
;;; entire command, if so
(defun continue-reading ( packet )
  (if (not (string-equal *partial-command* ""))
      (cond ((>= (+ (length *partial-command*) 
		    (length packet))
		 SIZEOFCOMMAND)
	     ; have command
	     (setq *packet-command* 
		   (concat *partial-command* 
			   (substring packet 0 
				      (setq chars-read 
					    (- SIZEOFCOMMAND
						  (length *partial-command*))))))
	     (setq *partial-command* "")
	     (process-rest-of-packet (substring packet chars-read) (1- number)))
	    ; Incomplete command
	    (t (setq *partial-command* (concat *partial-command* packet))
	       (setq *qui-packet-pending* t))
      )
      ; No partial command must be processing rest of packet
      (process-rest-of-packet packet (command-length *packet-command*))
   )
)
		       
(defun process-rest-of-packet ( packet number )
  (cond ((> number 0)
	 (if (not (string-equal *partial-size* ""))
	     (cond ((>= (+ (length *partial-size*)
			   (length packet)) 
			SIZEOFSIZE)
		    (setq *current-size* 
			  (string-to-int 
			    (concat *partial-size* 
				    (substring packet 0 
					       (setq chars-read 
						     (- SIZEOFSIZE 
							(length *partial-size*)))))))
		    (setq *partial-size* "")
		    (process-arg *current-size* (substring packet chars-read) 
				 number))
		   (t (setq *partial-size* (concat *partial-size* packet))))
	     ; have full size or no size
	     (if (not (equal *current-size* -1))
		 (process-arg *current-size* packet number)
		 (cond ((>= (length packet) SIZEOFSIZE)
			(setq *current-size* (string-to-int 
					       (substring packet 0 SIZEOFSIZE)))
			(setq *partial-size* "")
			(process-arg *current-size* (substring packet SIZEOFSIZE)
				     number))
		       (t (setq *qui-packet-pending* t)
			  (setq *partial-size* packet)))
	     )
	 ))
	(t (setq *qui-packet-pending* nil)
	   (process-qui-packets)
	   (if (> (length packet) 0)
	       ; the packet contains more than one command - begin reading again
	       ( begin-reading packet )
	   )
        )
     )
)

(defun process-arg ( size packet number )
  (if (not (string-equal *partial-arg* ""))
      (cond ((>= (+ (length *partial-arg*)
		    (length packet))
		 size)
	     (set-variable (intern (concat "arg" number))
			   (concat *partial-arg* 
				   (substring packet 0 
					      (setq chars-read 
						    (- size 
						       (length *partial-arg*))))))
	     (setq *current-arg* number)
	     (setq *partial-arg* "")
	     (process-rest-of-packet (substring packet chars-read) (1- number)))
	    (t (setq *qui-packet-pending* t)
	       (setq *current-arg* number)
	       (setq *partial-arg* packet))
      )
      ; No arg or full arg
      (if (<= *current-arg* number)
	  (process-rest-of-packet packet (1- number))
	  ; No arg yet
	  (cond ((>= (length packet) size)
		 ; Have full arg
		 (set-variable (intern (concat "arg" number ))
			       (substring packet 0 size))
		 (setq *current-arg* number)
		 (setq *current-size* -1)
		 (setq *partial-arg* "")
		 (process-rest-of-packet (substring packet size) (1- number)))
		(t (setq *qui-packet-pending* t)
		   (setq *current-arg* number)
		   (setq *partial-arg* packet)))
     )
  )
)   	       
	   


