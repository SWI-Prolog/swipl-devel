;;;  SCCS: @(#)91/01/04 qprocess.el    3.7
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
;;;
;;; This interface was made possible by contributions from Fernando
;;; Pereira and various customers of Quintus Computer Systems, Inc.,
;;; based on code for Quintus's Unipress Emacs interface.
;;; 
(defvar *prolog-term-reading-mode* t)
(defvar *saved-prolog-process-mark* nil	; SWI
  "Saved process mark of *prolog* buffer")

;(defvar *last-prolog-command-string*)
;(defvar *startup-jcl*
;  (concat *c-switch* "\"Emacs:\"" prolog-zap-file "\""))

; ----------------------------------------------------------------------
; Modification of the pr-do-newline found in Unipress process stuff.
; This needs to know when we are in clause reading mode and then sense
; end of clause properly.
; Modified again by Dave Bowen: now sends a newline to Prolog if no non-
; whitespace characters have been typed.  This causes a new ?\|?- ?\ o be
; printed by Prolog (which automatically updates the process-output-marker).
;
; Adapted for GnuEmacs, including goal-history stuff, by Muralidhar Sitaram.
; ----------------------------------------------------------------------

(defun prolog-newline ()
  "Send input to prolog process. At end of buffer, sends all text after last output
as input to the prolog process, including a newline inserted at the end."
  (interactive)
  (cond ((string-equal (process-name
			 (get-buffer-process (current-buffer))) "prolog")
	 (setq ori-dot (point))
	 (end-of-line)
	 (cond ((< (point) 
		   (process-mark (get-buffer-process (current-buffer))))
		(goto-char ori-dot)
		(newline))
	       (t (cond ((eobp)
			  (move-marker last-input-start
				       (process-mark 
					 (get-buffer-process 
					   (current-buffer))))
;			  (insert ?\n)
			  (move-marker last-input-end (point)))
			 (t (newline))
	          )
;		   (end-of-line)
		  (cond ((or (not *prolog-term-reading-mode*)
			      (clause-end-p))         
			  (insert ?\n)        ; send stuff to prolog
			  (push-mark (point) t)
			  (goto-char (process-mark 
				       (get-buffer-process 
					 (current-buffer))))
			  (setq input-clause (region-to-string))
;;
;; If not in debugger mode then add to prolog-goal-history only if the
;; query is valid  
;; 
			  (cond ((null @at-debugger-prompt)
				 (save-excursion
				   (cond ((not (string-equal (setq 
							       current-goal
							       (valid-line))
							     ""))
					  (setq prolog-goal-history
						(cons current-goal
						      prolog-goal-history))
					 )))))
			  (pop-mark)
			  (setq *prolog-term-reading-mode* nil)
			  (setq @at-debugger-prompt nil)
			  (goto-char (point-max))
			  (let ((process (get-buffer-process 
					   (current-buffer))))
			    (send-string process input-clause)
			    (set-marker (process-mark process) (point)))
	                  )
			 (t (cond ((white-space-only)   ;  print prompt
				   (send-string (get-buffer-process
						  (current-buffer)) "\n")
				       (setq *prolog-term-reading-mode* nil)
				       (setq @at-debugger-prompt nil)
				       (newline))
				  (t (insert-string "\n     ")))))
	         )   
              )
         )
	(t (newline))
      )
 )

; define ^X^E-cache-empty

(defvar ^X^E-cache-empty t)

(defun maybe-copy-into-^X^E-cache (clause)
  (cond ((not (error-occurred (re-search-backward "| ?- \\=")))
	 (save-excursion
	   (set-buffer (get-buffer-create "&^X^E-cache"))
	   (erase-buffer)
	   (insert-string clause)
	   (delete-previous-character)
	   (setq ^X^E-cache-empty nil)))
  )
)

(defun white-space-only ()
  (save-excursion
    (goto-char 
      (process-mark (get-buffer-process (current-buffer))))
    (looking-at "[\001- \177]*\\'")
    )
  )

; -------------------------------------------------------------------------
; clause-end-p looks to see if we are a the end of a clause, i.e. we are
; positioned after a "." which is not preceded by an agglutinating character.
; There may be any number of spaces, tabs and newlines between the "." and
; the current position and/or between the current position and the end of
; file.  Any such whitespace characters are deleted in the case where
; clause-end-p turns out to be true, but not if it is false.
; -------------------------------------------------------------------------

(defun clause-end-p ()
  (setq init-dot (point))
	(cond ((and  (progn
		       (while (or (= 32  (following-char))
				  (= 9   (following-char))
				  (= 10  (following-char))
			      )
			 (forward-char))
		       (eobp)
		     )
		     (progn
		       (goto-char init-dot)
		       (while (or (= 32 (preceding-char))
				  (= 9  (preceding-char))
				  (= 10 (preceding-char))
			      )
			 (backward-char)
		       )
		       (and (> (dot) (process-mark 
				     (get-buffer-process (current-buffer))))
			    (= (preceding-char) 46)   ; a "." 
		       )
		     )
		     (progn
		       (backward-char)
		       (not (agglutinating-charp (preceding-char))))
	       )
	       (forward-char)
	       (push-mark (point) t)
	       (goto-char (point-max))
	       (kill-region (point) (mark))
	       (pop-mark) t)
	      (t (goto-char init-dot)
		 nil))
)


(defun start-new-prollog-process (command-line)
(let (
    name
  old-use-users-shell
  old-use-csh-option-f)

	(setq name (ml-arg 1))
	(pop-to-buffer name)
        (change-current-filename (prolog-init-filename))
  	(erase-buffer)
	(set-mark-command)
	(setq process-output-marker (point-marker))
	(setq prolog-term-reading-mode 0)
	(setq old-use-users-shell use-users-shell)
	(setq old-use-csh-option-f use-csh-option-f)
	(setq use-users-shell 1)
	(setq use-csh-option-f 1)
	(start-filtered-process			
	    (ml-arg 2 "command:")
	    name
	    "prollog-process-filter"
	)
	(setq use-users-shell old-use-users-shell)
	(setq use-csh-option-f old-use-csh-option-f)
	1
    )
)


(defun restart-prolog (input-saved-state)
	(ml-if (/= (current-process) "qprolog")
	    (progn
	       (setq input-saved-state (read-string (concat
		  ":* saved state [<RETURN> for "
		  (get-saved-state)
		  "] ")))
	       (ml-if (= input-saved-state "")
		   0
		   (progn
	              (setq last-prolog-command-string
			(concat &machine-dependent-jcl
			 input-saved-state
			 startup-jcl))))
               (&clear-message)
	       (start-prollog
		   last-prolog-command-string))
	    (progn
	       (bell-message
	        "Cannot start new Prolog until current one is killed"))))



; This routine obtains the name of the current saved state
(defun get-saved-state  (pos end tmp ans)
	(setq pos (find-minus-c 1))
	(setq end pos)
	(ml-if
	   (/= pos "error")
	   (progn	; pos is now at the end of the saved state name
	       (setq tmp (ml-substr last-prolog-command-string pos 1))
	       (while		
		   (not (zerop (logand (/= tmp " ")
		      (/= pos 0))))
		   (setq pos (- pos 1))
		   (setq tmp (ml-substr last-prolog-command-string pos 1))
	       )
	       (setq pos (+ pos 1))
	       (setq ans (ml-substr last-prolog-command-string pos 
				 (+ (- end pos) 1)))
	   )
	   (progn		
	       (setq ans "none found")
	   )
	)
	ans
)

; ----------------------------------------------------------------------
; This program will return the position of the last character in the saved
;  state taken from the string last-prolog-command-string.  It should always be
;  given an initial argument of 1    
; ----------------------------------------------------------------------

(defun find-minus-c  (tmp tmp1 pos pos1 ans)
	(setq pos (ml-arg 1))
	(setq tmp (ml-substr last-prolog-command-string pos 1))
	(setq pos1 (+ pos 1))
	(setq tmp1 (ml-substr last-prolog-command-string pos1 1))
	(while
	    (not (zerop (logand (/= "" tmp1)
	       (lognot (found-minus-c tmp tmp1))
	    )))
	    (setq pos (+ pos 1))
	    (setq tmp (ml-substr last-prolog-command-string pos 1))
	    (setq pos1 (+ pos 1))
	    (setq tmp1 (ml-substr last-prolog-command-string pos1 1))
	)
	(ml-if
	   (= "" tmp1)
	   (setq ans "error")
	   (ml-if
	      (= " " (ml-substr last-prolog-command-string (- pos 1) 1))
	      (setq ans (- pos 2))
	      (setq ans (- pos 3))
	   )
	)			 
	ans
)

(defun found-minus-c ans ()
  (ml-if
   (logand (= (ml-arg 1) "-")
           (= (ml-arg 2) "C"))
   (setq ans 1)
   (setq ans 0)
   )
  ans

  )


; ----------------------------------------------------------------------
; the routine prollog-process-filter inserts output in the qprolog buffer  
; ----------------------------------------------------------------------

; This flag is set to true only if in the middle of processing a packet.
(defvar *packet-pending* nil)   ; Set the flag to its default position.

(defconst *begin-packet-char* 30)
(defconst *end-packet-char* 29)
(defvar *packet-buffer* nil)

(defconst *begin-packet-string* (char-to-string *begin-packet-char*))
(defconst *end-packet-string* (char-to-string *end-packet-char*))
(defconst *packet-control-string* (concat *begin-packet-string* "\\|"
                                          *end-packet-string*))
(defun prolog-process-filter (process packet)
  (process-packets packet process)
  (cond (*saved-prolog-process-mark*	; SWI
	 (set-marker (process-mark (get-buffer-process "*prolog*"))
		     *saved-prolog-process-mark*)
	 (setq *saved-prolog-process-mark* nil)))
  (display-any-messages)
  )

(defun process-packets (packet process)
  (let ((packet-control (string-match *packet-control-string* packet)))
    (if *packet-pending*
	(cond 
	 ((not packet-control) 
	  (setq *packet-buffer* (concat *packet-buffer*  packet)))
	 ((string-equal 
	   (substring packet packet-control (1+ packet-control))
	   *end-packet-string*)
	  (process-prolog-packets
	   (concat *packet-buffer*
		   (substring packet 0 packet-control)))
	  (setq *packet-buffer* "")
	  (setq *packet-pending* nil)
	  (process-packets 
	   (substring packet (1+ packet-control)) process))
	 (t
	  (setq *packet-pending* nil)
	  (&qp-message "New packet arrived before end of old one")
	  (setq *packet-buffer* "")
	  (process-packets
	   (substring packet (1+ packet-control))
	   process)))
      ;; else
      (cond 
       ((not packet-control)
	(save-excursion
	  (set-buffer (process-buffer process))
	  (goto-char (point-max))
	  (let ((now (point)))
	    (insert packet))
	  (if (process-mark process)
	      (set-marker (process-mark process) (point))))
	(if (eq (process-buffer process) (current-buffer))
	    (goto-char (point-max))))
       ((string-equal 
	 (substring packet packet-control (1+ packet-control)) 
	 *end-packet-string*)
	(&qp-message "Found end of packet which was not started")
	(setq *packet-buffer* "")
	(process-packets (substring packet (1+ packet-control)) process))
       (t
	(if (> packet-control 0)
	    (progn
	      (save-excursion
		(set-buffer (process-buffer process))
		(goto-char (point-max))
		(let ((now (point)))
		  (insert (substring packet 0 packet-control))
		  (if (process-mark process)
		      (set-marker (process-mark process) (point))))
		(if (eq (process-buffer process) (current-buffer))
		    (goto-char (point-max))))))
	(setq *packet-pending* t)
	(process-packets
	 (substring packet (1+ packet-control)) process))
       )
      )
    )
  )

; standard packet types that are very frequently given are indicated
; by a single letter, otherwise, the packet routine is simply the name
; of the Emacs-Lisp routine to be executed.

(defun process-prolog-packets (packet)
  (let ((packet-type (substring packet 0 1)))
    (cond    
     ((string-equal packet-type "a") (setq *prolog-term-reading-mode* t))
     ((string-equal packet-type "d")
      (setq global-mode-string 
	    (append original-mode-string 
		    (list (strip-module packet))))
      (cond 
       ((string-match "debug" packet)
        (setq mode-line-format 
              "--%1*%1*-Emacs: %b   %M *Debug*   %[(%m: %s)%]----%3p--%-"))
       ((string-match "trace" packet)
        (setq mode-line-format 
              "--%1*%1*-Emacs: %b   %M *Trace*  %[(%m: %s)%]----%3p--%-"))
       (t
        (setq mode-line-format 
              "--%1*%1*-Emacs: %b   %M          %[(%m: %s)%]----%3p--%-")))
      (set-buffer-modified-p (buffer-modified-p)))
     ((string-equal packet-type "m") (&qp-message (substring packet 1)))
     (t (if (error-occurred (eval (read packet)))
            (progn 
	      (&qp-message 
	       (concat "Lisp packet could not execute: " packet))
              )
	  )
          )
        )
     )
)

;-----------------------------------------------------------------------

(defun send-prolog (query)
  (send-string "prolog"
               (concat "\^]" query " .\n")
               )
  )

;; Function not used

(defun send-prolog-directly (string)
  (&clear-message)
  (setq *prolog-term-reading-mode* nil)
  (setq @at-debugger-prompt nil)
  (set-string "prolog" string)
  )

; mode-line support function
(defun strip-module (packet)
  (cond ((setq mod-pos (string-match "Module:" packet))
	 (concat "Module:" (substring packet (+ mod-pos 7) (+ mod-pos 17))))
	(t nil)))

