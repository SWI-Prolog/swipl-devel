;;; @(#)qprolog-indent.el	3.4 12/12/90 
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
;;; This interface was made possible by contributions from various
;;; customers of Quintus Computer Systems, Inc., based on code for
;;; Quintus's Unipress Emacs interface.
;;; 

;;;
;;; User Settable variables to control Indentation
;;;
(defvar head-continuation-indent 6
   "Offset for continuation of clause head arguments.")
(defvar body-predicate-indent 8
   "The column at which the body of a predicate is to begin")
(defvar if-then-else-indent 4
   "Offset within an if-then-else statement")
(defconst prolog-tab-always-indent t
   "*Non-nil means TAB in Prolog mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")
(defconst fact-column 0 
   "Column at which facts and single line clauses begin")


;; This is used by indent-for-comment
;; to decide how much to indent a comment in Prolog code
;; based on its context.
(defun prolog-comment-indent ()
  (cond 
   ((looking-at "%%%") (current-column))
   ((looking-at "%%") (current-column))
;;    (let ((tem (calculate-prolog-indent)))
;;      (if (listp tem) (car tem) tem)))
   ((= (following-char) ?%)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (current-column))
	 comment-column))
   ((looking-at "^/\\*") 0)		;Existing comment at bol stays there.
   (t (save-excursion
	(skip-chars-backward " \t")
	(max (current-column)           ;Else indent at comment column
	     comment-column)))))	

(defun prolog-inside-parens-p ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-clause) 
				   (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

(defun prolog-indent-command (&optional whole-exp)
  "Indent current line as Prolog code, or in some cases insert a tab
character.  If prolog-tab-always-indent is non-nil (the default),
always indent current line.  Otherwise, indent the current line only
if point is at the left margin or in the line's indentation; otherwise
insert a tab.  A numeric argument, regardless of its value, means
indent rigidly all the lines of the expression starting after point so
that this line becomes properly indented.  The relative indentation
among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as Prolog
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (prolog-indent-line))
	    beg end)
	(save-excursion
	  (if prolog-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-term 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "%")))
    (if (and (not prolog-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (prolog-indent-line))))

(defun prolog-indent-line ()
  "Indent current line as Prolog code.
Return the amount the indentation changed by."
  (interactive)
  (let ((indent (calculate-prolog-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-prolog-indent-within-comment)))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   ))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))


(defun calculate-prolog-indent (&optional parse-start)
  "Return appropriate indentation for current line as Prolog code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((head-of-clause-p)
	   0)
	  ((is-comment)
	   t)
	  (t (let ((indent-point (point)) 
		   (case-fold-search nil)
		   state
		   containing-sexp)
	       (if parse-start
		   (goto-char parse-start)
		   (beginning-of-clause))
	       (while (< (point) indent-point)
		 (setq parse-start (point))
		 (setq state (parse-partial-sexp (point) indent-point 0))
		 (setq containing-sexp (car (cdr state))))
	       (cond ((or (nth 3 state) (nth 4 state))
		      ;; return nil or t if should not change this line
		      (nth 4 state))
		     ((null containing-sexp)
		      ;; Line is at top level.  It is thus a complete
		      ;; predicate, or a continuation of an expression.
		      ;; if it is a clause head or fact then leave it
		      ;; at column 0. If it is in the clause body,
		      ;; then indent by body-predicate-indent.  
		      (goto-char indent-point)
		      (skip-chars-forward " \t")
		      (cond ((and (is-fact)           ;; if fact
				  (check-clause))
			     fact-column)
			    ((point-in-clause-head-p) ;; clause body or head
			     fact-column)
;;			     head-continuation-indent)
;;			    (t body-predicate-indent)))
			    (t (maybe-indent-body))))
		     (t
		      ;; line is expression, not statement:
		      ;; indent to just after the surrounding open.
		      (cond ((setq pos (prolog-inside-term-p))
;;	What is this ?      (= 1 (car state))) 
			     (get-arg-column pos))
			    (t 
			     ;; Must be an if-then-else
			     ;; If the else part i.e. [;|] then indent
			     ;; to level of current if [(]. Otherwise
			     ;; must indent to level of current if [(]
			     ;; plus if-then-else-indent (user settable)
			     (cond ((prev-line-has-comma)
				    (get-term-column 0))
				   ((string-equal (setq where
							(after-cond))
						 "no-term")
				    (+ if-then-else-indent 
				       (get-first-term-column)))
				   ((else-part indent-point)
				    (goto-char containing-sexp)
				    (current-column))
				   (t (+ if-then-else-indent 
					 containing-sexp))))
			    )
		      )
		    )
          )
        )
     )
  )
)
;;;
;;;  Are we in the head of a clause ? There mabe no :- or --> on
;;;  the line.
;;;
(defun point-in-clause-head-p ()
  (save-excursion
    (cond ((check-prev-line-for-dot)
	   t)
	  (t nil))))
;;;
;;; Are we on a line that contains the head of clause?
;;; That is does it contain :- or -->
;;;
(defun head-of-clause-p ()
  (save-excursion
    (end-of-line)
    (let ((limit (point)) 
	  (done)
	  (in-head))
      (beginning-of-line)
      (while (not done)
	(if (re-search-forward ":-\\|-->" limit t)
	    (if (not (prolog-inside-parens-p))
		(progn (setq done t) (setq in-head t)))
	  (setq done t)
	  (setq in-head nil)
	  )
	)
      in-head
      )
    )
  )
;;;
;;; prolog-inside-term-p
(defun prolog-inside-term-p ()
  (let ((bracket-pos nil))
    (condition-case ()
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point)
			      (progn (beginning-of-clause) (point)))
	    (goto-char (point-max))
	    (cond ((= (char-after 
			(setq bracket-pos (or (scan-lists (point) -1 1) 
					  (point-min))))
			?\()
		   (goto-char bracket-pos)
		   (skip-chars-backward "\t")
		   (cond ((not (or (= (preceding-char) 32)
				   (= (preceding-char) 9)
				   (= (preceding-char) 10)))
			  bracket-pos)
			 (t nil)))
		  (t nil))))
	    (error nil))))
;;;
;;; get-arg-column
;;;
(defun get-arg-column (pos)
  (save-excursion
    (goto-char pos)
    (forward-char)
    (skip-chars-forward " \t")
    (current-column)))
;;;
;;; maybe-indent-body
;;;
(defun maybe-indent-body ()
  (save-excursion
    (goto-valid-line)
    (beginning-of-line)
    (cond ((head-of-clause-p)
	   body-predicate-indent)
	  (t (skip-chars-forward " \t")
	     (current-column)))))
;;;
;;; is-fact ?
;;;
(defun is-fact ()
  (save-excursion
    (end-of-line)
    (let ((pos (point)))
      (beginning-of-line)
      (cond ((re-search-forward "%" pos t)   ; comment present
	     (backward-char)
	     (skip-chars-backward " \t"))
	    (t (end-of-line)
	       (skip-chars-backward " \t")))
      (cond ((= (preceding-char) ?.)
	     t)
	    ( t nil)))))
;;;
;;; check-clause
;;;
(defun check-clause ()
  (cond ((check-prev-line-for-dot)
	 t)
	(t nil)))

(defun goto-valid-line ()
  (let ((done t)
	(star-comment)
	(line-skip -1))
    (while done
      (cond ((not (eq (forward-line line-skip) 0))
	     (setq done nil))
	    (t (skip-chars-forward " \t")
	       (cond ((and (not (white-space))               ; valid line
			   (not (setq star-comment 
				      (in-prolog-/*-*/-comment)))
			   (/= (following-char) ?%))
		      (setq done nil) t)
		     (t (cond ((white-space)                 ; blank line
			       (setq line-skip -1))
			      (star-comment                  ; /*-*/ comment
			       (if (single-line-comment)
				   (if (not (only-comment))
				       (setq done nil) 
				       (setq line-skip -1))
				   (progn
				     (skip-prolog-/*-*/-comment (point-max))
				     (setq line-skip -1))))
			      ((= (following-char) ?%)       ; % comment line
			       (skip-prolog-%-comment (point-max))
			       (setq line-skip 0))))))))))
;;;
;;; only-comment
;;;
(defun only-comment ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond ((looking-at "/\\*")
	   t)
	  ( t nil))))
;;;
;;; white-space
;;;
(defun white-space ()
  (or (= (following-char) 32)
      (= (following-char) 9)
      (= (following-char) 10)))
;;;
;;; prolog-/*-*/-comment
;;;
(defun in-prolog-/*-*/-comment ()
  (save-excursion 
    (end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (cond ((re-search-forward "/\\*\\|\\*/" limit t)
	     t)
	    (t nil)))))
;;;
;;; single-line-comment
;;;
(defun single-line-comment ()
  (save-excursion
    (end-of-line)
    (let ((limit (point))
	  line)
      (beginning-of-line)
      (setq line (point))
      (if (and (search-forward "*\/" limit t)
	       (search-backward "\/*" line t))
	  t
	  nil))))
;;;
;;; check-prev-line-for-dot
;;;
(defun check-prev-line-for-dot ()
  (save-excursion
    (goto-valid-line)
    (end-of-line)
    (let ((pos (point)))
      (beginning-of-line)
      (cond ((re-search-forward "%" pos t)   ; comment present
	     (backward-char)
	     (skip-chars-backward " \t"))
	    (t (end-of-line)
	       (skip-chars-backward " \t")))
      (cond ((= (preceding-char) ?.)
	     t)
	    (t nil)))))
;;;
;;; Are we in the else (; or | otherwise ->) of an if ?
;;;
(defun else-part (indent-point)
  (save-excursion
    (goto-char indent-point)
    (skip-chars-forward " \t")
    (cond ((looking-at "[;|)]")
	   t)
	  ((looking-at "")
	   (cond ((prev-line-has-comma)
		  nil)
		 (t t)))
	  ( t nil)
    )
  )
)
;;;
;;; prev-line-has-comma
;;;
(defun prev-line-has-comma ()
  (save-excursion
    (goto-valid-line)
    (maybe-goto-end-of-line)
    (cond ((= (preceding-char) ?,)
	   t)
	  (t nil))))
;;;
;;; get-term-column
;;;
(defun get-term-column (arg)
  (save-excursion 
    (goto-valid-line)
    (beginning-of-line)
    (while (prolog-inside-term-p)
      (goto-valid-line))
    (skip-chars-forward " \t")
    (cond ((looking-at "[;|]")
	   (forward-char)
	   (skip-chars-forward " \t"))
	  (t (forward-term arg)))
    (current-column)))
;;;
;;; get-first-term-column
;;;
(defun get-first-term-column ()
  (save-excursion
    (goto-valid-line)
    (maybe-goto-end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (cond ((re-search-forward "(\\||\\|;" limit t)
	     (skip-chars-forward " \t")
	     (cond ((looking-at "(")
		    (forward-char)
		    (skip-chars-forward " \t")))
	     (current-column))))))
;;;
;;; after-cond
;;;
(defun after-cond ()
  (save-excursion
    (goto-valid-line)
    (maybe-goto-end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (cond ((re-search-forward "->" limit t)
	     (skip-chars-forward " \t")
	     (cond ((or (white-space)
			(= (following-char) 37))
		    "no-term")
		   (t "term")))
	    (t "no")))))
;;;
;;; maybe-goto-end-of-line
;;;
(defun maybe-goto-end-of-line ()
  (end-of-line)
  (let ((pos (point)))
    (beginning-of-line)
    (cond ((re-search-forward "%" pos t)   ; comment present
	   (backward-char)
	   (skip-chars-backward " \t"))
	  (t (end-of-line)
	     (skip-chars-backward " \t")))))
;;;
;;; Are we in a comment ? (Either [%+] or between "/*" and "*/")
;;;
(defun is-comment ()
  (let (found)
    (save-excursion
      (skip-chars-forward " \t")
      (cond ((or (looking-at "%+")
		 (looking-at "/\\*")
		 (looking-at "\\*/"))
	     (setq found t))
	    ((re-search-backward "/\\*\\|\\*/" 0 t)
	     (cond ((looking-at "/\\*")
		    (setq found t))
		   (t (setq found nil))))
	    (t (setq found nil))
      )
    )
    found
  )
)
;;;
;;; beginning-of-clause
;;;
(defun beginning-of-clause (&optional arg)
  "Move backward to next beginning-of-clause.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (let ((clause-point (point)) (not-done t) (command-point (point)))
    (while (and not-done (not (bobp)))
      (if (and arg (< arg 0))
      (skip-chars-forward " \t\n")
      (skip-chars-backward " \t\n"))
      (if (re-search-backward "^\\S-" nil 'move (or arg 1))
	  (progn
	    (if (white-space)
		(re-search-backward "^\\S-" nil 'move 1))
	    (if (= (following-char) ?%)
		(skip-prolog-%-comment clause-point)
		(if (is-comment)
		    (setq not-done 
			  (not (skip-prolog-/*-*/-comment clause-point)))
		    (if (not (white-space))
			(setq not-done nil))))))
      (setq clause-point (point)))
    )
  )


(defun calculate-prolog-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (cond 
       ((looking-at "%%%")
	(current-column))
       ((looking-at "%%")
	(current-column))
       ((looking-at "\\*/")
	(current-column))
       ((looking-at "/\\*")
	0)
;;	(let ((tem (calculate-prolog-indent)))
;;	  (if (listp tem) (car tem) tem)))
       ((= (following-char) ?%) 
	(skip-chars-backward " \t")
	(max (if (bolp) 0 (1+ (current-column)))
	     comment-column))
       (t
	 (setq star-start (= (following-char) ?\*))
	 (skip-chars-backward " \t\n")
	 (setq end (point))
	 (beginning-of-line)
	 (skip-chars-forward " \t")
	 (and (re-search-forward "/\\*[ \t]*" end t)
	      star-start
	      (goto-char (1+ (match-beginning 0))))
	 (current-column)
	 )
	)
      )
    )
  )

(defun prolog-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(beginning-of-line)
	(skip-chars-forward " \t")
	(if (looking-at "#")
	    (setq stop (<= (point) lim))
	  (setq stop t)
	  (goto-char opoint))))))   

(defun prolog-indent-clause ()
  "Indent each line of the prolog clause"
  (interactive)
  (end-of-clause)
  (setq end (point))
  (beginning-of-clause)
  (prolog-indent-lines end))

(defun prolog-indent-lines (end)
  (cond 
    ((> (point) end) t)
    (t (prolog-indent-line)
       (next-line 1)
       (prolog-indent-lines end))))
