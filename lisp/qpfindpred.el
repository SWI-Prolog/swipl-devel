;;;  SCCS: @(#)90/12/04 qpfindpred.el    2.3
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

(defmacro error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defun find-pred ()
  (interactive)
  (let 
      (term-start
       term-type
       target-functor
       target-arity 
       backward-predicate-beginning
       backward-predicate-end 
       (continue t)
       (saw-first-clause (find-potential-clause)))

    (get-clause-info)
    (setq target-functor *functor*)
    (setq target-arity *arity*)

    (setq backward-predicate-end (mark))
    (setq backward-predicate-beginning (dot))

    (previous-line 1)
    (while (and continue
                (not saw-first-clause)
                (not (error-occurred
                      (setq saw-first-clause (find-potential-clause))
                      (get-clause-info)))
                )
      (if (and (string-equal *functor* target-functor) 
               (= *arity* target-arity))
          (progn 
            (setq backward-predicate-beginning (dot))
            (previous-line 1))
        (setq continue nil)
        )
      )

    (goto-char backward-predicate-end)
    (setq continue t)
    (while (and continue
                (not (error-occurred (get-clause-info)))
                )
      (if (and (string-equal *functor* target-functor)
               (= *arity* target-arity))
          (progn 
            (setq backward-predicate-end (mark))
            (exchange-dot-and-mark))
        (setq continue nil)))

    (goto-char backward-predicate-beginning)
    (set-mark (point))
    (goto-char backward-predicate-end)
    )
  )

(defun forward-to-clause-end  (term-start)
  (let ((continue t)
        (token-type (next-token)))
    (while continue
      (cond 
       ((string-equal token-type "eof")
        (goto-char term-start)
        (BadFind "Can't find the end of this term"))
        ((string-equal token-type "stop")
        (setq continue nil))
        (t (setq token-type (next-token)))
        )
      )
    )
  )

(defun forward-to-clause-end-heuristic (term-start)
  (if (not (re-search-forward "[^-+*/\\^<>=`~:.?@#$&]\\.[\001- \177]" nil t))
      (progn
        (goto-char term-start)
        (BadFind "Can't find the end of this term")
        )
    )
  (while (within-comment)               ;  doesn't work with '%' style comments
    (if (not (re-search-forward "[^-+*/\\^<>=`~:.?@#$&]\\.[\001- \177]" nil t))
        (progn
          (goto-char term-start)
          (BadFind "Can't find the end of this term")
          )
      )
    )
  )


(defun arity-overhead-for-grammar-rule  (term-start)
  (let
      ((token-type (next-token))
       (token (region-to-string))
       return)

    (cond 
     ((string-equal token ":-")
      (forward-to-clause-end term-start)
      (setq return 0))
     ((string-equal token-type "stop")
      (setq return 0))
     (t
      (if (grammar-rule token-type token)
          (progn 
            (forward-to-clause-end term-start)
            (setq return 2)
            )
        (progn 
          (goto-char term-start)
          (BadFind "Can't determine what type of clause this is")
          )
        )
      )
     )
    return
    )
  )

(defun BadFind (message)
  (sleep-for-millisecs 10)
  (error message))

(defun grammar-rule  (mgr-token-type mgr-token)
  (let (return
        (continue t))
    (while continue
      (cond
       ((string-equal mgr-token "-->")
        (setq continue nil)
        (setq return t))
       ((or (string-equal mgr-token-type "stop")
            (string-equal mgr-token-type "eof"))
        (setq continue nil)
        (setq return nil))
        (t
         (setq mgr-token-type (next-token))
         (setq mgr-token (region-to-string))
         )
	)
    )
    return
))

; 
; The following code finds the first line less than or equal to the current
;	line which has a non-layout character in the first column or the
;	first column does not start a comment ('%' or '/*'), end a comment
;	('*/'), or is within a comment.
;

(defun find-potential-clause ()
  (let
      ((continue t)
       (original-dot (dot))
       char 
       (return))
    (while continue
      (beginning-of-line)
      (setq char (following-char))
      (if (or 
           (or (and (>= char ?\001) (<= char ?  )) (= char ?\177))  ; layout
           (looking-at "%\\|/\\*\\|\\*/")  ; '%' or '/*' or '*/'
           (within-comment)
           (eobp)
           )
          (if (bobp)
              (progn 
                (goto-char original-dot)
                (BadFind "Cannot find a valid line to start the procedure, command or query")
                )
            (previous-line 1)
            )
        (progn 
          (if (bobp)
              (setq return t)
	       )
	       (setq continue nil)
	   )
	)
    )
    return
))

(defun get-clause-info ()
  (let ((term-start (dot))
        (token-type (next-token))
        (token (region-to-string)))
    (cond 
     ((string-equal token-type "atom")
      (if (or (string-equal token ":-")
              (string-equal token "?-"))
          (progn 
            (setq *functor* token)
            (setq *arity* 1)
            (forward-to-clause-end term-start)
            (set-mark (point))
            (goto-char term-start)
            )
        (progn 
          (setq *functor* token)
          (setq *arity* 0)
          (setq *arity* 
                (+ *arity* (arity-overhead-for-grammar-rule term-start)))
          (set-mark (point))
          (goto-char term-start)
          )
        )
      )
     ((string-equal token-type "functor")
      (setq *functor* token)
      (setq *arity* (all-arity term-start))
      (set-mark (point))
      (goto-char term-start)
      )
      ((string-equal token-type "eof")
       (setq *functor* "end_of_file")
       (setq *arity* -1)
       (set-mark (point))
       (goto-char term-start)
       )
      (t (BadFind "Wrong term type to start clause, command, or query"))
      )
    )
)

(defun BadArity (message)
  (error message)
)

(defun all-arity (term-start)
  (let ((temp-arity (head-arity)))
    (+ temp-arity (arity-overhead-for-grammar-rule term-start))
  )
)

; 
; This function assumes that you are looking at the left parenthesis
; immediately following the atom of a complex predicate.  It knows that
; it's looking at the parenthesis because the tokenizer looks ahead to
; it, '(', to determine that it is just read a "functor" instead of just an 
; "atom".
;	    foo(X,Y) ...
;	       ^
; 

(defun head-arity ()
  (let  ((state 0)
         (stack)
         (arity 1)
         token-type
         token
         (original-dot (dot)))

    (if (= (following-char) ?\( )
	(forward-char)
      (BadArity "Can't start calculating arity at this point")
      )

    (while (not (= state 2))
      (setq token-type (next-token))
      (setq token (region-to-string))

      (if (or (string-equal token-type "stop")
              (string-equal token-type "eof")
              )
          (progn 
            (goto-char original-dot)
            (BadArity "Unclosed parenthesis.")
            )
        )
      (cond 
       ((= state 0)
        (cond
         ((string-equal token "(")
          (progn 
            (setq stack (cons ")" stack))
            (setq state 1)
            ))
         ((string-equal token "[")
          (progn 
            (setq stack (cons "]" stack))
            (setq state 1)
            ))
         ((string-equal token "{")
          (progn 
            (setq stack (cons "}" stack))
            (setq state 1)
            ))
         ((string-equal token ")")
          (setq state 2))
         ((or (string-equal token "]") (string-equal token "}"))
          (BadArity "Mismatched parentheses."))
         ((string-equal token ",")
          (setq arity (+ arity 1)))
	 ))
       ((= state 1)
        (cond
         ((string-equal token "(") (setq stack (cons ")" stack)))
         ((string-equal token "[") (setq stack (cons "]" stack)))
         ((string-equal token "{") (setq stack (cons "}" stack)))
         ((or 
           (string-equal token ")") 
           (string-equal token "]") 
           (string-equal token "}"))
          (if (not (string-equal (car stack) token))
              (BadArity "Mismatched parentheses.")
            )
          (setq stack (cdr stack))
          (if (null stack)
              (setq state 0)
            )
          )
	 )
        )
       )
      )
    arity
    )
  )

(defun within-comment ()
  (let ((original-dot (dot)) (return))
    (if (not (re-search-backward "/\\*\\|\\*/" nil t)) ; '/*' or '*/'
        (setq return nil)
        (if (= (following-char) ?*)      ; found '*/'
            (progn 
              (goto-char original-dot)
              (setq return nil)
              )
          (progn
            (goto-char original-dot)
            (setq return (search-forward "*/" nil t))
            (goto-char original-dot)
            )
          )
      )
    return
    )
)
