;;;  SCCS: @(#)91/01/10 qpcommands.el    3.10
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

; ----------------------------------------------------------------------
;                    Incremental reconsulting
; ----------------------------------------------------------------------


(defmacro error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defun prolog-compile ()
  (interactive)
  (prolog-load "compile")
)

(defun ensure-prolog-is-running ()
  (let ((source-buffer (current-buffer)))
    (if (get-buffer "*prolog*")
;	(progn
;	  (switch-to-buffer-other-window "*prolog*")
;	  (other-window 1))
      (let ((proc (get-buffer-process (get-buffer "*prolog*"))))
 	  (if (not (and proc (eq (process-status proc) 'run)))
 	      (error "No Prolog process available")))
      (condition-case nil
	  (progn
	    (switch-to-buffer-other-window "*prolog*")
	    (run-prolog)
 	    (switch-to-buffer-other-window source-buffer))
	(error
	 (progn
	   (or (get-buffer "*prolog*")
	       (kill-buffer "*prolog*")
	       (switch-to-buffer source-buffer)
 	       (signal 'error nil))))))))

(defun prolog-consult-predicate ()
  (interactive)
  (ensure-prolog-is-running)
  (let (pl-char )
    (cond 
     ((string-equal (buffer-name) "*prolog*")
      (error "Cannot load from the Prolog execution buffer"))
     ((string-equal (buffer-file-name) "")
      (error "Cannot load from this buffer before it is written to a file"))
     ((not (safe-to-load-code))
      (error "Cannot load unless Prolog is at top-level prompt"))
     (t
      (send-load-to-prolog "consult" ?p)
      )
     )
    )
  )

(defun prolog-load  (pl-load-type)
  (let (pl-char )
    (ensure-prolog-is-running)
    (cond 
     ((string-equal (buffer-name) "*prolog*")
      (error "Cannot load from the Prolog execution buffer"))
     ((string-equal (buffer-file-name) "")
      (error "Cannot load from this buffer before it is written to a file"))
     ((not (safe-to-load-code))
      (error "Cannot load unless Prolog is at top-level prompt"))
     (t
      (sleep-for 1)
      (message 
       (concat 
        pl-load-type
        " Prolog... enter p for procedure, r for region or b for buffer "))
      (send-load-to-prolog pl-load-type (read-char))
      )
     )
    )
  )

(defun send-load-to-prolog  (sltp-load-type sltp-char)
  (let (word1 word2
	      (file-name (expand-file-name (buffer-file-name))))
    
    (cond 
     ((string-equal sltp-load-type "compile")
      (setq word1 "Compiling"))
     ((string-equal sltp-load-type "consult")
      (setq word1 "Consulting"))
     )
    (cond 
     ((= sltp-char ?p)
      (save-excursion
        (message "Please Wait, finding predicate boundaries...")
        (sit-for 0)
	(save-excursion
	  (find-pred)
					;       (wink-region)
	  (write-region (point) (mark) prolog-zap-file)
	  )
        )
      (setq word2 "procedure")
      )
     ((= sltp-char ?r)
					;      (wink-region)
      (write-region (point) (mark) prolog-zap-file)
      (setq word2 "region")
      )
     ((= sltp-char ?b)
      (save-excursion
        (mark-whole-buffer)
        (write-region (point) (mark) prolog-zap-file)
        )
      (setq word2 "buffer")
      )
     (t (error "Bad option"))
     )
    (pop-to-buffer "*prolog*")
    (goto-char (process-mark (get-buffer-process "*prolog*")))
    (setq *prolog-term-reading-mode* nil)
    (message (concat word1 " " word2 "..."))
    (sit-for 0)
    (&clear-message)
    (send-prolog 
     (concat "'$editor_load_code'('" word2 "','" file-name "')"))
    ))

(defun safe-to-load-code ()
  (and (or *prolog-term-reading-mode*
	   @at-debugger-prompt)
       (dot-process-output-marker-ok)
       )
  )

(defun dot-process-output-marker-ok ()
    (save-excursion
	(set-buffer "*prolog*")
	(goto-char (process-mark (get-buffer-process "*prolog*")))
	(looking-at "[\001- \177]*\\'")
    )
)


; ---------------------------------------------------------------------
;   This routine is not being used presently, see goal-history instead
; ---------------------------------------------------------------------


(defun execute-previous-input(arg)
  (interactive "p")
  (let ( index previous-query original-dot)
       (cond 
	 ((and (>= (point-marker) 
		      (process-mark (get-buffer-process "*prolog*")))
                (<= arg 1)  ; Check This
		(null ^X^E-cache-empty)
          )
          (end-of-buffer)
	  (insert-buffer "&^X^E-cache")
         )
      (t (setq original-dot (point-marker))
         (cond 
	    ((= (point-marker) (process-mark
				 (get-buffer-process "*prolog*")))
             (search-backward "| ?- "))
	    ((> (point-marker) (process-mark
				 (get-buffer-process "*prolog*")))
             (goto-char (process-mark (get-buffer-process "*prolog*")))
	     (search-backward "| ?- "))
 	     (t (forward-char))
         )
	 (setq index arg)
	 (while (> index 0)
	   (cond 
	     ((error-occurred (search-backward "| ?- "))
	      (goto-char original-dot)
	      (error "Cannot find a valid query to grab"))
	     (t (save-excursion 
		  (re-search-forward " ?- ")
		  (setq previous-query (valid-line))
		)
		(cond ((not (string-equal previous-query ""))
		       (setq index (- index 1)))
		)
	     )
	   )
         )
	 (end-of-buffer)
	 (insert-string previous-query)
      )
  )
))

;; valid-line
;; This function called by "prolog-newline"
;; Checks if current query is valid; appropriate to send to prolog
;;

(defun valid-line ()
  (let ((current-token nil)
	 (return nil)
	 (continue 1)
	 (current-mark (dot))
	 (saved-mark  (mark-marker)))
    (cond ((error-occurred (setq current-token (next-token)))
	   (setq continue 0)
           (setq return ""))
    )
    (while (not (zerop continue))
	(cond ((string-equal current-token "stop")
               (setq continue 0)
	       (backward-char)
	       (set-mark (point))
	       (goto-char current-mark)
	       (setq return (region-to-string)))
	      ((and (string-equal current-token "atom")
	            (string-equal (region-to-string) "?-"))
	       (setq continue 0)
	       (setq return ""))
	      ((string-equal current-token "eof")
	       (setq continue 0)
	       (setq return ""))
	      (t (cond ((error-occurred (setq current-token (next-token)))
		     (setq continue 0)
		     (setq return ""))))
	 )
     )	
    (set-mark (point))
    (goto-char saved-mark)
    (exchange-dot-and-mark)
    return
))

; ----------------------------------------------------------------------
;                          Goal History
;                          Author: Sitaram Muralidhar
;                          Date  : 4/18/89
; ----------------------------------------------------------------------

;;
;; prolog-history-command-map - the key bindings available within the
;; the mini-buffer when goal-history is called.
;;
;;
(defvar prolog-history-command-map (copy-alist minibuffer-local-map))
(define-key prolog-history-command-map "\ep" 'previous-goal)
(define-key prolog-history-command-map "\en" 'next-goal)

(defun goal-history (arg)
  "Edit and re-evaluate last prolog goal, or ARGth from last.
The goal is placed in the minibuffer as a string for editing.
The result is executed, repeating the goal as changed.
If the goal has been changed or is not the most recent previous goal
it is added to the front of the goal history.
Whilst editing the goal, the following commands are available:
\\{prolog-history-command-map}"
  (interactive "p")
  (cond ((< (point) (process-mark 
		      (get-buffer-process "*prolog*")))
	 (end-of-line)
	 (let ((index 1))
	   (while (> index 0)
		     (cond ((error-occurred (search-backward  "| ?- "))
			    (message  "Cannot find a valid query to grab")
			    (setq index 0)
			    (beginning-of-line))
			   (t (re-search-forward " ?- ")
			      (cond ((string-equal
				       (setq goal 
					     (save-excursion
					       (valid-line))) "")
				     (beginning-of-line))
				    (t (place-in-prolog-buffer goal)
				       (setq index 0))))
		     )
	   )
	  )
	 )
	(t (let ((goal (nth (1- arg) prolog-goal-history)))
	     (setq newgoal (read-from-minibuffer "| ?- "
						 (prin1-to-string goal)
						 prolog-history-command-map
						 t))
;;  The new sequence of goals need not be added to the history
;;  since it would be added by prolog-newline (qprocess.el) anyway.
;;    (or (equal newgoal (car prolog-goal-history))
;;	(setq prolog-goal-history (cons newgoal prolog-goal-history)))
;;
;;
	     (place-in-prolog-buffer newgoal)
           )
	 )
   )
)

;;
;;  Place string (goal) in buffer (prolog)
;;  The goal sequence is placed in the *prolog* buffer and when a RET
;;  is entered "prolog-newline" is invoked.

(defun place-in-prolog-buffer (goal)
    (set-buffer "*prolog*")
    (goto-char (point-max))
    (insert-string goal)
)
  
(defun next-goal (n)
  "Inserts the next element of `prolog-goal-history' into the minibuffer."
  (interactive "p")
    (let ((narg (min (max 1 (- arg n)) (length prolog-goal-history))))
    (if (= arg narg)
	(error (if (= arg 1)
		   "No following item in goal history"
		   "No preceding item in goal history"))
      (erase-buffer)
      (setq arg narg)
      (insert (prin1-to-string (nth (1- arg) prolog-goal-history)))
      (goto-char (point-min)))))

(defun previous-goal (n)
  "Inserts the previous element of `prolog-goal-history' into the minibuffer."
  (interactive "p")
  (next-goal (- n)))



;; Given a "string", search through prolog's goal history for a goal
;; which contains "string" as a substring; Place this goal in its
;; entire form in the minibuffer and ask for conformation ; If not the
;; desired goal then continue to search backward for next goal which
;; contains "string". Made minor changes to definition of
;; repeat-matching-complex-command from chistory.el in the GNU Emacs
;; distribution. 

(defun repeat-matching-goal-command (&optional pattern)
  "Re-evaluate Prolog goal with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you
select a form for evaluation.  If PATTERN is empty (or nil), every form
in the goal history is offered."
  (interactive "sRedo Goal (regexp): ")
  (if pattern
      (if (equal (setq pattern
                       (substring pattern
                                  (or (string-match "[ \t]*[^ \t]" pattern)
                                      (length pattern))))
                 "")
          (setq pattern nil)))
  (let ((history prolog-goal-history)
        (temp)
        (what))
    (while (and history (not what))
      (setq temp (car history))
      (if (and (or (not pattern) 
		   (string-match pattern temp))
               (y-or-n-p (format "| ?- %s? " 
				 (setq temp (prin1-to-string temp)))))
          (setq what (car history))
        (setq history (cdr history))))
    (if (not what)
        (error "Prolog goal history exhausted.")
      (place-in-prolog-buffer what))))


; ----------------------------------------------------------------------

(defvar *term-reading-mode-before-^C* nil)

;;;  Used to be control-c-interrupt
(defun interrupt-prolog ()
  (interactive)
	(&clear-message)
	    (progn
	      (setq *term-reading-mode-before-^C* *prolog-term-reading-mode*)
	      (setq *prolog-term-reading-mode* nil)
	      (setq @at-debugger-prompt nil)
	      (interrupt-process nil t)))
	
(defun @restore-term-reading-mode ()
  (setq *prolog-term-reading-mode*
	*term-reading-mode-before-^C*))

; ---------------------------------------------------------------------
; 		  	   Find Definition
; ---------------------------------------------------------------------

(defvar *functor* 0)
(defvar *arity* 0)
(defvar *env*)
(defvar *print-name* "")
(defvar *already-saw-last-file* t)
(defvar *called-from-@find* nil)
(defconst *NoArity* -1)
(defvar *prolog-term-reading-mode* t)

(defun region-to-string ()
  (buffer-substring (min (point) (mark)) (max (point) (mark))))

(defun safe-to-find-with-prolog ()
   (and (or *prolog-term-reading-mode*
 	   @at-debugger-prompt)
        (dot-process-output-marker-ok)
   )
)



(defun find-definition  ()
  (interactive)
  (let (token-type token)
    (@fd-clear)
    (if (not (safe-to-find-with-prolog))
        (progn 
	  (pop-to-buffer "*prolog*")
	  (error 
           "Cannot use ""find-definition"" unless Prolog is at top level prompt")
          )
      )

    (save-excursion 	;; # added for FCP 
      (if (not (re-search-backward "[][?\001- \"%(#),{|}?\177]" nil t))
          (beginning-of-buffer)
	(forward-char)
        )
      (condition-case nil 
          (let (token1 token1-type)
            (setq token-type (next-token))
            (setq token (region-to-string))
            (cond 
             ((string-equal token-type "atom")
              (setq *functor* token)
              (save-excursion 
                (setq token1-type (next-token))
                (setq token1 (region-to-string))
                (if (string-equal token1 "/")
                    (progn 
                      (setq token1-type (next-token))
                      (setq token1 (region-to-string))
                      (if (string-equal token1-type "integer")
                          (setq *arity* (string-to-int token1))
                        (setq *arity* -1)
                        )
                      )
                  (setq *arity* -1)
                  )
                )
              (if (= *arity* -1)
                  (progn 
                    (setq *arity* 0)
                    (error-occurred
                     (next-token)
                     (if (string-equal (region-to-string) "-->")
                         (setq *arity* (+ *arity* 2)
                               )
                       )
                     )
                    )
                ))
             ((string-equal token-type "functor")
              (setq *functor* token)
              (condition-case nil
                  (let ()
                    (setq *arity* (head-arity))
                    (error-occurred
                     (next-token)
                     (if (string-equal (region-to-string) "-->")
                         (setq *arity* (+ *arity* 2)
                               )
                       )
                     )
                    )
                (error (setq *arity* *NoArity*)))
              )
             (t 
              (setq *functor* "")
              (setq *arity* -1)
              )
             )
            )
        (error                          ; Handler for error
         (setq *functor* "")
         (setq *arity* -1)
         )
        )
      )
    (query-user)
    (let ((mess
           (concat "Please Wait, looking for predicate: "
                   *functor*
                   (if (= *arity* *NoArity*)
                       ""
                     (concat "/" (int-to-string *arity*))
                     )
                   "..."
                   )
           ))
      (message mess)
      (sit-for 0)
                                        ;   (&qp-message mess)
      )
    (get-predicate-files)
    )
  )

(defun get-prolog-buffers ()
  (let ((buffers (buffer-list))
        (prolog-mode-buffers))
    (while buffers
      (set-buffer (car buffers))
      (if (eq major-mode 'prolog-mode)
          (setq prolog-mode-buffers (cons (car buffers) prolog-mode-buffers)))
      (setq buffers (cdr buffers)))
      prolog-mode-buffers))

(defun get-predicate-files ()
  (if (not (get-process "prolog"))
      (progn
        (setq *print-name*  (if (= *arity* -1)
                                *functor*
                              (concat *functor* "/" *arity*)))
        (let ((prolog-buffers (get-prolog-buffers))
              (no-good t))
          (while (and prolog-buffers no-good)
            (set-buffer (car prolog-buffers))
            (if (string-equal
                 ""
                 (locate-definition *functor* *arity* *print-name*))
                (progn
                  (pop-to-buffer (car prolog-buffers))
		  
                  (setq no-good nil))
              (setq prolog-buffers (cdr prolog-buffers))))
          (if no-good
              (conditional-message 
               (concat "Definition for " *print-name* " not found")))))
    (send-prolog
     (concat
      "find_predicate1(("
      *functor*
      "),"
      (if (= *arity* *NoArity*)
          "NoArity"
        (int-to-string *arity*)
        )
      ")"
      )
     )
    )
  )
  
(defun parse-*functor*-and-*arity*  (&optional string)
  (let ((buf (get-buffer-create "*temp*"))
        token-type token)
    (if (not string)
        (setq string (read-string "Name/Arity: ")))
    (save-excursion 
      (set-buffer buf)
      (widen)
      (erase-buffer)
      (insert-string string)
      (beginning-of-buffer)
      (setq token-type (next-token))
      (setq token (region-to-string))
      (if (string-equal token-type "atom")
          (progn 
            (setq *functor* token)
            (setq token-type (next-token))
            (setq token (region-to-string))
            (cond 
             ((string-equal token-type "eof")
              (setq *arity* *NoArity*))
              ((not (string-equal token "/"))
               (error 
                (concat "Name and arity must be separated by a '/': " token)))
              (t
               (setq token-type (next-token))
               (setq token (region-to-string))
               (if (string-equal token-type "integer")
                   (progn
                     (setq *arity* (string-to-int token))
                     (setq token-type (next-token))
                     (setq token (region-to-string))
                     (if (not (string-equal token-type "eof"))
                         (error "Extra tokens after arity will be ignored")
                       )
                     )
                 (error "Arity must be an integer: " token)
                 )
               )
              )
            )
        (error (concat "Functor must be an atom: " token))
        )
      )
    ))


(defun query-user  ()
  (let (user-response)
    (setq user-response
	  (read-from-minibuffer "Find (Name/Arity): "
				(if (string-equal *functor* "")
				    ""
				  (concat 
				   *functor*
				   (if (= *arity* *NoArity*)
				       ""
				     (concat "/" (int-to-string *arity*))
				     )
				   )
				  )
				)
          )
    (if (not (string-equal user-response ""))
	(parse-*functor*-and-*arity* user-response)
      )
    ))

(defun query-user-for-predicate  (message)
  (let (user-response)
    (setq user-response
	  (read-from-minibuffer message
				(if (string-equal *functor* "")
				    ""
				  (concat 
				   *functor*
				   (if (= *arity* *NoArity*)
				       ""
				     (concat "/" (int-to-string *arity*))
				     )
				   )
				  )
				)
          )
    (if (not (string-equal user-response ""))
	(parse-*functor*-and-*arity* user-response)
      )
    ))


; ----------------------------------------------------------------------

(defun @find  (&optional flag env)
  (setq flag (or flag (read-string "Flag: ")))
  (setq *env* (or env read-string "Env: "))
  (setq *print-name* 
        (if (= *arity* -1)
            *functor*
          (concat *functor* "/" *arity*))
	)
  (cond 
   ((string-equal flag "built_in")
    (&qp-message (concat *print-name* " is a built-in predicate")))
   ((string-equal flag "undefined")
    (&qp-message (concat *print-name* " is undefined")))
    ((string-equal flag "none")	
     (&qp-message (concat *print-name* " has no file(s) associated with it")))
     ((string-equal flag "ok")
      (setq *already-saw-last-file* nil)
      (setq *called-from-@find* t)
      (find-more-definition))
     (t &qp-message (concat "Find definition error: " flag))
	)
)


(defun find-more-definition  ()
  (interactive)
  (if *already-saw-last-file*
      (conditional-message "find-definition ""ESC ."" must be used first")
    (if (fd-buffer-empty)
        (progn 
          (setq *already-saw-last-file* t)
          (conditional-message 
           (concat *print-name* " has no more source files")))
      (let ((fmd-file-name (fd-get-filename)) fmd-message)
        (if (string-equal fmd-file-name "user")
            (setq fmd-message (concat *print-name*
                                      " was defined in pseudo-file 'user'"))
          (progn
            (condition-case nil
                (let () 
                  (find-file fmd-file-name)
                  (setq fmd-message
                        (locate-definition *functor* *arity* *print-name*))
                  (if (string-equal *env* "debug")
                      (pop-to-buffer "*prolog*" nil))
                  )
              (error
               (setq fmd-message
                     (concat *print-name*
                             " was defined in "
                             fmd-file-name 
                             ", but the file no longer exists")))
              )
            )
          )
        (if (fd-buffer-empty)
            (if (string-equal fmd-message "")
                (setq fmd-message " ")
              )
          (if (string-equal fmd-message "")
              (setq fmd-message "Type ""ESC ,"" for more")
            (setq fmd-message
                  (concat fmd-message ", type ""ESC ,"" for more"))
            )
          )
        (conditional-message fmd-message)
        )
      )
    )
  (setq *called-from-@find* nil)
  )

(defun conditional-message (message)
  (if *called-from-@find*
      (&qp-message message)
    (progn 
      (message message)
	    (sit-for 0)
	)
    )
)


(defun @fd-clear ()
  (let ((buf (get-buffer-create "*find-def*")))
    (save-excursion
      (set-buffer buf)
      (widen)
      (erase-buffer)
      )
    )
  )

(defun @fd-in (file)
  (save-excursion
    (set-buffer "*find-def*")
    (end-of-buffer)
    (insert-string (concat file "\n"))
    )
  )

(defun fd-get-filename ()
  (let (ans)
    (save-excursion
      (set-buffer "*find-def*")
      (goto-char (point-min))
      (forward-char)
      (search-forward "\"")
      (backward-character)
      (setq *functor* (buffer-substring (+ (point-min) 1) (point)))
      (forward-character)
      (forward-character)
      (delete-region (point-min) (point))
      (search-forward " ")
      (backward-character)
      (setq *arity* (string-to-int (buffer-substring (point-min) (point))))
      (forward-character)
      (delete-region (point-min) (point))
      (end-of-line)
      (setq ans (buffer-substring (point-min) (point)))
      (beginning-of-line)
      (kill-line)
      (kill-line)
      ans
    )
))


(defun fd-buffer-empty ()
  (save-excursion
    (set-buffer "*find-def*")
    (= (buffer-size) 0)
    )
  )

; ----------------------------------------------------------------------

(defun locate-definition (&optional functor arity print-name)

  (if (not functor) (setq  functor (read-string "Functor: ")))
  (if (not arity) (setq  arity (read-string "Arity: ")))
  (if (not print-name) (setq print-name (read-string "Print Name: ")))

  (let ((continue t)
        (found-arity 0) (saved-point (point)) return)
    (goto-char (point-min))
    (while continue
      (if (not (re-search-forward (concat "^'?" functor "'?") nil t))
          (progn 
	    (goto-char saved-point)		      
	    (setq return
                  (concat "Cannot find a definition for " 
                          print-name 
                          " in this file"))
	    (setq continue nil)
            )
        (if (not (within-comment))
            (let  (valid-arity (saved-dot (point)))
              (cond 
               ((looking-at "[A-Za-z0-9_]")
                (setq valid-arity nil))
               ((= (following-char) ?\( )
                (setq valid-arity
                      (condition-case nil
                          (progn (setq found-arity (all-arity saved-dot)) t)
                        (error nil))
                      ))
               (t
                (setq found-arity 0)
                (setq found-arity
                      (+ found-arity
                         (arity-overhead-for-grammar-rule saved-dot)))
                (setq valid-arity t)
                )
               )
              (if valid-arity
                  (if (or (= arity found-arity) (= arity *NoArity*))
                      (progn 
			(goto-char saved-dot)
			(beginning-of-line)
			(setq return "")
			(setq continue nil)
                        )
		    (goto-char saved-dot)
                    )
		(goto-char saved-dot)
                )
              )
          )
        )
      )
    (if (string-equal return "") (push-mark saved-point))
    return)
  )

; ---------------------------------------------------------------------
; 		  	   Change Directory
; ---------------------------------------------------------------------

;;
;  Trap M-x cd, pass all others
;;

(defun meta-x-trap (cmd)
  (interactive "CM-x ")
  (cond ((string-equal cmd "cd")
	 (call-interactively 'prolog-cd))
	(t (call-interactively cmd))
  )
)


(defun prolog-cd  (cd-path)
  (ensure-prolog-is-running)
  (interactive "DChange default directory: ")
  (cond
    ((not (safe-to-load-code))
     (error "Prolog is not at the top-level prompt"))
    (t
      (if (string-equal cd-path "")
	  (setq cd-path (getenv "HOME"))
	(sit-for 0)
	(&no-message)
	(send-prolog (concat "unix(cd('" cd-path "'))")))))
)


;
; This function is only called by Prolog
;

(defun @cd  (path prolog-success)
  (cond ((not (zerop prolog-success))
	 (condition-case nil 
	     (progn
	       (cd path)
	       (&qp-message
		 (concat "Current directory now: " path)))
	   (error
	     (&qp-message
	      (concat 
	      "Prolog did, but Emacs did not change current directory to: " 
		path)
	      )
	   )
	  )                  ; ends condition-case
	 )
	(t (&qp-message
	     (concat 
	       "Neither Prolog nor Emacs changed current directory to: " 
	       path)
	   )
	)
   )
)  

; ---------------------------------------------------------------------
; 		  	   Library
; ---------------------------------------------------------------------

;The command "<ESC>-x library" supports the library directory package 
; of version 1.5

(defun library  ()
  (interactive)
  (cond ((bufferp (get-buffer "*prolog*"))
	 (if (safe-to-load-code)
	     (let* ((lib-file (read-string "Library name: "))
		    (mess (concat
			    "Please Wait, looking for library file: "
			    lib-file
			    "..." )))
	       (message mess)
	       (sit-for 0)
	       (&qp-message mess)
	       (send-prolog (concat "find_library_package((" lib-file "))"))
	       )
	     (progn	
	       (error 
		 "Cannot use ""library"" unless Prolog is at top level prompt")
	       )
	     )
	 )
	((bufferp (get-buffer "*qui-emacs*"))
	 (error "Emacs Invoked from QUI: library not a valid command"))
	(t (error "Invalid command")))
  )
	
(defun @lib  (lib-file)
  (if (string-equal
       lib-file
       "cannot find library file, check facts for library_directory/1")
      (&qp-message lib-file)
    (progn		
      (pop-to-buffer "*prolog*")
      (find-file-other-window lib-file)
      (&clear-message)
      )
    )
  )

(defun @remove-gc-tick-mark (tick)
  (save-excursion 
    (set-buffer "*prolog*")
    (goto-char (point-max))
    (if (= (preceding-char) tick))
    (delete-previous-character)
    (&qp-message "Gc tick mark missing from ""qprolog"" buffer")
    )
  )

(defvar @at-debugger-prompt nil)

(defun @debug ()
  (setq *prolog-term-reading-mode* nil)
  (setq @at-debugger-prompt t)
  )

(defun spy ()
  (interactive)
  (@fd-clear)
  (ensure-prolog-is-running)
  (if (not (safe-to-find-with-prolog))
      (progn 
	(pop-to-buffer "*prolog*")
	(error 
	 "Cannot use ""spy"" unless Prolog is at top level prompt")
	)
    )
  (get-current-predicate)
  (query-user-for-predicate "Spy (Name/Arity): ")
  (pop-to-buffer "*prolog*")
  (send-prolog (concat "spy " *functor* 
		       (if (< *arity* 0) ""
			 (concat "/" (int-to-string *arity*)))
		       )
	       )
  )

(defun nospy ()
  (interactive)
  (@fd-clear)
  (ensure-prolog-is-running)
  (if (not (safe-to-find-with-prolog))
      (progn 
	(pop-to-buffer "*prolog*")
	(error 
	 "Cannot use ""spy"" unless Prolog is at top level prompt")
	)
    )
  (get-current-predicate)
  (query-user-for-predicate "Spy (Name/Arity): ")
  (pop-to-buffer "*prolog*")
  (send-prolog (concat "nospy " *functor* 
		       (if (< *arity* 0) ""
			 (concat "/" (int-to-string *arity*)))
		       )
	       )
  )

(defun get-current-predicate ()
  (let (token-type token)
    (save-excursion;; # added for FCP 
      (if (not (re-search-backward "[][?\001- \"%(#),{|}?\177]" nil t))
	  (beginning-of-buffer)
	(forward-char)
	)
      (condition-case nil 
	  (let (token1 token1-type)
	    (setq token-type (next-token))
	    (setq token (region-to-string))
	    (cond 
	     ((string-equal token-type "atom")
	      (setq *functor* token)
	      (save-excursion 
		(setq token1-type (next-token))
		(setq token1 (region-to-string))
		(if (string-equal token1 "/")
		    (progn 
		      (setq token1-type (next-token))
		      (setq token1 (region-to-string))
		      (if (string-equal token1-type "integer")
			  (setq *arity* (string-to-int token1))
			(setq *arity* -1)
			)
		      )
		  (setq *arity* -1)
		  )
		)
	      (if (= *arity* -1)
		  (progn 
		    (setq *arity* 0)
		    (error-occurred
		     (next-token)
		     (if (string-equal (region-to-string) "-->")
			 (setq *arity* (+ *arity* 2)
			       )
		       )
		     )
		    )
		))
	     ((string-equal token-type "functor")
	      (setq *functor* token)
	      (condition-case nil
		  (let ()
		    (setq *arity* (head-arity))
		    (error-occurred
		     (next-token)
		     (if (string-equal (region-to-string) "-->")
			 (setq *arity* (+ *arity* 2)
			       )
		       )
		     )
		    )
		(error (setq *arity* *NoArity*)))
	      )
	     (t 
	      (setq *functor* "")
	      (setq *arity* -1)
	      )
	     )
	    )
	(error				; Handler for error
	 (setq *functor* "")
	 (setq *arity* -1)
	 )
	)
      )
    )
  )

