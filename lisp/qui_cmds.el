; /ports/emacs/GNU/el3.1 @(#)qui_cmds.el	1.3 1/8/91 
; /ports/home/sitaram/Gnu/qui_emacs @(#)qui_cmds.el	1.8 11/13/90 
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

(defvar qui-zap-file (make-temp-name "/tmp/qui")
  "Temporary file name used for code being consulted or compiled in Qui.")
(defvar  " ")

(defun qui-compile ()
  (interactive)
  (cond
    ((string-equal (buffer-name) "*qui-emacs*")
     (error "Cannot load from the qui scratch buffer")))
  (qui-load "compile")
)

(defun qui-load  (pl-load-type)
  (let (pl-char )
    (cond 
     ((string-equal (buffer-file-name) "")
      (error "Cannot load from this buffer before it is written to a file"))
;;  Need to check this, currently just sending things blindly to emacs
     (t
      (sleep-for 1)
      (message 
       (concat 
        pl-load-type
        " Prolog... enter p for procedure, r for region or b for buffer "))
      (send-load-to-qui pl-load-type (read-char))
      )
     )
    )
  )

(defun send-load-to-qui  (sltp-load-type sltp-char)
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
	  (write-region (point) (mark) qui-zap-file)
	  )
        )
      (setq word2 "procedure")
;;; loadpred <size> filename <size> tmpfile
      (send-qui 
	(concat LOADPRED  (padded-length file-name) 
		file-name  (padded-length qui-zap-file)  qui-zap-file)))
     ((= sltp-char ?r)
      (write-region (point) (mark) qui-zap-file)
      (setq word2 "region")
;;; loadregi <size> filename <size> tmpfile
      (send-qui 
	(concat LOADREGI  (padded-length file-name) 
		file-name  (padded-length qui-zap-file)  qui-zap-file)))
     ((= sltp-char ?b)
      (save-excursion
        (mark-whole-buffer)
        (write-region (point) (mark) qui-zap-file)
      )
      (setq word2 "buffer")
;;; loadfile <size> filename
      (send-qui 
	(concat LOADFILE (padded-length file-name) file-name)))
     (t (error "Bad option"))
    )
    (message (concat word1 " " word2 "..."))
    (sit-for 0)
    (&clear-message)
  )
)

; ---------------------------------------------------------------------
; 		  	   Find Definition
; Most of this stuff is the same as the prolog-emacs interface, minor
; modifications have been made to accomodate the manner in which Emacs
; talks to QUI.
; ---------------------------------------------------------------------

(defvar   *qui-functor*               0)
(defvar   *qui-arity* 		      0)
(defvar   *qui-env*)
(defvar   *qui-print-name*           "")
(defvar   *qui-already-saw-last-file* t)
(defvar   *called-from-@find*       nil)
(defconst *QuiNoArity*               -1)


(defun find-qui-definition  ()
  (interactive)
  (let (token-type token)
    (@fd-clear)
;;; Currently blindly sending to qui
    (save-excursion 
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
              (setq *qui-functor* token)
              (save-excursion 
                (setq token1-type (next-token))
                (setq token1 (region-to-string))
                (if (string-equal token1 "/")
                    (progn 
                      (setq token1-type (next-token))
                      (setq token1 (region-to-string))
                      (if (string-equal token1-type "integer")
                          (setq *qui-arity* (string-to-int token1))
                        (setq *qui-arity* -1)
                        )
                      )
                  (setq *qui-arity* -1)
                  )
                )
              (if (= *qui-arity* -1)
                  (progn 
                    (setq *qui-arity* 0)
                    (error-occurred
                     (next-token)
                     (if (string-equal (region-to-string) "-->")
                         (setq *qui-arity* (+ *qui-arity* 2)
                               )
                       )
                     )
                    )
                ))
             ((string-equal token-type "functor")
              (setq *qui-functor* token)
              (condition-case nil
                  (let ()
                    (setq *qui-arity* (head-arity))
                    (error-occurred
                     (next-token)
                     (if (string-equal (region-to-string) "-->")
                         (setq *qui-arity* (+ *qui-arity* 2)
                               )
                       )
                     )
                    )
                (error (setq *qui-arity* *QuiNoArity*)))
              )
             (t 
              (setq *qui-functor* "")
              (setq *qui-arity* -1)
              )
             )
            )
        (error                          ; Handler for error
         (setq *qui-functor* "")
         (setq *qui-arity* -1)
         )
        )
      )
    (query-user)
    (let ((mess
           (concat "Please Wait, looking for predicate: "
                   *qui-functor*
                   (if (= *qui-arity* *QuiNoArity*)
                       ""
                     (concat "/" (int-to-string *qui-arity*))
                     )
                   "..."
                   )
           ))
      (message mess)
      (sit-for 0)
      )
    (get-predicate-files)
    )
  )

; Send 	"finddef <size> functor <size> arity <size> module" to QUI
; No Module information available, "_NoModule_" sent to QUI.

(defun get-predicate-files ()
  (send-qui (concat FINDDEF  (padded-length *qui-functor*) 
		      *qui-functor*  
		      (if (= *qui-arity* *QuiNoArity*)
			    (padded-length "-1") 
			    (padded-length (int-to-string *qui-arity*))
		      )
		      (if (= *qui-arity* *QuiNoArity*)
			  *QuiNoArity*
			  *qui-arity*
		      )
		      (padded-length NOMODULE)  NOMODULE
	      )
     )
)
  
(defun parse-*qui-functor*-and-*qui-arity*  (&optional string)
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
            (setq *qui-functor* token)
            (setq token-type (next-token))
            (setq token (region-to-string))
            (cond 
             ((string-equal token-type "eof")
              (setq *qui-arity* *QuiNoArity*))
              ((not (string-equal token "/"))
               (error 
                (concat "Name and arity must be separated by a '/': " token)))
              (t
               (setq token-type (next-token))
               (setq token (region-to-string))
               (if (string-equal token-type "integer")
                   (progn
                     (setq *qui-arity* (string-to-int token))
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
				(if (string-equal *qui-functor* "")
				    ""
				  (concat 
				   *qui-functor*
				   (if (= *qui-arity* *QuiNoArity*)
				       ""
				     (concat "/" (int-to-string *qui-arity*))
				     )
				   )
				  )
				)
          )
    (if (not (string-equal user-response ""))
	(parse-*qui-functor*-and-*qui-arity* user-response)
      )
    ))


(defun find-more-qui-definition  ()
  (interactive)
  (if *qui-already-saw-last-file*
      (conditional-message "find-definition ""ESC ."" must be used first")
    (if (fd-buffer-empty)
        (progn 
          (setq *qui-already-saw-last-file* t)
          (conditional-message 
           (concat *qui-print-name* " has no more source files")))
      (let ((fmd-file-name (fd-get-filename)) fmd-message)
        (if (string-equal fmd-file-name "user")
            (setq fmd-message (concat *qui-print-name*
                                      " was defined in pseudo-file 'user'"))
          (progn
            (condition-case nil
                (let () 
                  (find-file-other-window fmd-file-name)
                  (setq fmd-message
                        (locate-definition *qui-functor* *qui-arity* *qui-print-name*))
                  (if (string-equal *qui-env* "debug")
                      (pop-to-buffer "*prolog*" nil))
                  )
              (error
               (setq fmd-message
                     (concat *qui-print-name*
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
  (let ((buf (get-buffer-create "*qui-find-def*")))
    (save-excursion
      (set-buffer buf)
      (widen)
      (erase-buffer)
      )
    )
  )

(defun @fd-in (file)
  (save-excursion
    (set-buffer "*qui-find-def*")
    (end-of-buffer)
    (insert-string (concat file "\n"))
    )
  )

(defun fd-get-filename ()
  (let (ans)
    (save-excursion
      (set-buffer "*qui-find-def*")
      (beginning-of-buffer)
	(set-mark (point))
	(search-forward " ")
	(backward-char)
	(setq *qui-functor* (region-to-string))
	(forward-char)
	(delete-region (point) (mark))
	(set-mark (point))
	(search-forward " ")
	(backward-char)
	(setq *qui-arity* (string-to-int (region-to-string)))
	(forward-char)
	(delete-region (point) (mark))
	(set-mark (point))
	(end-of-line)
	(setq ans (region-to-string))
	(forward-char)
	(delete-region (point) (mark))
	ans
    )
))

(defun fd-buffer-empty ()
  (save-excursion
    (set-buffer "*qui-find-def*")
    (= (buffer-size) 0)
    )
  )

(defun locate-definition (&optional functor arity print-name)
  (if (not functor) (setq  functor (read-string "Functor: ")))
  (if (not arity) (setq  arity (read-string "Arity: ")))
  (if (not print-name) (setq print-name (read-string "Print Name: ")))
  (let ((continue t)
        (found-arity 0) (saved-point (point)) return)
    (goto-char (point-min))
    (while continue
      (if (not (re-search-forward (concat "^" functor) nil t))
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
                  (if (or (= arity found-arity) (= arity *QuiNoArity*))
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

;---------------------------------------------------------------------------
; Qui sends a list of Name-Arity-filename triples to emacs by calling
; founddef with the Functor, Arity, Module and Filename. On receiving
; an "enddef ", the "find" begins through the triples in *qui-find-def*.
;---------------------------------------------------------------------------

;;;
;;; Built-in definition
;;;

(defun builtin (functor arity module)
  (cond ((= (string-to-int arity) *QuiNoArity*)
	 (setq *qui-print-name* functor))
	(t (setq *qui-print-name* (concat functor "/" arity)))
  )
  (&qp-message (concat *qui-print-name* " is a built-in predicate"))
)

;;;
;;; No definition for predicate
;;;

(defun nondef (functor arity module)
  (cond ((= (string-to-int arity) *QuiNoArity*)
	 (setq *qui-print-name* functor))
	(t (setq *qui-print-name* (concat functor "/" arity)))
  )
  (&qp-message 
    (concat *qui-print-name* " has no file(s) associated with it"))
)

;;;
;;; Undefined predicate
;;; arg3 - functor, arg2 - arity, arg1 - module

(defun undef (functor arity module)
  (cond ((= (string-to-int arity) *QuiNoArity*)
	 (setq *qui-print-name* functor))
	(t (setq *qui-print-name* (concat functor "/" arity)))
  )
  (&qp-message (concat *qui-print-name* " is undefined"))
)

;;;
;;; Look for first definition of predicate, signaled by enddef
;;;

(defun enddef ()
  (setq *qui-already-saw-last-file* nil)
  (setq *called-from-@find* 1)
  (cond ((= *qui-arity* *QuiNoArity*)
	 (setq *qui-print-name* *qui-functor*))
	(t (setq *qui-print-name* (concat *qui-functor* "/" *qui-arity*))))
  (setq *qui-env* "")
  (find-more-qui-definition)
)

;;;
;;; edit-file - find-file file and goto-char pos
;;; arg2 is filename arg1 is pos
;;;

(defun edit-file (file pos)
  (find-file file)
  (cond ((= pos 0)
	 (goto-char (point-min)))
	(t (goto-char (1+ pos))))
)

;;;
;;; fill find defns buffer - create one if necessary and write triples
;;; to it, this is repeatedly called by QUI to fill in definitions.
;;; arg4 is functor, arg3 - arity, arg2 - module, arg1 - filename

(defun fill-defns (functor arity module filename)
  (get-buffer-create "*qui-find-def*")
  (let ((triple (concat functor space arity space filename)))
    (@fd-in triple)
  )
)

;;;
;;; Qui to quit
;;;

(defun qui-quit ()
  (cond ((get-buffer "*qui-find-def*")
	 (kill-buffer "*qui-find-def*")))
  (cond ((get-buffer "*temp*")
	 (kill-buffer "*temp*")))
  (message "Qui quitting, terminating qui-emacs interface ")
)

;;;
;;; Cantload
;;;

(defun cantload ()
  (message "Cannot load into prolog now")
  (sit-for 0)
)

;;;
;;; Cantccp
;;;

(defun cantccp ()
  (message "Cannot find definition now")
  (sit-for 0)
)
