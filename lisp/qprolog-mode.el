;;;  SCCS: @(#)91/01/11 qprolog-mode.el    3.9
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

;;;
;;;  qprequire - suppresses message printing but offers the same
;;;  functionality as require.
;;;

;;;  This file is slightly modified version for proper cooperation with
;;;  SWI-Prolog:
;;;
;;;	* Prompt has changed
;;;	* The keybindings for 'prolog-compile are left out as this does	
;;;	  not yet work
;;;	* prolog-mode runs the 'prolog-mode-hook
;;;	* run-prolog runs the 'inferior-prolog-mode-hook


(cond ((null (car global-mode-string))
       (setq global-mode-string (list ""))))
(defvar original-mode-string global-mode-string
  "The default mode line string when prolog starts up. Note that if the
mode string is changed after prolog is invoked the new setting will be lost")

(defun qprequire (file)
  (cond ((not (featurep file))
	 (load (prin1-to-string file) nil t))
	(t t))
)

(defvar prolog-prompt-pattern "^| \\?- " ; SWI
  "Regexp to detect prompt (toplevel)")

(defvar prolog-zap-file (make-temp-name "/tmp/qp")
  "Temporary file name used for code being consulted or compiled in Prolog.")

(defvar prolog-goal-history nil
  "List of recent goals executed by Quintus prolog.")

(defvar multiline-goal nil
  "Multiline goal executed by Quintus prolog.")

(defvar prolog-mode-syntax-table nil
  "Syntax table used while in prolog mode.")

(defvar prolog-mode-abbrev-table nil "")

(defvar quintus-gnu-version "18.55/3.1" 
 " Version numbers of this version of Quintus-GNU emacs interface.")

(define-abbrev-table 'prolog-mode-abbrev-table ())

(defvar prolog-mode-map nil)
(qprequire 'quintaux)
(if prolog-mode-map 
    nil
  (setq prolog-mode-map (make-sparse-keymap))
  (define-key prolog-mode-map "\t"     'prolog-indent-line       )
  (define-key prolog-mode-map "\e\C-q" 'prolog-indent-clause     )
  (define-key prolog-mode-map "\e\C-a" 'beginning-of-clause      )
  (define-key prolog-mode-map "\eh"    'mark-clause              )
  (define-key prolog-mode-map "\ef"    'forward-prolog-word      )
  (define-key prolog-mode-map "\eb"    'backward-prolog-word     )
  (define-key prolog-mode-map "\e\C-f" 'forward-term             )
  (define-key prolog-mode-map "\e\C-b" 'backward-term            )
  (define-key prolog-mode-map "\ed"    'kill-prolog-word         )
  (define-key prolog-mode-map "\e\177" 'backward-kill-prolog-word)
  (define-key prolog-mode-map "\e\C-k" 'kill-clause              )
  (define-key prolog-mode-map "\e\C-e" 'end-of-clause            )
  (define-key prolog-mode-map "\e."    'find-definition          )
  (define-key prolog-mode-map "\e,"    'find-more-definition     )
; (define-key prolog-mode-map "\e\C-x" 'prolog-consult-predicate ) ;SWI
; (define-key prolog-mode-map "\ek"    'prolog-compile           ) ;SWI
; (define-key prolog-mode-map "\ei"    'prolog-compile           ) ;SWI
  (define-key prolog-mode-map "\e#"    'shell-filename-complete  )
)

(fset 'prolog-mode 'prolog-mode) 

(defun quintus-version ()
  (interactive)
  (message "Quintus-Gnu Emacs interface version %s" 
	   quintus-gnu-version)
)
  

(defun prolog-mode ()
  "Major mode for editing files of prolog code.
 The following commands are available:
 \\{prolog-mode-map}."

  (interactive)
  (kill-all-local-variables)
  (use-local-map prolog-mode-map)
  (setq mode-name "prolog")
  (setq major-mode 'prolog-mode)
  (setq local-abbrev-table prolog-mode-abbrev-table)
  (or (mark) (set-mark 0))
  (ensure-prolog-syntax)
  (prolog-mode-variables)
  (run-hooks 'prolog-mode-hook))	; SWI

(defun ensure-prolog-syntax ()
  "Hack to make sure Prolog syntax table is set up properly"
  ; PMartin 18 Mar 88
  (if (null prolog-mode-syntax-table)
      (progn
	(setq prolog-mode-syntax-table (make-syntax-table))
	(set-syntax-table prolog-mode-syntax-table)
	(modify-syntax-entry ?\( "()  ")
	(modify-syntax-entry ?\) ")(  ")
	(modify-syntax-entry ?\{ "(}  ")
	(modify-syntax-entry ?\} "){  ")
	(modify-syntax-entry ?\[ "(]  ")
	(modify-syntax-entry ?\] ")[  ")
	(modify-syntax-entry ?\_ "_   ")
	(modify-syntax-entry ?\% "<   ")
	(modify-syntax-entry ?\n ">   ")
	(modify-syntax-entry ?\" "\"  ")
	(modify-syntax-entry ?\\ "\\  ")
	(modify-syntax-entry ?/ "  14")
	(modify-syntax-entry ?* "  23")
	(modify-syntax-entry ?' "w   "))
    ))

(defun prolog-mode-commands (map)
  (define-key map "\C-m" 'prolog-newline)
  (define-key map "\e\C-y" 'command-pop)
  (define-key map "\e\C-f" 'forward-term)
  (define-key map "\e\C-b" 'backward-term)
  (define-key map "\e." 'find-definition)
  (define-key map "\e," 'find-more-definition)
  (define-key map "\ex" 'meta-x-trap)
  (define-key shell-mode-map "\C-c\C-c" 'interrupt-prolog)
  (define-key map "\C-x\C-e" 'goal-history)
  (define-key map "\C-x\C-y" 'repeat-matching-goal-command)
  (define-key map "\C-d"  'qp-kill-character)
  (define-key map "\ed"   'qp-kill-word)
  (define-key map "\eD"   'qp-kill-word)
  (define-key map "\e\177"  'qp-backward-kill-word)
  (define-key map "\C-c\C-w"  'qp-backward-kill-word)
  (define-key map "\C-x\177"  'qp-backward-kill-sentence)
  (define-key map "\ek"  'qp-kill-sentence)
  (define-key map "\177"  'qp-backward-kill-character)
  (define-key map "\C-k"  'qp-kill-lines)
  (define-key map "\C-w"  'qp-kill-region))

(defun prolog-mode-variables ()
  (set-syntax-table prolog-mode-syntax-table)
  (setq local-abbrev-table prolog-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'prolog-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 50)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'prolog-comment-indent))

(defvar inferior-prolog-mode-map nil)

(qprequire 'shell)
(qprequire 'qpshell-command-ring)

(if inferior-prolog-mode-map
    nil
  (setq inferior-prolog-mode-map (copy-alist shell-mode-map))
  (prolog-mode-commands inferior-prolog-mode-map))


(defun inferior-prolog-mode ()
    "Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{inferior-prolog-mode-map}

Entry to this mode calls the value of prolog-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
prolog-mode-hook is called after shell-mode-hook.

You can send text to the inferior Prolog from other buffers
using the commands send-region, send-string.

Commands:
Delete converts tabs to spaces as it moves back.
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Meta-Control-Q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Percent(%) start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the shell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from shell.
C-x C-v puts top of last batch of output at top of window."
    (interactive)
    (kill-all-local-variables)
    (setq major-mode 'inferior-prolog-mode)
    (setq mode-name "Prolog")
    (setq mode-line-format 
          "--%1*%1*-Emacs: %12b   %M          %[(%m: %s)%]----%3p--%-")
    (prolog-mode-variables)
    (use-local-map inferior-prolog-mode-map)
    (make-local-variable 'last-input-start)
    (setq last-input-start (make-marker))
    (make-local-variable 'last-input-end)
    (setq last-input-end (make-marker))
    (make-local-variable '@at-debugger-prompt)
    (setq  @at-debugger-prompt nil)
    (run-hooks 'shell-mode-hook 'inferior-prolog-mode-hook)) ; SWI

(defvar startup-jcl (concat " +C" " Emacs:" prolog-zap-file)
  "String that identifies that emacs is the sender")
(defvar *prolog-executable* nil
  "The prolog executable")
(defvar *prolog-flags* nil
  "Prolog command line switches")

(defun run-prolog (&optional gnu-context)
    "Run an inferior Prolog process, input and output via buffer
*prolog*.  Environment variable QUINTUS_PROLOG_PATH must be set to the
pathname of the prolog executable before invoking this function
interactively. Optional first argument means fire up named save-state;
Called by GNU Emacs 'recover-context'. Gnu-context is the name of the
prolog saved-state."
    (interactive)
    (ensure-prolog-syntax)
    (qprequire 'shell)
    (cond (gnu-context
	   (setq *prolog-executable* gnu-context))
	  (t (if (getenv "QUINTUS_PROLOG_PATH")
		 (let  
		     ((prolog-command-string (concat 
					       (getenv "QUINTUS_PROLOG_PATH")
					       startup-jcl)))
		   (get-prolog-exec-and-flags prolog-command-string))
	       (setq *prolog-executable* "")))
    )
    (cond ((string-equal *prolog-executable* "")
	   (message "Environment variable QUINTUS_PROLOG_PATH not set"))
	  (t (switch-to-buffer  (apply 'make-shell "prolog"
				       *prolog-executable* nil  
					*prolog-flags*))
	     (set-process-filter (get-process "prolog") 'prolog-process-filter)
	     (sleep-for 2)
	     (inferior-prolog-mode)
	     (error-occurred (prolog-startup-hook))
	  )
    )
)

;---------------------------------------------------------------------
; Separates the executable from rest of args (to prolog)
;---------------------------------------------------------------------
(defun get-prolog-exec-and-flags (prolog-command-string)
  (let ((i 1))
    (while (not (string-equal 
		  (substring prolog-command-string i (+ i 1))
		  " "))
      (setq i (+ i 1)))
    (setq *prolog-executable* (substring prolog-command-string 0 i))
    (setq *prolog-flags* (prolog-args (substring prolog-command-string
						 (+ i 1))))
  )
)
;---------------------------------------------------------------------
; Breaks up a single string of args into individual strings
;---------------------------------------------------------------------
(defun prolog-args (prolog-command-string)
  (let ((argnum 1)
	(arg-list nil)
	(done t))
    (while done
      (cond ((not (string-equal (setq arg
				      (get-arg prolog-command-string argnum))
				"")
	     )
	     (set-variable (intern (concat "arg" "_" argnum)) arg)
	     (setq arg-list (cons arg arg-list))
	     (setq argnum (+ argnum 1)))
	    (t (setq done nil))
      )
    )
  (nreverse arg-list)
  )
)

; ----------------------------------------------------------------------
; get-arg returns the arg-pos'th command string from prolog-command-string
; ---------------------------------------------------------------------- 
(defun get-arg (prolog-command-string arg-pos)
  (let ((i 0)
	(j 0)
	(done t)
	(arg arg-pos)
	(len (length prolog-command-string)))
    (while (and (/= arg 0)
		 done)
      (while (and (< i len)
		  (string-equal (substring prolog-command-string i (+ i 1))
				" "))
	(setq i (+ i 1))
	(setq j (+ j 1))
      )
      (while (and (< i len)
		  (not (string-equal 
			 (substring prolog-command-string i (+ i 1))
			 " ")
		  )
	     )
	(setq i (+ i 1))
      )
      (cond ((>= i len)
	     (setq done nil))
      )
      (setq arg (1- arg))
      (cond ((/= arg 0)
	      (setq j i))
      )
    )
    (substring prolog-command-string j i)
  )
)
	

(defmacro prolog-in-other-window ()
  (if (equal (get-lru-window) (selected-window))
      (split-window-vertically nil))
  (pop-to-buffer "*prolog*"))

;;
;;
; This function no longer used, see prolog-newline (qprocess.el) instead
;;
;;
(defun send-to-prolog ()
  "Send input to subshell.
At end of buffer, sends all text after last output
 as input to the subshell, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of shell-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (end-of-line)
  (cond ((or (null *prolog-term-reading-mode*)
	     (clause-end-p))
  (if (eobp) 
      (progn
        (move-marker last-input-start
                     (process-mark (get-buffer-process (current-buffer))))
        (insert ?\n)
        (move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward prolog-prompt-pattern nil t)
    (let ((copy (buffer-substring (point)
				  (progn (end-of-clause 1) 
                                         (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (insert ?\n))
    (move-marker last-input-end (point)))
  (check-for-module-change last-input-start last-input-end)
  (save-command-in-ring last-input-start last-input-end)
;;
;; The following "cond" has been added to facilitate goal-history
;; functionality. Nothing is added to goal-history if at debugger
;; prompt. The variable multiline goal keeps track of multi-line
;; queries. This is required since each line is sent to prolog 
;; separately. The variable is reset when the query is complete.
;;
  (cond ((null @at-debugger-prompt)
	 (save-excursion
	    (goto-char last-input-start)
	    (cond (( string-equal (setq current-goal (valid-line)) "")
		   (setq multiline-goal (concat multiline-goal
						(buffer-substring
						  last-input-start
						  (1- last-input-end)))))
		  ((not (null multiline-goal))
		   (setq multiline-goal (concat multiline-goal 
						current-goal))
		   (setq prolog-goal-history
			 (cons multiline-goal prolog-goal-history))
		   (setq multiline-goal nil))
		  (t (setq prolog-goal-history 
			   (cons current-goal prolog-goal-history))))))
   )
  (setq *prolog-term-reading-mode* nil)
  (setq @at-debugger-prompt nil)
  (let ((process (get-buffer-process (current-buffer))))
    (send-region process last-input-start last-input-end)
    (set-marker (process-mark process) (point))))))

(defun small-prolog-window ()
  (interactive)
    (cond
     ((get-buffer-window "*prolog*")
      (expand-window (- (screen-height) window-min-height)))
     (t (split-window-vertically (- (window-height) window-min-height))
        (other-window 1)
        (switch-to-buffer "*prolog*"))))

(defun big-prolog-window ()
  (interactive)
    (cond
     ((get-buffer-window "*prolog*")
      (expand-window (- (screen-height) window-min-height)))
     (t (split-window-vertically (- (window-height) window-min-height))
        (other-window 1)
        (switch-to-buffer "*prolog*"))))

(defun kill-prolog-word (arg)
  "Kill characters forward until encountering the end of a word.
Treats underscore as a word if it appears as an argument.
With argument, do this that many times."
  (interactive "*p")
  (kill-region (point) (progn (forward-prolog-word arg) (point))))

(defun backward-kill-prolog-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-prolog-word (- arg)))

(defun backward-term (arg)
  (interactive "p")
  (forward-term (- arg)))

(defun forward-term (arg)
  (interactive "p")
  (cond
   ((zerop arg) t)
   ((> arg 0)
    (forward-sexp 1)
    (if (= (following-char) ?\( )
	(forward-sexp 1))
    (forward-term (1- arg)))
   (t
    (forward-sexp -1)
    (if (= (following-char) ?\( )
	(forward-sexp -1))
    (forward-term (1+ arg)))))

(defun forward-prolog-word (arg)
  "Just like word forward, but it treats `_' like a word
if it is used in the position that it is a anynomous
variable"
  (interactive "p")
  (or arg (setq arg 1))
  (cond
   ((zerop arg) t)
   ((> arg 0)
    (if (looking-at "[(,]?\\s-*_\\s-*[),]")
        (progn
          (forward-char 1)
          (re-search-forward "[),]")
          (backward-char 1))
      (forward-word 1))
    (forward-prolog-word (1- arg)))
   (t
    (if (looking-at "_") (forward-char -1))
    (if (looking-at "\\s-\\|$") (re-search-backward "\\S-"))
    (cond
     ((looking-at "[,)]")
      (forward-char -1)
      (if (looking-at "\\s-") (re-search-backward "\\S-"))
      (if (not (looking-at "_"))
          (progn
            (forward-char 1)
            (forward-word -1))))
     ((looking-at "_") t)
     (t (forward-word -1)))
    (forward-prolog-word (1+ arg)))))

(defun backward-prolog-word (arg)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call forward-word with negative arg."
  (interactive "p")
  (forward-prolog-word (- arg)))






