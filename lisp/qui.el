; /ports/emacs/GNU/el3.1 @(#)qui.el	1.3 12/20/90 
; /ports/home/sitaram/Gnu/qui_emacs @(#)qui.el	1.6 11/13/90 
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


(defvar port 0 
 "The port number emacs will connect to")
(defvar hostname 0
 "The hostname to connect to")
(defvar qui-arg 0
 "The command argument sent by QUI")
(defvar *qui-packet-pending* nil
  "Processing the same packet ? ")
(defvar  *packet-length* 0
  "Length of curent command")
(defconst  space  " ")

;;;
;;; Emacs -> Qui commands
;;;
(defvar EMACSUP  "emacsup "
  "Qui-Emacs interface startup message")
(defvar EDFAILED "edfailed "
  "Qui-Emacs edit failed message")
(defvar FINDDEF  "finddef "
  "Qui-Emacs find definition")
(defvar LOADFILE "loadfile"
  "Qui-Emacs load -compile file")
(defvar LOADPRED "loadpred"
  "Qui-Emacs load -compile predicate")
(defvar LOADREGI "loadregi"
  "Qui-Emacs load -compile region")
(defvar NOMODULE "_NoModule_"
  "Qui-Emacs no module flag")
;;;
;;;  Qui -> Emacs commands
;;;
(defvar EDITFILE "editfile"
  "Qui-Emacs edit file command")
(defvar FOUNDDEF "founddef"
  "Qui-Emacs found requested definition")
(defvar ENDDEF   "enddef  "
  "Qui-Emacs found all definitions")
(defvar BLTINDEF "bltindef"
  "Qui-Emacs is a builtin predicate")
(defvar NONEDEF  "nonedef "
  "Qui-Emacs is not defined")
(defvar UNDEF    "undefdef"
  "Qui-Emacs is an undefined predicate")
(defvar CLEARDEF "cleardef"
  "Qui-Emacs clear file defn buffer")
(defvar CANTLOAD "cantload"
  "Qui-Emacs cannot load into prolog now")
(defvar CANTCCP  "cantccp "
  "Qui-Emacs cannot call prolog now")
(defvar QUIQUIT  "qui_quit"
  "Qui-Emacs qui has quit")
(defvar SYNSTART  "synstart"
  "Qui-Emacs qui has quit")
(defvar SYNEND    "synend  "
  "Qui-Emacs qui has quit")
;;;
;;; standard command length (number of separate strings)
;;;

(defvar  EDIT_FILE_LENGTH 2
  " Number of args in edit-file command")
(defvar	 FOUND_DEF_LENGTH 4
  " Number of args in founddef command")
(defvar	 ENDDEF_LENGTH	  0
  " Number of args in enddef command")
(defvar	 BLTIN_DEF_LENGTH 3
  " Number of args in bltindef command")
(defvar	 NONEDEF_LENGTH   3
  " Number of args in nonedef command")
(defvar	 UNDEF_LENGTH     3
  " Number of args in undefdef command")
(defvar	 CLEARDEF_LENGTH  0
  " Number of args in cleardef command")
(defvar	 CANTLOAD_LENGTH  0
  " Number of args in cantload command")
(defvar	 CANTCCP_LENGTH   0
  " Number of args in cantccp command")
(defvar	 QUI_QUIT_LENGTH   0
  " Number of args in qui_quit command")
(defvar	 SYN_START_LENGTH   0
  " Number of args in synstart command")
(defvar	 SYN_END_LENGTH    0
  " Number of args in synend command")

;;;
;;; get-load-path
;;;

(defun get-load-path (command-args)
  (cond ((null command-args)
	 nil)
	((string-equal (car command-args) "-l")
	 (nth 1 command-args))
	(t (get-load-path (cdr command-args)))
  )
)
;;;
;;; set up load-path
;;;
(setq load-path (cons (file-name-directory
			(get-load-path command-line-args))
		      load-path))
;;
;; load in support files
;;
(load "qpfindpred.el"    )              ; Contains find definition
					; support fns
(load "qptokens.el"      )              ; Prolog tokenising stuff
(load "qprolog-mode.el"  )              ; prolog mode functions
(load "qprolog-indent.el")              ; For indentation stuff
(load "qpfile-compl.el"  )		; file-completion within qui-buffer
(load "qui_aux.el"       )		; For auxiliary function defns
(load "qui_cmds.el"      )
(load "qui_filter.el"    )

(defvar qui-mode-map nil)
(if qui-mode-map 
    nil
  (setq qui-mode-map (make-sparse-keymap))
  (define-key qui-mode-map "\t"         'prolog-indent-line       )
  (define-key qui-mode-map "\e\C-q" 	'prolog-indent-clause     )
  (define-key qui-mode-map "\e\C-a" 	'beginning-of-clause      )
  (define-key qui-mode-map "\eh" 	'mark-clause              )
  (define-key qui-mode-map "\ef" 	'forward-prolog-word      )
  (define-key qui-mode-map "\eb" 	'backward-prolog-word     )
  (define-key qui-mode-map "\e\C-f" 	'forward-term             )
  (define-key qui-mode-map "\e\C-b" 	'backward-term            )
  (define-key qui-mode-map "\ed" 	'kill-prolog-word         )
  (define-key qui-mode-map "\e\177" 	'backward-kill-prolog-word)
  (define-key qui-mode-map "\e\C-k" 	'kill-clause              )
  (define-key qui-mode-map "\e\C-e" 	'end-of-clause            )
  (define-key qui-mode-map "\e." 	'find-qui-definition      )
  (define-key qui-mode-map "\e," 	'find-more-qui-definition )
  (define-key qui-mode-map "\ek" 	'qui-compile              )
  (define-key qui-mode-map "\ei" 	'qui-compile		  )
  (define-key qui-mode-map "\e#" 	'shell-filename-complete  )
)

(fset 'qui-mode 'qui-mode) 
;;
;; set auto-mode-alist to switch to qui-mode if file is .pl
;;
(setq auto-mode-alist 
      (append
	'(("\\.pl$" . qui-mode))
	auto-mode-alist))

(defun qui-mode ()
  "Major mode for editing files of prolog code from QUI
 The following commands are available:
 \\{qui-mode-map}."
  (interactive)
  (kill-all-local-variables)
  (use-local-map qui-mode-map)
  (setq mode-name "qui")
  (setq major-mode 'qui)
  (or (mark) (set-mark 0)))

(defun initialize ()
  "Get the network TCP paramters and open a connection, send an
initialization done message to QUI"
  ( setq port (string-to-int (getenv "QUI_PORT") ))
  ( setq hostname (getenv "QUI_HOST" ))
  ( open-connection hostname port )
  ( qui-init )
)

(defun get-arg (arg cmd-line )
  "Returns the next argument after the arg switch."
  ( cond ((null cmd-line) nil)
	 ((cond ((string-equal (car cmd-line) arg)
		 (nth 1 cmd-line))
		(t (get-arg arg (cdr cmd-line)))))))

(defun open-connection (hostname port)
  "Open a connection to port port and host hostname and associate
buffer *qui-emacs* with it."
  (switch-to-buffer "*qui-emacs*")
  (qui-mode)
  (setq qui-process
	(open-network-stream "qui" "*qui-emacs*"  hostname port))
  (set-process-filter qui-process 'qui-process-filter)
)

(defun qui-init ()
  (send-qui EMACSUP)
)

(defun qui-process-filter (process packet)
  (if *qui-packet-pending*
      (continue-reading packet)
      (begin-reading packet)
  )
  (display-any-messages)
)

;;;
;;; reinit - initializes all variables to their zero state
;;;
(defun reinit ()
  (setq *current-size* -1)
  (setq *partial-arg*  "")
  (setq *partial-size* "")
  (setq *partial-command* "")
  (setq *current-arg* 100)
)

;;;
;;; process-qui-packets: by the time this function is called "packet"
;;; represents a valid (complete) interface list. The car of this list
;;; is a qui-command and the cdr is a list of sizes and arguments
;;;

(defun process-qui-packets ()
  (reinit)
  (cond ((string-equal *packet-command* "editfile")
	 (edit-file arg2 (string-to-int arg1)))
	((string-equal *packet-command* "founddef")
	 (fill-defns arg4 arg3 arg2 arg1 ))
	((string-equal *packet-command* "bltindef")
	 (builtin arg3 arg2 arg1 ))
	((string-equal *packet-command* "nonedef ")
	 (nondef arg3 arg2 arg1))
	((string-equal *packet-command* "undefdef")
	 (undef arg3 arg2 arg1))
	((string-equal *packet-command* "enddef  ")
	 (enddef))
	((string-equal *packet-command* "qui_quit")
	 (qui-quit))
	((string-equal *packet-command* "cleardef")
	 (@fd-clear))
	((string-equal *packet-command* "cantload")
	 (cantload))
	((string-equal *packet-command* "cantccp ")
	 (cantccp))
	; The two following commands, synstart, synend do nothing
	; currently.
	((string-equal *packet-command* "synstart")
	 t)
	((string-equal *packet-command* "synend  ")
	 t)
	
	(t (message "Unknown Packet")))
  (setq *partial-packet* nil))

;;
;; returns the command string - the first eight characters
;;

(defun command (packet)
  (let ((i 1))
    (while (not (string-equal 
		  (substring packet i (+ i 1))
		  " "))
      (setq i (+ i 1)))
    (setq qui-arg (substring packet (+ i 1)))
    (substring packet 0 i))
)

;;;
;;; command-length
;;;

(defun command-length ( command )
  (cond ((string-equal command EDITFILE)
	 EDIT_FILE_LENGTH)
	((string-equal command FOUNDDEF)
	 FOUND_DEF_LENGTH)
	((string-equal command ENDDEF)
	 ENDDEF_LENGTH)	 
	((string-equal command BLTINDEF)
	 BLTIN_DEF_LENGTH)
	((string-equal command NONEDEF)
	 NONEDEF_LENGTH)
	((string-equal command UNDEF)
	 UNDEF_LENGTH)
	((string-equal command CLEARDEF)
	 CLEARDEF_LENGTH)
	((string-equal command CANTLOAD)
	 CANTLOAD_LENGTH)
	((string-equal command CANTCCP)
	 CANTCCP_LENGTH)
	((string-equal command QUIQUIT)
	 QUI_QUIT_LENGTH)
	((string-equal command SYNSTART)
	 SYN_START_LENGTH)
	((string-equal command SYNEND)
	 SYN_END_LENGTH)
  )
)

;;; returns length of STRING padded to four bytes
;;; currently handles strings less than 9999 characters long

(defun padded-length ( string ) 
  (let ((len (length string)))
    (cond ((<= len 9)
	   (concat len "   "))
	  ((and (<= len 99)
		(> len 9))
	   (concat len "  "))
	  ((and (<= len 999)
	       (> len 99))
	   (concat len " "))
	  (t len))))
       
;;;
;;; Actual interface function to QUI

(defun send-qui ( string )
  ( process-send-string qui-process string )
)
