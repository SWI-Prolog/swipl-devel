;;; SCCS: @(#)90/09/25 qphelp.el    2.8
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
;;; Note: there may be a problem using Quintus Prolog help under X windows.
;;; If the emacs window is resized or moved after help is invocated, restoring
;;; the previous window  configuration will result in an error. 

(provide 'qphelp)
(qprequire 'qphelp-functions)

; Quintus Prolog Help System file types.
;
(defconst MENU "{menu}")
(defconst TEXT "{text}")
(defconst SHELL "{shell}")
(defconst HELP "{help}")
(defconst Prolog-buffer "*prolog*")

; Constants
;
(defconst NOERROR t)
(defconst EMPTY '())

; Initialize Variables.
;
(defvar *state* '() 
  "List of buffer states, i.e. (current-buffer point).")
(defvar current-file nil 
  "The help file we are now looking for; used for error reporting.")
(defvar Quintus-help-key-map nil "Local keymap for Prolog menu help.")
(defvar Quintus-text-key-map nil "Local keymap for Prolog text help.")

; buffer:
;    Quintus-help-system: for help and text interaction.
;
(defvar Quintus-help-system "*Quintus-Help-System*" 
  "Buffer name used during a Quintus Prolog help session.")

(defmacro error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defun push-state (buffer point)
  "Save the current buffer and point location."
  (setq *state* (append (list (cons buffer point)) *state*)))

(defun pop-state ()
  "pop to the most recent buffer. If we are returning to the 
   top level, return the point to the end of the buffer."
  (kill-buffer (current-buffer))
  (while (error-occurred (pop-to-buffer (car (car *state*)))) 
					; in case intermediate buffers have been killed
    (setq *state*  (cdr *state*)))
  (if  (equal (buffer-name (car (car *state*))) "*prolog*")
       (goto-char (point-max))			; then part
       (goto-char	 (cdr (car *state*))))  ; else part
  (setq *state*  (cdr *state*)))
  
(defun initialize-state ()
  "Initilize variables with each invocation from Quintus Prolog."
  (if (string-equal (buffer-name) Prolog-buffer)
      (setq *state* '())))

(defun @help (file)
  "Help executive for Quintus Prolog."
  (initialize-state)
  (push-state (current-buffer) (point))
  (process-file file))

(defun @manual (file)
  "Manual executive for Quintus Prolog."
  (initialize-state)
  (push-state (current-buffer) (point))
  (process-file file))

(defun process-file (file)
  "Process a help or manual query from Quintus Prolog"
  (switch-to-buffer-other-window (generate-new-buffer Quintus-help-system))
  (erase-buffer)
  (if (error-occurred (insert-file file)) ;read the file into an empty buffer
      (message "%s" (concat "There is no information "
			    "currently available on this topic."))
    (progn (setq current-file file) ; save current file for error reporting
	   (goto-char (point-min))  ; goto top of file
	   (initialize))))          ; initialize the window

(defun initialize () 
  "Initiailzations performed on entry to a buffer. Different actions are 
   performed depending whether the file is {menu} or {text}."
  (cond ((file-type MENU) (progn 
			    (delete-type-marker)   ;remove the type marker
			    (define-local-key-map MENU) ;define local key map
			    (find-next-entry)))  ;go to first entry
	((file-type TEXT) (progn 
			    (delete-type-marker) ;delete type marker
			    (define-local-key-map TEXT))) ;define local key map
	(t (message "%s" (concat "error malformed help/manual "
				 "file: type marker not found."))))
  (toggle-read-only))

(defun define-local-key-map (type)
  "Select a key map for either menu or text files."
  (cond ((string-equal type MENU) 
	 (progn 
	   (Define-Quintus-help-keys)
	   (use-local-map Quintus-help-key-map)))
	((string-equal type TEXT) 
	 (progn 
	   (Define-Quintus-text-keys)
	   (use-local-map Quintus-text-key-map)))
		((string-equal type SHELL)
	 (progn 
	   (Define-Quintus-help-keys)
	   (use-local-map Quintus-help-key-map)))
	(t (message "define-local-key-map: illegal map specifer: %s", type))))

(defun Define-Quintus-help-keys ()
  (if (equal Quintus-help-key-map nil)
      (progn 
	(setq Quintus-help-key-map (make-keymap))
	(suppress-keymap Quintus-help-key-map)    
	(Quintus-help-key-map))))

(defun Define-Quintus-text-keys ()
  (if (equal Quintus-text-key-map nil)
      (progn 
	(setq Quintus-text-key-map (make-keymap))
	(suppress-keymap Quintus-text-key-map)
	(Quintus-text-key-map))))

(defun Quintus-text-key-map ()
  "Define the local key for The Quintus Prolog Text System."
  (define-key Quintus-text-key-map "q"      'stop-it)
  (define-key Quintus-text-key-map "b"      'back-one-step)
  (define-key Quintus-text-key-map "u"      'up-one-level)
  (define-key Quintus-text-key-map "?"      'get-text-help)
  (define-key Quintus-text-key-map "x"      'find-next-reference)
  (define-key Quintus-text-key-map "X"      'find-previous-reference)
  (define-key Quintus-text-key-map "\C-l"   'redraw-display)
  (define-key Quintus-text-key-map "\C-m"   'retrieve-next-reference)
  (define-key Quintus-text-key-map "\C-v"   'next-page)
  (define-key Quintus-text-key-map "\e\C-v" 'previous-page)
  (define-key Quintus-text-key-map "\ez"    'scroll-one-line-up)
  (define-key Quintus-text-key-map "\e\C-z" 'scroll-one-line-down)
  (define-key Quintus-text-key-map " "      'scroll-up)
  (define-key Quintus-text-key-map "\C-?"   'scroll-down)
  (define-key Quintus-text-key-map "<"      'beginning-of-buffer)
  (define-key Quintus-text-key-map ">"      'end-of-buffer))

(defun Quintus-help-key-map ()  
  "Define the local key map for The Quintus Prolog Help System."
  (define-key Quintus-help-key-map " "      'find-next-entry)
  (define-key Quintus-help-key-map "\C-m"   'get-entry)
  (define-key Quintus-help-key-map "\C-?"   'find-previous-entry)
  (define-key Quintus-help-key-map "q"      'stop-it)
  (define-key Quintus-help-key-map "u"      'up-one-level)
  (define-key Quintus-help-key-map "b"      'back-one-step)
  (define-key Quintus-help-key-map "?"      'get-menu-help)
  (define-key Quintus-help-key-map "\C-l"   'redraw-display))

(defun delete-type-marker () 
  "Delete the type marker. Function 'delete-type-marker' assumes the
   existence of the type marker has been verified with 'file-type'."	
  (goto-char (point-max))
  (forward-line -1)
  (kill-line 1)
  (goto-char (point-min)))

(defun file-type (type)
  "Find the type marker: {menu} or {text}."
  (goto-char (point-max))
  (forward-line -1)
  (beginning-of-line)
  (cond ((search-forward type nil NOERROR) (goto-char (point-min)))
	(t (not (goto-char (point-min))))))

