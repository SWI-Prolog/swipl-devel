;;; Interface to SWI-Prolog
;;; Author: Jan Wielemaker, SWI, University of Amsterdam
;;; E-mail: jan@swi.psy.uva.nl

;;; This package forms a layer around the Quintus-Prolog interface.  It
;;; should be used together with the Prolog library 'emacs_interface.pl'.

;;; It implements hooks that allow SWI-Prolog to give compilation-warnings
;;; back to EMACS, so the user can step through them using the normal
;;; ^X` command

;;; Usage:
;;;
;;;     Put the following lines in your ~/.emacs:
;;;
;;;	(autoload 'run-prolog "swi-prolog" "Run an inferior prolog process" t)
;;;	(autoload 'prolog-mode "swi-prolog" "SWI-Prolog mode" t)

;;; Notes:
;;;
;;; As far as I was able to figure out, the Quintus Prolog GNU-Emacs lisp
;;; interface can be distributed under the normal GNU general public licence
;;;
;;; This file is distributed confirm the GNU general public licence.


(defconst qplisp-directory "/usr/local/lib/emacs/qplisp"
  "Directory with all the quintus interface files")
(defvar run-prolog-command "pl"
  "Command to start SWI-Prolog")
(defconst prolog-warning-buffer "*compilation*"
  "SWI-Prolog buffer for warnings")

;;; Get the Quintus EMACS library in your load path

(setq load-path (cons qplisp-directory load-path))
(load-library "qprolog-mode")
(load-library "compile")
(setq prolog-prompt-pattern "^[0-9]+ \\?- ")


(defun prolog-compilation-start (dir)
  "Clear *compilation* buffer"
  (save-excursion
    (set-buffer (get-buffer-create prolog-warning-buffer))
    (erase-buffer)
    (setq default-directory dir)
    (compilation-forget-errors)
    (setq compilation-error-list t)
    (setq compilation-error-message "No more SWI-Prolog errors")
    (insert "cd " dir)
    (newline)
    (insert "SWI-Prolog warnings")
    (newline)))


(defun prolog-compilation-finish ()
  "Finish prolog-compilation"
  (save-excursion
    (set-buffer prolog-warning-buffer)
    (end-of-buffer)
    (newline 2)
    (insert "Compilation finished at " (current-time-string))
    (newline)
    (setq compilation-parsing-end 1)))


(defun prolog-compilation-warning (file line msg)
  "Put a prolog error-message in *compilation*"
  (save-excursion
    (set-buffer prolog-warning-buffer)
    (end-of-buffer)
    (display-buffer (current-buffer))
    (insert file ":" line ": " msg)
    (newline)))


;;; STARTING PROLOG
;;; This function is a modified version of run-prolog in qprolog-mode.el

(defun run-prolog (command)
  "Run an inferior SWI-Prolog process, input and output via buffer
*prolog*."
  (interactive (list (read-string "Run prolog: " run-prolog-command)))
  (setq run-prolog-command command)
  (ensure-prolog-syntax)
  (qprequire 'shell)
  (get-prolog-exec-and-flags (concat command startup-jcl))
  (switch-to-buffer-other-window (apply 'make-shell "prolog"
					*prolog-executable* nil  
					*prolog-flags*))
  (set-process-filter (get-process "prolog") 'prolog-process-filter)
  (sleep-for 2)
  (inferior-prolog-mode)
  (local-set-key "\t" 'prolog-dabbrev-atom)
  (local-set-key "\C-d" 'prolog-complete-atom)
  (local-set-key "\C-c\C-n" 'prolog-next-command)
  (local-set-key "\C-c\C-p" 'prolog-previous-command))


;;; ATOM COMPLETION

(defvar *prolog-start-completion* nil
  "Start of prolog completion")
(defvar *prolog-end-completion* nil
  "End of prolog completion")
(defvar *prolog-atom-completions* nil
  "Collect-list for prolog completions")
(defvar *prolog-completion-process-mark* nil
  "Process mark when starting completion")

(defun prolog-completion-backward-word ()
  (interactive)
  (backward-word 1)
  (backward-char 1)
  (if (looking-at "_")
      (prolog-completion-backward-word)
      (forward-char 1)))


(defun prolog-completion-sofar ()
  (setq *prolog-end-completion* (point))
  (let ((end (point)))
    (save-excursion
      (backward-char 1)
      (cond ((looking-at "[a-zA-Z0-9_]\\b")
	     (prolog-completion-backward-word)
	     (setq *prolog-start-completion* (point))
	     (setq *prolog-completion-process-mark*
		   (marker-position (process-mark
				     (get-buffer-process "*prolog*"))))
	     (buffer-substring (point) end))
	    (t nil)))))

(defun prolog-complete-atom-with (extended unique)
  (cond ((eq *prolog-end-completion* (point))
	 (kill-region *prolog-start-completion* *prolog-end-completion*)
	 (insert extended)
	 (setq *saved-prolog-process-mark* *prolog-completion-process-mark*)
	 (if (not unique) (message "[incomplete]")))
	(t
	 (prolog-completion-error-message "Mismatch of dabbrev-point"))))

(defun prolog-completion-error-message (string)
  (message string)
  (setq *saved-prolog-process-mark* *prolog-completion-process-mark*))

;;; DABBREV

(defun prolog-dabbrev-atom ()
  (interactive)
  (let (sofar)
    (cond ((setq sofar (prolog-completion-sofar))
	   (send-prolog (concat 
			 "'$silent'(emacs_dabbrev_atom(\""
			 sofar
			 "\"))")))
	  (t
	   (message "Point not at end of atom")))))

;;; COMPLETION

(defun prolog-complete-atom ()
  (interactive)
  (let (sofar)
    (cond ((setq sofar (prolog-completion-sofar))
	   (send-prolog (concat
			 "'$silent'(emacs_complete_atom(\""
			 sofar
			 "\"))")))
	  (t
	   (message "Point not at end of atom")))))
		
(defun prolog-completions-start-collect ()
  (setq *prolog-atom-completions* nil))

(defun prolog-transfer-completion (atom number)
  (setq *prolog-atom-completions*
	(cons (list atom number)
	      *prolog-atom-completions*)))

(defun prolog-completions-run (sofar)
  (prolog-complete-atom-with
   (completing-read "Complete atom: "
		    *prolog-atom-completions*
		    nil
		    nil
		    sofar)
   t))
  
;;; HISTORY

(defun prolog-previous-command ()
  (interactive)
  (end-of-buffer)
  (setq *prolog-completion-process-mark*
	(marker-position (process-mark (get-buffer-process "*prolog*"))))
  (send-prolog "'$silent'(emacs_previous_command)"))


(defun prolog-next-command ()
  (interactive)
  (end-of-buffer)
  (setq *prolog-completion-process-mark*
	(marker-position (process-mark (get-buffer-process "*prolog*"))))
  (send-prolog "'$silent'(emacs_next_command)"))


(defun prolog-insert-history-command (cmd)
  (kill-region *prolog-completion-process-mark* (point))
  (insert cmd ".")
  (setq *saved-prolog-process-mark* *prolog-completion-process-mark*))


;;; COMPILATION

(defun prolog-recompile ()
  (interactive)
  (save-some-buffers)
  (if (not (eq (current-buffer) (get-buffer "*prolog*")))
      (pop-to-buffer (get-buffer "*prolog*") nil))
  (end-of-buffer)
  (insert "make.\n")
  (send-prolog "make"))
