;;; SCCS: @(#)90/11/15 qphelp-functions.el    2.10
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

(provide 'qhelp-functions)

;
; key functions. Each function is bound to a specific key code.
;

(defmacro error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defun find-next-entry ()
  (interactive)
  (if (error-occurred (re-search-forward "manual([a-z]+-*[0-9---]+*)"))
      (progn 
	(goto-char (point-min))
	(if (error-occurred (re-search-forward "manual([a-z]+-*[0-9---]+*)")) 
	    (progn 
	      (message "Bad menu format for file: %s" current-file)
	      (stop-it)))))
 ; (message "<Space>/<DEL> to position cursor, <Return> to select"))
  (message "<Space> to advance cursor, <Return> to select item, q to Quit, ? for Help"))

(defun find-previous-entry ()
  (interactive)
  (beginning-of-line)
  (if (error-occurred (re-search-backward "manual([a-z]+-*[0-9---]+*)"))
      (progn 
	(goto-char (point-max))
	(if (error-occurred (re-search-backward "manual([a-z]+-*[0-9---]+*)"))
	    (progn 
	      (message "Bad menu format for file: %s" current-file)
	      (stop-it)))))
  (re-search-forward "[a-z]+-*[0-9---]+*)")
  (message "<Space> to advance cursor, <Return> to select item, q to Quit, ? for Help"))

(defun get-entry ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (error-occurred (re-search-forward "manual([a-z]+-*[0-9---]+*)"))
	(message (concat "The cursor is not at a cross reference, " 
			 "try typing a <Space>/<DEL>."))
      (let* ((end (point))
	     (reference (progn 
			  (re-search-backward "manual")
			  (buffer-substring (point) end))))
	(send-prolog reference)))))

(defun stop-it ()
  "Return to the top level. Typically this is the prolog shell."
  (interactive)
  (let* ((list (buffer-list)))
    (while list
      (let* ((buffer (car list)))
	(and (string-match "Quintus-Help-System" (buffer-name buffer))
	     (kill-buffer buffer)))
      (setq list (cdr list))))
  (pop-to-buffer "*prolog*")
  (goto-char (point-max)))

(defun find-next-reference ()
  (interactive)
  (if (error-occurred (re-search-forward "manual([a-z]+-*[0-9---]+*)")) 
      (message "End of cross references.")))

(defun find-previous-reference ()
  (interactive)
  (let ((position (point)))
    (if (error-occurred (re-search-backward "{"))
	(progn 
	  (message "Beginning of cross references.")
	  (goto-char position))
      (if (error-occurred (re-search-backward "manual([a-z]+-*[0-9---]+*)"))
	  (progn 
	    (message "Beginning of cross references.")
	    (goto-char position))
	(re-search-forward ")")))))
	  
(defun retrieve-next-reference ()
  (interactive)
  (save-excursion
    (if (error-occurred (re-search-backward "manual([a-z]+-*[0-9---]+*)"))
	(message "Reference not found, try typing 'X' or 'x'.")
    (let* ((start (point))
	  (reference (progn 
		       (re-search-forward "[0-9]+*)")
		       (buffer-substring start (point)))))
      (send-prolog reference)))))

(defun back-one-step ()
  "Return to the previous level."
  (interactive)
  (pop-state))

(defun get-menu-help ()
  (interactive)
  (message "Please wait, getting help...")
  (send-prolog "manual('menus, Emacs commands for')"))

(defun get-text-help ()
  (interactive)
  (message "Please wait, getting help...")
  (send-prolog "manual('text, Emacs commands for')"))

(defun up-one-level ()
  "Go to the parent reference"
  (interactive)
  (goto-char (point-min))
  (end-of-line)
  (cond ((equal (point-min) (point))
	 (stop-it))
	(t (cond ((re-search-backward "-" (point-min) t)
		  (let ((reference (concat 
				     "manual(" 
				     (buffer-substring (point-min) (point))
				     ")")))
		    (message 
		      "Please wait calling:" reference)
		    (send-prolog reference)))
		 (t  (message "Please wait calling: manual.")
		     (send-prolog "manual"))))))


(defun previous-page ()
  (interactive)
  (scroll-down nil))

(defun next-page ()
  (interactive)
  (scroll-up nil))

(defun scroll-one-line-up ()
  (interactive)
  (scroll-up 1))

(defun scroll-one-line-down ()
  (interactive)
  (scroll-down 1))


