;;;  SCCS: @(#)89/11/21 qpstart.el    1.1
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
;;; Define load-path argument.
;;;
(defconst load-path-arg 2 
  "The argument in command-line-args which identifies the directory in
which the Quintus Prolog Emacs interface files are located")
;;;
;;; Set load-path variable.
;;; 
(setq load-path (cons 
		  (file-name-directory 
		    (nth load-path-arg command-line-args))
		  load-path))

;;; Load the rest
(load "qprolog-mode" nil t) 
