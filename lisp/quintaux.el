;;;  SCCS: @(#)89/11/28 quintaux.el    3.4
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
;;; Load file for Quintus Prolog Emacs interface

(provide 'quintaux)
; ----------------------------------------------------------------------
;                          Quintus files
; ----------------------------------------------------------------------

(load "qptokens" nil t)
(load "qpaux" nil t)
(load "qpcommands" nil t)
(load "qpfindpred" nil t)
(load "qprocess" nil t)
(load "qpdelete" nil t)
(load "qprolog" nil t)
(load "qprolog-indent" nil t)
;;;
;;; The following elisp file provides file completion anywhere in emacs
;;; - useful for commands like unix(cd(..)).
;;;
(load "qpfile-compl.el" nil t)                ;; due to Paul Davis
(qprequire 'qphelp)
