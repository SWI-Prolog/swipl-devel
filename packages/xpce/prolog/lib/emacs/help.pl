/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_help,
	  [ emacs_help/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(emacs), [emacs/1]).
:- require([ absolute_file_name/3
	   ]).

:- dynamic
	help_file/1.

:- absolute_file_name(library(emacs), [extensions([hlp])], File),
   asserta(help_file(File)).

emacs_help :-
	help_file(HelpFile),
	emacs(HelpFile).
	
