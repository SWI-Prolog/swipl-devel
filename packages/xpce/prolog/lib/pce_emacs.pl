/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(start_emacs,
	  [ emacs/0
	  , emacs/1				% x File
	  , start_emacs/0
	  ]).
:- use_module(library(pce)).

:- pce_autoload(emacs_buffer_menu, library('emacs/emacs')).

:- pce_global(@emacs_buffers, new(dict)).
:- pce_global(@emacs, new(emacs_buffer_menu(@emacs_buffers))).


start_emacs :-
	send(@emacs, create).


emacs :-
	start_emacs,
	new(Scratch, emacs_buffer(@nil, '*scratch*')),
	send(Scratch, open).
	

emacs(File) :-
	start_emacs,
	new(B, emacs_buffer(File)),
	send(B, open).
