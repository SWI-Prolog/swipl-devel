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
	  , emacs_server/0
	  , emacs_toplevel/0
	  ]).
:- use_module(library(pce)).
:- require([ append/3
	   , maplist/3
	   , unix/1
	   ]).

:- pce_autoload(emacs, library('emacs/emacs')).

:- pce_global(@emacs_buffers, new(dict)).
:- pce_global(@emacs, new(emacs(@emacs_buffers))).


start_emacs :-
	send(@emacs, start).


emacs_server :-
	start_emacs,
	send(@pce, trap_errors, @off),
	send(@pce, console_label, 'PceEmacs Server').


emacs :-
	start_emacs,
	new(Scratch, emacs_buffer(@nil, '*scratch*')),
	send(Scratch, open).
	

emacs(File) :-
	start_emacs,
	new(B, emacs_buffer(File)),
	send(B, open).

emacs_toplevel :-
	unix(argv(Argv)),
	files(Argv, Files),
	(   Files = [_|_]
	->  start_emacs,
	    maplist(make_buffer, Files, [B0|_]),
	    send(B0, open)
	;   emacs
	).

make_buffer(File, Buffer) :-
	new(Buffer, emacs_buffer(File)).

files(List, Files) :-
	append(_, ['--'|Files], List), !.
files(List, Files) :-
	append(_, ['-x', _State|Files], List), !.
files([_Prog|Files], Files).
