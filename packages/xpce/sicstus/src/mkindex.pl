/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

user:portray_message(informational, _).

:- use_module(library(mkindex)).

mkindex :-
	prolog_flag(argv, Argv),
	mkindex(Argv).

mkindex([]).
mkindex([H|T]) :-
	mkindex(H),
	mkindex(T).
mkindex(Dir) :-
	make_index:make_library_index(Dir).

:- mkindex, halt(0).
:- halt(1).
