/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_accelerators, []).
:- use_module(pce_principal, [send/2, send/3, get/3, get/5]).
:- use_module(pce_realise, [pce_extended_class/1]).
:- require([ concat/3
	   , is_alnum/1
	   , is_upper/1
	   , member/2
	   , to_lower/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provisional implementation for automatic assignment   of accelerators in
XPCE menus and dialog windows. One day, this  should all be moved to the
C kernel, but for experimenting this is handled in Prolog for now.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

accelerators(Objects, _) :-
	length(Objects, Len),
	Len > 15, !.
accelerators(Objects, Prefix) :-
	accelerators(Objects, Prefix, []).

accelerators([], _, _).
accelerators([H|T], Prefix, Given) :-
	accelerator(H, Prefix, Given, Acc),
	(   Acc \== []
	->  send(H, accelerator, Acc),
	    accelerators(T, Prefix, [Acc|Given])
	;   accelerators(T, Prefix, Given)
	).

accelerator(Object, _, _, []) :-
	\+ send(Object, has_send_method, accelerator), !.
accelerator(Object, Prefix, Given, Acc) :-
	(   get(Object, label, Label)
	;   send(Object, has_get_method, name),
	    get(Object, name, Label)
	;   send(Object, has_get_method, value),
	    get(Object, value, Label)
	), 
	get(@pce, convert, Label, name, Atom), !,
	propose(Atom, Char),
	to_lower(Char, Lwr),
	atom_chars(C, [Lwr]),
	concat(Prefix, C, Acc),
	\+ member(Acc, Given).


propose(Label, Char) :-
	preferred_accelerator(Label, Char).
propose(Label, Char) :-
	atom_chars(Label, Chars),
	(   member(Char, Chars),
	    is_upper(Char)
	;   member(Char, Chars),
	    is_alnum(Char)
	).


preferred_accelerator(Label, Char) :-
	get(Label, downcase, Lwr),
	get(Lwr, value, LwrAtom),
	preferred(LwrAtom, AtomChar),
	atom_chars(AtomChar, [Char]).

preferred(open, o).
preferred(exit, x).
preferred(quit, q).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Not using library(pce_util) to avoid the kernel from requiring the library.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

get_chain(Rec, Sel, List) :-
	get(Rec, Sel, Chain),
	(   Chain == @nil
	->  List = []
	;   send(Chain, current_no, 1),
	    collect_chain(Chain, List)
	).

collect_chain(Chain, [El|Rest]) :-
	get(Chain, next, El), !, 
	collect_chain(Chain, Rest).
collect_chain(Chain, []) :-
	\+ get(Chain, current, _).


		 /*******************************
		 *	 CLASS EXTENSIONS	*
		 *******************************/

:- pce_extend_class(menu_bar).

assign_accelerators(MB) :->
	get_chain(MB, buttons, Btns),
	accelerators(Btns, '\e').

:- pce_end_class.

:- pce_extend_class(menu).

assign_accelerators(MB) :->
	get_chain(MB, members, MIS),
	accelerators(MIS, '').

:- pce_end_class.

:- pce_extend_class(dialog).

assign_accelerators(MB) :->
	get_chain(MB, graphicals, MIS),
	accelerators(MIS, '\e').

:- pce_end_class.
