/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(prolog_term_view,
	  [ view_term/1,		% +Term
	    view_term/2			% +Term, +Attributes
	  ]).
:- use_module(pprint).

view_term(Term) :-
	view_term(Term, []).

view_term(Term, Attributes0) :-
	defaults(Defs),
	append(Attributes0, Defs, Attributes),
	tv(Term, Attributes).

defaults([ view(@view_term),
	   clear(true),
	   open(true)
	 ]).

tv(Term, Attributes) :-
	attribute(Attributes, view(V)),
	if(Attributes, clear(true), send(V, clear)),
	if(Attributes, open(true), send(V, open)),
	if(Attributes, comment(Comment), send(V?frame, label, Comment)),
	get(V, text_buffer, TB),
	pce_open(TB, write, Fd),
	print_term(Term, [output(Fd)|Attributes]),
	close(Fd),
	send(V, caret, 0).

attribute(Attributes, A) :-
	memberchk(A, Attributes).

if(Attributes, Cond, Goal) :-
	memberchk(Cond, Attributes), !,
	Goal.
if(_, _, _).

:- pce_global(@view_term, new(term_viewer)).

:- pce_begin_class(term_viewer, view,
		   "Pretty-print a Prolog term").


:- pce_end_class.
