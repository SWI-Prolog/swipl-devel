/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_mkcommon,
	  [ mkcommon/1
	  ]).
:- require([ forall/2
	   , member/2
	   , memberchk/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Used to make common.pl; the file holding all predicates available in all
supported Prolog platforms.  To generate this   file, start each of your
Prolog environments and do:

	?- tell('myprolog.pl'),
	   predicate_property(X, built_in),
	   write_canonical(X), put(0'.), nl,
	   fail ; told.

Next, load this package into Prolog and type:

	?- tell('common.pl'), mkcommon(['quintus.pl', 'sicstus.pl'], told.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	common_term/1.

mkcommon([First|Files] :-
	retractall(common_term(_)),
	read_terms(Files, QpTerms),
	forall(member(T, QpTerms), assert(common_term(T))),
	common(Files),
	setof(T, common_term(T), Terms),
	forall(member(T, Terms),
	       (write_canonical(built_in(T)), put(.), nl)).

common([]).
common([F|T]) :-
	read_terms(F, Terms),
	findall(T2, (common_term(T2), \+ memberchk(T2, Terms)), Del),
	forall(member(D, Del), retract(common_term(D))),
	common(T).

read_terms(F, Terms) :-
	seeing(Old), see(F),
	read_terms(Terms),
	seen, see(Old).

read_terms([H|T]) :-
	read(Raw),
	Raw \== end_of_file, !,
	(   Raw = built_in(H)
	->  true
	;   H = Raw
	),
	read_terms(T).
read_terms([]).
	
	    
	
