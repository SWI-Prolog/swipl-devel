/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(pce_expand, []).

:- multifile
	user:term_expansion/2.
:- dynamic
	user:term_expansion/2.

user:term_expansion(pce_ifhostproperty(Prop, Clause), TheClause) :-
	(   pce_host:property(Prop)
	->  TheClause = Clause
	;   TheClause = []
	).
user:term_expansion(pce_ifhostproperty(Prop, If, Else), Clause) :-
	(   pce_host:property(Prop)
	->  Clause = If
	;   Clause = Else
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Defined properties:

property(prolog(PrologId)).
property(file_extensions(ListOfLoadableExtensions)).
property(repeat_meta_declaraction).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
