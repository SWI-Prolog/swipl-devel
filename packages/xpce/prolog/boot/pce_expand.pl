/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- dynamic
	term_expansion/2.
:- multifile
	term_expansion/2.

term_expansion(pce_ifhostproperty(Prop, Clause), TheClause) :-
	(   pce_host:property(Prop)
	->  TheClause = Clause
	;   TheClause = []
	).
term_expansion(pce_ifhostproperty(Prop, If, Else), Clause) :-
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
