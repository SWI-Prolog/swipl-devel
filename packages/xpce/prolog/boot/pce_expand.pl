/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
