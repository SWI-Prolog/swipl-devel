/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(checkselect,
	  [ check_old_select/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module simplifies porting 3.3.x   applications  using select/3 with
the wrong argument order to 3.4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

check_old_select :-
	print_message(informational, select_check).

user:goal_expansion(select(L,E,R), _) :-
	print_message(warning, select_arguments(select(L,E,R))),
	fail.

		 /*******************************
		 *	CHECKING VERSION	*
		 *******************************/

:- redefine_system_predicate(user:select(_,_,_)).

user:select(E, L, R) :-
	\+ is_list(L),
	print_message(error, select_call(select(E,L,R))),
	trace,
	fail.
user:select(E, [E|R], R).
user:(select(E, [H|T], [H|R]) :-
	select(E, T, R)).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(select_check) -->
	[ 'Enabled checking for wrong argument order in select/3' ].
prolog:message(select_arguments(S)) -->
	{ numbervars(S, 0, _)
	},
	[ 'Select/3 used; check argument order: ~p'-[S] ].
prolog:message(select_call(S)) -->
	[ 'Suspicious select/3 call, entering debugger: ~p'-[S] ].

