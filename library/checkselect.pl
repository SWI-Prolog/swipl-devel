/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(checkselect,
	  [ check_old_select/0
	  ]).

/** <module> Check usage of old select/3

This module simplifies porting 3.3.x   applications  using select/3 with
the wrong argument order to 3.4.
*/

%%	check_old_select
%
%	When compiling, print calls to select/3 that may use the wrong
%	argument order.  Upto version 3.3.x the argument order of select/3
%	as
%
%		select(+List, ?Element, ?RestList).
%
%	Later versions use the compatible version
%
%		select(?Element, +List, ?RestList).

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

