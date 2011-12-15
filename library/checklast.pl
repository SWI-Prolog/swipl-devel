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

:- module(checklast,
	  [ check_old_last/0
	  ]).
:- use_module(library(lists)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module simplifies porting < 5.1.11   applications using last/2 with
the wrong argument order to >=  5.1.12.   For  further discussion on the
argument order of last/2, see

http://www.prolog-standard.fmg.uva.nl/twiki/bin/view/Library/PredLast2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

check_old_last :-
	print_message(informational, last_check).

user:goal_expansion(last(L,E), _) :-
	print_message(warning, last_arguments(last(L,E))),
	fail.

		 /*******************************
		 *	CHECKING VERSION	*
		 *******************************/

:- abolish(lists:last/2).

%%	last(?List, ?Elem)
%
%	Succeeds if `Last' unifies with the last element of `List'.

lists:last(List, Last) :-
	\+ is_list(List), !,
	print_message(error, last_call(last_call(List, Last))),
	trace,
	fail.
lists:last([X|Xs], Last) :-
	last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :-
	last_(Xs, X, Last).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(last_check) -->
	[ 'Enabled checking for wrong argument order in last/2' ].
prolog:message(last_arguments(S)) -->
	{ numbervars(S, 0, _)
	},
	[ 'Last/2 used; check argument order: ~p'-[S] ].
prolog:message(last_call(S)) -->
	[ 'Suspicious last/2 call, entering debugger: ~p'-[S] ].

