/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(test_db, [test_db/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core database functions

@author	Jan Wielemaker
*/

test_db :-
	run_tests([ assert2,
		    dynamic
		  ]).

:- begin_tests(assert2).

:- dynamic
	term/0.

test(bound, error(_)) :-
	assert(term,noref).

:- end_tests(assert2).

:- begin_tests(dynamic).

test(make_dynamic, [true, cleanup(abolish(Name, 1))]) :-
	gensym(somepred, Name),
	Term =.. [Name, a],
	catch(Term, _, true),
	assertz(Term),
	Term.

:- end_tests(dynamic).
