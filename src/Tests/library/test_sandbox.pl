/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_sandbox,
	  [ test_sandbox/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test set for library(sandbox)


@tbd	A lot more tests.  In particular test and precise the relation
	between sandboxs and modules.
*/

test_sandbox :-
	run_tests([ sandbox
		  ]).

:- begin_tests(sandbox).
:- use_module(library(sandbox)).
:- use_module(library(aggregate)).

test(cleanup) :-
	safe_goal(setup_call_cleanup(true,true,true)).
test(time) :-
	safe_goal(time(true)).
test(setof) :-
	safe_goal(setof(X, Y^between(1, Y, X), _Xs)).
test(phrase) :-
	safe_goal(phrase("hello", `hello`, [])).
test(apply) :-
	safe_goal(forall(true,true)).
test(aggregate) :-
	safe_goal(aggregate(count, between(1,10,_), _Count)).
test(aggregate) :-
	safe_goal(aggregate(sum(I), X^between(1,X,I), _Count)).

:- end_tests(sandbox).
