/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

:- module(test_occurs_check, [test_occurs_check/0]).

/** <module> Test occurs check

This module is a test-frame for the occurs_check flag.
*/

test_occurs_check :-
        run_tests([ occurs_check_fail,
		    occurs_check_error
		  ]).

has_occurs_check_flag :-
	catch(current_prolog_flag(occurs_check, _), _, fail).

set_occurs_check(Old, New) :-
	current_prolog_flag(occurs_check, Old),
	set_prolog_flag(occurs_check, New).


		 /*******************************
		 *	   UTILITY PREDS	*
		 *******************************/

unify(X, X).

		 /*******************************
		 *	OCCURS-CHECK TESTS	*
		 *******************************/

:- begin_tests(occurs_check_fail,
	       [ condition(has_occurs_check_flag),
		 setup(set_occurs_check(Old, true)),
		 cleanup(set_occurs_check(_, Old))
	       ]).

test(unify, fail) :-
	X = f(X).
test(unify, fail) :-
	unify(X, f(X)).
test(unifiable, fail) :-
	unifiable(X, f(X), _).

:- end_tests(occurs_check_fail).


:- begin_tests(occurs_check_error,
	       [ condition(has_occurs_check_flag),
		 setup(set_occurs_check(Old, error)),
		 cleanup(set_occurs_check(_, Old))
	       ]).

test(unify, error(occurs_check(X, f(X)))) :-
	X = f(X).
test(unify, error(occurs_check(X, f(X)))) :-
	unify(X, f(X)).
test(unifiable, error(occurs_check(X, f(X)))) :-
	unifiable(X, f(X), _).

:- end_tests(occurs_check_error).


