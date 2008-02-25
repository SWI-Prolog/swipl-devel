/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(test_term, [test_term/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core term manipulation primitives

This module is a Unit test for  Prolog built-ins that process terms,
suchj as numbervars, univ, etc.

@author	Jan Wielemaker
*/

test_term :-
	run_tests([ numbervars
		  ]).

:- begin_tests(numbervars).

test(single, End == 1) :-
	numbervars(_, 0, End).
test(single_s, End == 0) :-
	numbervars(_, 0, End, [singletons(true)]).
test(shared, End == 1) :-
	X = d(_),
	Y = t(X,X),
	numbervars(Y, 0, End, []).
test(shared_s, End == 1) :-
	X = d(_),
	Y = t(X,X),
	numbervars(Y, 0, End, [singletons(true)]).
test(cyclic, [sto(rational_trees), End == 1]) :-
	X = d(X, _),
	numbervars(X, 0, End, []).
% currently singletons(true) is ignored for cyclic terms
test(cyclic_s, [sto(rational_trees), End == 1]) :-
	X = d(X, _),
	numbervars(X, 0, End, [singletons(true)]).

:- end_tests(numbervars).
