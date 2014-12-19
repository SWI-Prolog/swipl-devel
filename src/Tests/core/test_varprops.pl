/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University, Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_varprops, [test_varprops/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test variable information

Test var_property/2
*/

test_varprops :-
	run_tests([ varprops
		  ]).

:- begin_tests(varprops).

goal_expansion(fresh(X), error(fresh(X))) :-
	print_freshness(X),
	var_property(X, fresh(false)).
goal_expansion(nonfresh(X), error(nonfresh(X))) :-
	print_freshness(X),
	var_property(X, fresh(true)).

print_freshness(_Var) :-
	\+ debugging(var_property), !.
print_freshness(Var) :-
	get_attr(Var, '$var_info', Info), !,
	format(user_error, '~w: ~w~n', [Var, Info]).
print_freshness(Var) :-
	format(user_error, '~w: virgin~n', [Var]).

expand_freshness(Body, Expanded) :-
	context_module(Here),
	expand_term((x:-Here:Body), (x:-Here:Expanded)).


test(simple, Expanded =@= Body) :-
	Body = (fresh(X), nonfresh(X)),
	expand_freshness(Body, Expanded).
test(not, Expanded =@= Body) :-
	Body = (\+ fresh(X), fresh(X)),
	expand_freshness(Body, Expanded).
test(not, Expanded =@= Body) :-
	Body = (fresh(X), \+ nonfresh(X)),
	expand_freshness(Body, Expanded).
test(disjunct, Expanded =@= Body) :-
	Body = (fresh(X); fresh(X)),
	expand_freshness(Body, Expanded).
test(disjunct, Expanded =@= Body) :-
	Body = ((fresh(X); fresh(X)), nonfresh(X)),
	expand_freshness(Body, Expanded).
test(disjunct, Expanded =@= Body) :-
	Body = ((any; fresh(X)), nonfresh(X)),
	expand_freshness(Body, Expanded).
test(disjunct, Expanded =@= Body) :-
	Body = ((fresh(X); any), nonfresh(X)),
	expand_freshness(Body, Expanded).
test(disjunct, Expanded =@= Body) :-
	Body = (fresh(X), (nonfresh(X); nonfresh(X)), nonfresh(X)),
	expand_freshness(Body, Expanded).

:- end_tests(varprops).

