/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, VU University, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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

