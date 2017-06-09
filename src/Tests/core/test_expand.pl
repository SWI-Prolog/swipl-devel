/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2009-2015, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_expand,
	  [ test_expand/0
	  ]).
:- use_module(library(plunit)).
:- set_module(base(user)).

test_expand :-
	run_tests([ expand
		  ]).

		 /*******************************
		 *	       RULES		*
		 *******************************/

:- multifile
	user:term_expansion/2,
	user:goal_expansion/2.
:- dynamic
	user:term_expansion/2,
	user:goal_expansion/2.
:- discontiguous
	goal_expansion/2.

user:term_expansion(b2c, final_c).

term_expansion(a2b, b2c).
term_expansion(make_dcg, (a --> "")).

goal_expansion(g_b, g_c).


		 /*******************************
		 *	     PROGRAMS		*
		 *******************************/

:- '$clausable'((a/2, final_c/0)).

:- if(false).				% nothing compiles if false
no_a --> [].
:- endif.

make_dcg.				% expand into DCG
a2b.					% chained expansion

e_not :-
	not(g_b).

g_c.

% Test meta-predicate declaration before (local) definition of the
% predicate.

goal_expansion(foo, bar).

:- meta_predicate
	run(0).

test_foo_bar :-
	run(foo).

bar.

run(Goal) :- Goal.

goal_expansion(onetime(X), (X->true)).

one(X) :-
	(   onetime(X=1)
	;   X = 2
	).


		 /*******************************
		 *	       TESTS		*
		 *******************************/

:- begin_tests(expand).

test(cond_dcg, true) :-
	\+ current_predicate(no_a//0).
test(make_dcg, B == true) :-
	clause(a(_,_), B).
test(chained_term_expansion, B == true) :-
	clause(final_c, B).
test(meta_arg, [fail]) :-
	e_not.
test(goal_expansion_local_pred) :-
	test_foo_bar.
test(expand_once, all(X == [1,2])) :-
	one(X).

:- end_tests(expand).
