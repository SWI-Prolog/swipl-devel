/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_lists,
	  [ test_lists/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(sandbox)).

test_lists :-
	run_tests([ lists,
		    reverse
		  ]).

:- begin_tests(lists).

% must be deterministic.
test(reverse, L == [b,a]) :-
    reverse(L, [a,b]).

:- end_tests(lists).

:- begin_tests(reverse).

% reverse/2 uses C code where it can.  It must do exactly what its
% Prolog definition does.

pl_reverse(Xs, Ys) :-
    lists:reverse(Xs, Ys, [], Ys).

% True if reverse/2 and its Prolog definition give the same (first
% three) answers for a copy of the arguments, with the occurs_check
% flag set to Flag.
same(Flag, Xs, Ys) :-
    outcome(Flag, reverse, Xs-Ys, O1),
    outcome(Flag, pl_reverse, Xs-Ys, O2),
    O1 =@= O2.

outcome(Flag, Reverse, Args, Outcome) :-
    copy_term(Args, Xs-Ys),
    current_prolog_flag(occurs_check, Old),
    setup_call_cleanup(
        set_prolog_flag(occurs_check, Flag),
        catch(once(findnsols(3, Answer-Delayed,
                             ( call(Reverse, Xs, Ys),
                               copy_term(Xs-Ys, Answer, Delayed)
                             ),
                             Outcome)),
              error(Error, _),
              Outcome = error(Error)),
        set_prolog_flag(occurs_check, Old)).

test(empty, L == []) :-
    reverse([], L).
test(proper, L == [c,b,a]) :-
    reverse([a,b,c], L).
test(variables, L == [C,f(B),A]) :-
    reverse([A,f(B),C], L).
test(long) :-
    numlist(1, 100000, L),
    reverse(L, R),
    pl_reverse(L, R0),
    R == R0,
    reverse(R, L1),
    L1 == L.
test(bound_output) :-
    reverse([a,b,c], [c,b,a]),
    \+ reverse([a,b,c], [a,b,c]),
    \+ reverse([a,b,c], [c,b]),
    reverse([a,b,c], [c|T]),
    T == [b,a].
test(backtrack, true(var(R))) :-
    (   reverse([a,b,c], R),
        fail
    ;   true
    ).
test(c_used) :-                         % proper list, unbound output
    '$list_reverse'([a,b,c], R),
    R == [c,b,a].
test(c_not_used,
     [ forall(member(L, [_, [a|_], [a,b|_], foo, [a|foo], 1]))
     ]) :-
    '$list_reverse'(L, R),
    var(R).
test(same,
     [ forall(( member(Flag, [false, true, error]),
                member(Xs-Ys, [ []-_, [a]-_, [a,b,c]-_, [X,Y,X]-_,
                                [a|_]-_, [a,b|_]-_, _-_, T-T,
                                foo-_, [a|foo]-_, [a,b|foo]-_,
                                [Y]-Y, [a,Y]-Y, [Y,Y]-Y,
                                [a,b]-[b,a], [a,b]-[b|_], [a,b]-[b],
                                [a,b]-[_,_,_|_], _-[a,b], [a|_]-[b,a],
                                [a,b|_]-[_,_,_], [a,b]-foo
                              ])
              ))
     ]) :-
    same(Flag, Xs, Ys).
test(attributed_output,
     [ forall(member(Flag, [false, true, error]))
     ]) :-
    freeze(Ys, true),
    same(Flag, [a,b,c], Ys),
    freeze(X, true),
    same(Flag, [a,X,c], _),
    same(Flag, [a,b|X], _).
test(sandbox) :-
    safe_goal(reverse([a,b,c], _)).

:- end_tests(reverse).
