/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009, University of Amsterdam
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

:- module(test_ch_shift,
	  [ test_ch_shift/0
	  ]).

test_ch_shift :-
	or_dept(Depth),
	test1(Depth),
	test2.

or_dept(Depth), current_prolog_flag(asan,true) => Depth = 1000.
or_dept(Depth), current_prolog_flag(emscripten,true) => Depth = 1000.
or_dept(Depth), current_prolog_flag(windows,true) => Depth = 5000.
or_dept(Depth) => Depth = 10_000.

%%	test1(+Depth) is det.
%
%	Tests expansion of the local stack due to a clause with many
%	choicepoints.

test1(Depth) :-
	trim_stacks,
	make_or(Depth, OR),
	asserta((t :- OR), Ref),
	once(t),
	erase(Ref).

make_or(0, a) :- !.
make_or(N, (G;a)) :-
	N2 is N - 1,
	make_or(N2, G).

a.


%%	test2
%
%	Tests local stack shifting when there are pending choicepoint frames.

test2 :-
	test2_1, !.

test2_1 :-
	test2_2, true.

test2_2 :-
	setup_call_cleanup(true, cp, lshift).

cp.
cp.

lshift :-
	statistics(local_shifts, S0),
	lshift(S0), !, garbage_collect.

lshift(S0) :-
	statistics(local_shifts, S0),
	lshift(S0).
lshift(_).
