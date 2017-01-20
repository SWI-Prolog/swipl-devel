/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

:- module(test_call_residue_vars,
	  [ test_call_residue_vars/0
	  ]).
:- use_module(library(plunit)).

test_call_residue_vars :-
	run_tests([ call_residue_vars
		  ]).

:- begin_tests(call_residue_vars).

test(freeze_in, Vars == [X]) :-
	call_residue_vars(freeze(X, true), Vars).
test(freeze_bind, Vars == []) :-
	call_residue_vars((freeze(X, true), X = 1), Vars).
test(freeze_out, Vars == []) :-
	x(X),
	freeze(X, true),
	call_residue_vars(true, Vars).
test(freeze_oi, Vars == [X]) :-
	x(X),
	freeze(X, true),
	call_residue_vars(freeze(X, fail), Vars).
test(nogc, Vars = [_]) :-
	call_residue_vars(gc_able, Vars).
test(gc, Vars = [_]) :-
	call_residue_vars((gc_able, garbage_collect), Vars).
test(gc2, Vars = [_]) :-
	call_residue_vars(gc_able2_gc, Vars).
test(modify, Vars == [X]) :-
	put_attr(X, a, 1),
	call_residue_vars(put_attr(X, a, 2), Vars).
test(trail, [all(Vars == [[]])]) :-
	G=(freeze(X,X=1),X=1),
	call_residue_vars(G,Vars),
	(true;Vars=[2]).
test(frozen_stacks, Vars == []) :-
	x(X),
	call_residue_vars(
	    (	freeze(X, true),
		nb_setval(x, a(b)),
		fail
	    ;   true
	    ),
	    Vars).
test(copy_term) :-
	T = x(X), put_attr(X, a, 1),
	copy_term(T, T2),
	garbage_collect,
	x(T2).
test(copy_term, Vars == [V]) :-
	T = x(X), put_attr(X, a, 1),
	call_residue_vars(copy_term(T, T2), Vars),
	arg(1, T2, V).
test(copy_term, Vars == [V]) :-
	put_attr(X, a, 1),
	T = x(X,X),
	call_residue_vars(copy_term(T, T2), Vars),
	arg(1, T2, V).
test(record) :-
	T = x(X), put_attr(X, a, 1),
	cp_record(T, T2),
	garbage_collect,
	x(T2).
test(record, Vars == [V]) :-
	T = x(X), put_attr(X, a, 1),
	call_residue_vars(cp_record(T, T2), Vars),
	arg(1, T2, V).
test(record, Vars == [V]) :-
	put_attr(X, a, 1),
	T = x(X,X),
	call_residue_vars(cp_record(T, T2), Vars),
	arg(1, T2, V).

cp_record(T,T2) :-			% copy using recorded DB
	findall(T2, T2=T, [T2]).

x(_).					% avoid singleton warnings

gc_able :-
	gc_able2.

gc_able2 :-
	x(X),
	freeze(X, fail).

gc_able2_gc :-
        freeze(X, writeln(X)),
        garbage_collect.

:- end_tests(call_residue_vars).
