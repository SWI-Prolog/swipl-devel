/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008, University of Amsterdam
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

:- module(thread_agc_findall,
	  [ thread_agc_findall/0,
	    thread_agc_findall/2	% +Threads, +Count
	  ]).
:- use_module(library(debug)).

%%	thread_agc_findall
%
%	Test interaction of findall  without   locking  atoms in records
%	with AGC. Like thread_agc_queue, we create atoms in a thread and
%	verify we do not loose anything.

thread_agc_findall :-
	thread_agc_findall(4, 10000).

thread_agc_findall(Threads, Count) :-
	current_prolog_flag(agc_margin, Old),
	set_prolog_flag(agc_margin, 1000),
	call_cleanup(test(Threads, Count),
		     set_prolog_flag(agc_margin, Old)).

test(Threads, Count) :-
	numlist(1, Threads, Is),
	maplist(create_test(Count), Is, Ids),
	maplist(thread_join, Ids, States),
	maplist(==(true), States).

create_test(Count, I, Id) :-
	prefix(I, Prefix),
	thread_create(test_find(Prefix, Count), Id, []).

prefix(N, Prefix) :-
	A is 0'A+N,
	atom_codes(Prefix, [A,A,A]).

test_find(Prefix, N) :-
	findall(Atom, gen_atom(N, Prefix, Atom), Atoms),
	check_atoms(Atoms, 1, N, Prefix).

gen_atom(High, Prefix, Atom) :-
	between(1, High, N),
	atom_concat(Prefix, N, Atom).

check_atoms([], I, N, _Prefix) :-
	assertion(I=:=N+1).
check_atoms([H|T], I, N, Prefix) :-
	atom_concat(Prefix, Rest, H),
	atom_number(Rest, Num),
	assertion(I == Num),
	I2 is I + 1,
	check_atoms(T, I2, N, Prefix).
