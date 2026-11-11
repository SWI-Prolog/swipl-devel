/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(functor_create_race,
	  [ functor_create_race/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Create the same new functors concurrently

Several threads create the same  new  functors   while  the  functor
table is rehashed.  A functor that was added during a rehash could be
missing from the new table, after which a second functor with the same
name and arity was created.
*/

functor_create_race :-
	forall(between(1, 3, Round), round(Round)).

round(Round) :-
	N = 20000,
	findall(T, ( between(1, 4, I),
		     thread_create(make(Round, I, N), T, [])
		   ), Threads),
	maplist(collect, Threads, Lists),
	Lists = [L0|Rest],
	maplist(same_functors(L0), Rest).

make(Round, I, N) :-
	numlist(1, N, Is0),
	(   I mod 2 =:= 0
	->  reverse(Is0, Is)
	;   Is = Is0
	),
	maplist(make_term(Round), Is, L0),
	msort(L0, L),
	thread_self(Me),
	thread_send_message(main, result(Me, L)).

make_term(Round, I, T) :-
	format(atom(Name), 'fcr_~w_~w', [Round, I]),
	functor(T, Name, 3).

collect(T, L) :-
	thread_get_message(result(T, L)),
	thread_join(T, Status),
	assertion(Status == true).

same_functors(L0, L) :-
	length(L0, Len),
	length(L, Len),
	maplist(same_functor, L0, L).

same_functor(T0, T) :-
	(   T0 =@= T
	->  true
	;   functor(T0, N, A),
	    format(user_error, 'Duplicate functor ~q/~w~n', [N, A]),
	    fail
	).
