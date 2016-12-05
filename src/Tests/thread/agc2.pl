/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2009, University of Amsterdam
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

:- module(agc2,
	  [ agc2/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test does intensive atom-manipulation with   a  thread doing AGC in
the background. It was used to spot a bug in the the interaction of atom
marking and atom-reference counting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

agc :-
	repeat,
	sleep(0.01),
	garbage_collect_atoms,
	fail.

agc2 :-
	thread_create(agc, Id, []),
	go(5000),
	thread_signal(Id, abort),
	thread_join(Id, _),
	garbage_collect_atoms.

go(N) :-
	make_atoms(N, xxxx, L),
	check_atoms(N, xxxx, L).


make_atoms(0, _, []) :- !.
make_atoms(N, Prefix, [H|T]) :-
	atom_concat(Prefix, N, H),
	N2 is N - 1,
	make_atoms(N2, Prefix, T).


check_atoms(0, _, []) :- !.
check_atoms(N, Prefix, [H|T]) :-
	atom_concat(Prefix, N, H0),
	(   H0 == H
	->  true
	;   format('~q <-> ~q~n', [H0, H]),
	    fail
	),
	N2 is N - 1,
	check_atoms(N2, Prefix, T).


