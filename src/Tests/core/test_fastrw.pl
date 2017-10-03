/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2016, VU University Amsterdam
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

:- module(test_fastrw, [test_fastrw/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test fast term I/O

This module is a Unit test  for   Prolog  built-ins for fast binary term
serizalization

@author	Jan Wielemaker
*/

test_fastrw :-
	run_tests([ fastrw
		  ]).

term(int, 0).
term(int, -1).
term(int, 1).
term(int, X) :- X is 1<<62.
term(float, 0.0).
term(float, X) :- X is pi.
term(atom, '').
term(atom, aap).
term(atom, noot).
term(atom, A) :-
	numlist(1, 1100, L),
	atom_codes(A, L).
term(string, S) :-
	term(atom, A),
	atom_string(A, S).
term(nil, []).
term(compound, f(1)).
term(compound, f(1, f(2))).
term(cyclic, X) :- X = f(X).
term(list, L) :-
	numlist(-1000, 1000, L).

:- begin_tests(fastrw, [sto(rational_trees)]).

test(serialize) :-
	forall(term(_, T),
	       ( fast_term_serialized(T, S),
		 fast_term_serialized(T2, S),
		 assertion(T =@= T2))).
test(fastrw) :-
	findall(T, term(_,T), L),
	setup_call_cleanup(
	    tmp_file_stream(binary, File, Out),
	    maplist(fast_write(Out), L),
	    close(Out)),
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    maplist(read_and_check(In), L),
	    close(In)),
	delete_file(File).

read_and_check(In, T) :-
	fast_read(In, T2),
	assertion(T =@= T2).

test(error, error(permission_error(fast_serialize, blob, S))) :-
	setup_call_cleanup(
	    ( open_null_stream(S),
	      set_stream(S, encoding(octet))
	    ),
	    fast_write(S, a(X,X,S)),
	    close(S)).

:- end_tests(fastrw).
