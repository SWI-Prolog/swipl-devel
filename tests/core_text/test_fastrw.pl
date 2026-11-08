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
serialization

@author	Jan Wielemaker
*/

test_fastrw :-
	run_tests([ fastrw,
		    fastrw_safety
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
term(bigint, X) :- current_prolog_flag(max_tagged_integer, X0), X is X0+1.
term(bigint, X) :- current_prolog_flag(min_tagged_integer, X0), X is X0-1.
term(bigint, X) :- X is -(1<<200).
term(rational, X) :- X is -22 rdiv 7.
term(rational, X) :- X is (1<<100) rdiv 3.
term(wide, '\u2603atom').
term(wide, "\u2603string").
term(compound, f()).
term(dict, _{a:1, b:"x", c:[1,2]}).
term(dict, T) :- T =.. [dict, t, b, 1, a, 2].	% not a valid dict
term(cyclic, X) :- X = f(_{a:X}).		% used to crash
term(attvar, T) :- put_attr(V, test_fastrw, V), T = t(V, V).

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

/* Check that corrupted data is rejected safely.  Before decoding, the
   data is validated, so fast_term_serialized/2 fails and fast_read/2
   raises a syntax error.  What matters is that the process survives
   and any decoded term is valid.
*/

:- begin_tests(fastrw_safety, [sto(rational_trees)]).

safety_term(T) :-
	X = f(_{k:X, l:"\u2603"}),
	put_attr(V, test_fastrw, val),
	T = t(X, V, V, W, W, 'w\u2603', 1r3, -22r7, 1.5,
	      123456789012345678901234567890, -7, [a,b|c], "str", f()).

test(corrupt_byte) :-
	safety_term(T),
	fast_term_serialized(T, S),
	string_codes(S, Codes),
	length(Codes, Len),
	forall(( between(1, Len, I),
		 nth1(I, Codes, B0),
		 member(B, [0, 1, 0x7f, 0x80, 0xff]), B \== B0
	       ),
	       ( set_nth1(I, Codes, B, Codes1),
		 decode_codes(Codes1)
	       )).
test(truncated) :-
	safety_term(T),
	fast_term_serialized(T, S),
	string_codes(S, Codes),
	forall(append(Prefix, [_|_], Codes),
	       \+ ( Prefix \== [],
		    string_codes(S1, Prefix),
		    fast_term_serialized(_, S1) )).
test(no_int64_indirect, [condition(current_prolog_flag(bounded, false))]) :-
	% PL_TYPE_INTEGER (3) is not valid with bignums: indirect integers
	% are MPZ numbers.  Used to be accepted as a malformed MPZ.
	fast_term_serialized(f(a), S0),
	string_codes(S0, [M|_]),
	string_codes(S, [M, 15, 5, 13, 1, 11, 1, 0'f, 3, 8, 0x40,0,0,0,0,0,0,0]),
	\+ fast_term_serialized(_, S).
test(fast_read_header) :-
	fast_term_serialized(f(a), S),
	string_codes(S, [M|_]),
	forall(bad_header(M, Bytes),
	       catch(( read_bytes(Bytes, _), fail ),
		     error(syntax_error(_), _),
		     true)).

decode_codes(Codes) :-
	string_codes(S, Codes),
	(   fast_term_serialized(T, S)
	->  garbage_collect,
	    catch(\+ \+ ( numbervars(T, 0, _, [attvar(skip)]),
			  format(string(_), "~q", [T]) ),
		  _, true)
	;   true
	).

set_nth1(1, [_|T], X, [X|T]) :- !.
set_nth1(I, [H|T0], X, [H|T]) :- I1 is I-1, set_nth1(I1, T0, X, T).

bad_header(M, [M, 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f, 1, 0]).
bad_header(M, [M, 0x81,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f, 1, 0]).
bad_header(M, [M, 0x84,0x80,0x80,0x80,0x80,0x00, 2, 9]).
bad_header(M, [M, 10, 2, 9]).
bad_header(M, [M, 9, 2, 0'A, 0'B, 0'C, 0'D, 0'E, 0'F, 0'G, 0'H, 0'I]).

read_bytes(Bytes, T) :-
	setup_call_cleanup(
	    tmp_file_stream(binary, File, Out),
	    format(Out, "~s", [Bytes]),
	    close(Out)),
	call_cleanup(
	    setup_call_cleanup(
		open(File, read, In, [type(binary)]),
		fast_read(In, T),
		close(In)),
	    delete_file(File)).

:- end_tests(fastrw_safety).
