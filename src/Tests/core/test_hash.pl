/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2015, University of Amsterdam
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

:- module(test_hash,
	  [ test_hash/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_hash :-
	run_tests([ variant_sha1,
		    variant_hash
		  ]).

:- begin_tests(variant_sha1).

test(atom) :-
	variant_sha1(this_is_an_atom, Hash),
	atom_length(Hash, 40).
test(vars, Hash1 == Hash2) :-
	v(A), v(B),
	variant_sha1(x(A), Hash1),
	variant_sha1(x(B), Hash2).
test(variant, true) :-
	v(A), v(B),
	variant_sha1(x(A,A), Hash1),
	variant_sha1(x(A,B), Hash2),
	assertion(Hash1 \== Hash2).
test(shared, Hash1 == Hash2) :-
	A = x(C),
	variant_sha1(x(A,A), Hash1),
	variant_sha1(x(x(C),x(C)), Hash2).
					% error handling
test(cycle, [sto(rational_trees),error(type_error(acyclic_term, A))]) :-
	A = a(A),
	variant_sha1(A, _).
test(cycle, [sto(rational_trees),error(type_error(acyclic_term, _))]) :-
	A = a(A),
	variant_sha1(x(A), _).
test(attvar, error(_)) :-
	dif(X, 3),
	variant_sha1(X, _).
test(attvar, true) :-
	dif(X, 3), % error(_) fails because subsumes_term does not deal with attvar
	catch(variant_sha1(x(a(X)), _), _, true).
test(float, fail) :-
	variant_sha1(1.0, Hash),
	variant_sha1(2.0, Hash).

v(_).

:- end_tests(variant_sha1).

:- begin_tests(variant_hash).

test(variant, true) :-
	v(A), v(B),
	variant_hash(x(A,A), Hash1),
	variant_hash(x(A,B), Hash2),
	assertion(Hash1 \== Hash2).
test(variant, Hash1 == Hash2) :-
	v(A),
	freeze(B, true),
	variant_hash(x(A,A), Hash1),
	variant_hash(x(B,B), Hash2).
test(variant, Hash1 == Hash2) :-
	v(A),
	freeze(B, true),
	variant_hash(x(A,_), Hash1),
	variant_hash(x(B,_), Hash2).

v(_).

:- end_tests(variant_hash).
