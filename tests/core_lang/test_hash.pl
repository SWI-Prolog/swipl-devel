/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2025, University of Amsterdam
			    SWI-Prolog Solutions b.v.
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
		    variant_hash,
		    term_hash2
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
test(numbered_vars,		% variables are hashed by number, not by cell
     [ Hash == '669ed46a96591cd32bc02101dbc2d1341d242700',
       VHash == 13_096_724
     ]) :-
	variant_sha1(f(X,Y,X,Y), Hash),
	variant_hash(f(X,Y,X,Y), VHash).
test(platform,			% the same hash on all platforms
     [ Hash == '6afcd5544dd2de42ffa7a721af18f08fd77c2130',
       WHash == d07c530ffa18a37458b596e1d9c0c7ef17310536,
       SHash == '457c2c0c8fa8a026e7ddb87716e229aa82646fc4'
     ]) :-
	variant_sha1(f(aap, "str", 1.5, 42, [x]), Hash),
	atom_codes(W, [0x1F600, 0x3B1, 0x61]),
	variant_sha1(f(W), WHash),
	string_codes(S, [0x1F600, 0x3B1]),
	variant_sha1(f(S), SHash).
					% cyclic terms
test(cycle, [sto(rational_trees)]) :-
	A = a(A),
	variant_sha1(A, Hash),
	atom_length(Hash, 40).
test(cycle, [sto(rational_trees)]) :-
	A = a(A),
	variant_sha1(x(A), Hash),
	atom_length(Hash, 40).

%	Terms denoting the same infinite tree hash alike however many cells
%	they were written with.  This is the example the documentation gives
%	for what a canonical cycle has to do.

test(cycle_variant, [sto(rational_trees), Hash1 == Hash2]) :-
	A = [a|A],
	B = [a,a|B],
	variant_sha1(A, Hash1),
	variant_sha1(B, Hash2).
test(cycle_distinct, [sto(rational_trees)]) :-
	A = a(A),
	B = b(B),
	variant_sha1(A, Hash1),
	variant_sha1(B, Hash2),
	assertion(Hash1 \== Hash2).

%	A cyclic term is hashed over its canonical form, so the digest must
%	not be the one that form would get as a term in its own right.

test(cycle_no_collision, [sto(rational_trees)]) :-
	A = [a|A],
	variant_sha1(A, Hash1),
	'$term_canonical_form'(A, Form),
	variant_sha1(Form, Hash2),
	assertion(Hash1 \== Hash2).

%	Attributed terms stay refused, cyclic or not.

test(cycle_attvar, [sto(rational_trees), error(type_error(free_of_attvar, _))]) :-
	dif(X, 3),
	A = a(A,X),
	variant_sha1(A, _).

%	variant_hash/2 does take attributed variables, as plain ones.

test(cycle_attvar_hash, [sto(rational_trees)]) :-
	dif(X, 3),
	A = a(A,X),
	variant_hash(A, Hash),
	integer(Hash).

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
test(long_atom, true(Hash1 \== Hash2)) :-	% Issue #1538
	long_text(atom, 0'x, A),
	long_text(atom, 0'y, B),
	variant_hash(A, Hash1),
	variant_hash(B, Hash2).
test(long_string, true(Hash1 \== Hash2)) :-
	long_text(string, 0'x, A),
	long_text(string, 0'y, B),
	variant_hash(A, Hash1),
	variant_hash(B, Hash2).

long_text(Type, Tail, Text) :-
	Spec =.. [Type, Text],
	format(Spec, '~*c~*c', [300, 0'a, 10, Tail]).

v(_).

:- end_tests(variant_hash).

:- begin_tests(term_hash2).

                 /*******************************
                 *           TERM-HASH          *
                 *******************************/

% Hashes are the same on all platforms: integers are hashed as
% little-endian 64-bit values, wide text as little-endian code points,
% and numbers by value (GMP and LibBF agree).

test(simple_1, X == 3_331_158_974) :-
    term_hash(aap, X).
test(simple_2, X == 1_220_239_556) :-    % small int
    term_hash(42, X).
test(simple_3, X == 2_344_943_952) :-    % big int
    Num is 366454713<<64,
    term_hash(Num, X).
test(simple_4, X == 4_260_353_184) :-
    A is pi,
    term_hash(A, X).
test(simple_5, X == 1_540_712_431) :-
    string_codes(S, "hello world"),
    term_hash(S, X).
test(simple_6, X == 2_089_503_823) :-    % small or big depending on the layout
    N is 2**60+1,
    term_hash(N, X).
test(simple_7,                           % rational
     [ condition(current_prolog_flag(bounded, false)),
       X == 1_619_139_142
     ]) :-
    R is rdiv(1, 3),
    term_hash(R, X).
test(simple_8, X == 1_634_837_417) :-    % wide atom
    atom_codes(A, [0x1F600, 0x3B1]),
    term_hash(A, X).
test(compound_1, X == 3_978_712_206) :-
    term_hash(hello(world), X).
test(compound_2, X == 3_797_822_550) :-
    A = x(a),
    term_hash(hello(A, A), X).
test(compound_3, X == 3_797_822_550) :-
    term_hash(hello(x(a), x(a)), X).

:- end_tests(term_hash2).
