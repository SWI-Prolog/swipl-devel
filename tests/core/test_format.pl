/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2024, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_format, [test_format/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog text formatting primitives

This module is a Unit test for  Prolog format/2, etc.

@author	Jan Wielemaker
*/

test_format :-
	run_tests([ format
		  ]).

:- begin_tests(format).

test(fail, fail) :-
	format('~@', [fail]).
test(fail, throws(error(42))) :-
	format('~@', [throw(error(42))]).
test(no_stream, error(existence_error(stream, S))) :-
	S = stream_dhfuweiu,
	format(S, fmt, []).
test(atom, A == 'a\n') :-
	format(atom(A), 'a\n', []).
test(atom, A == '--++') :-
	format(atom(A), '~`-t~`+t~4+', []).
test(radix, A == "101") :-
	format(string(A), '~2r', [5]).
test(radix, error(domain_error(radix, 1))) :-
	format(string(_), '~1r', [5]).
test(radix, error(domain_error(radix, 37))) :-
	format(string(_), '~37r', [5]).
test(asterisk, error(format('no or negative integer for `*\' argument'))) :-
    format('~t~*|', [-1]).
test(intD_1, X == '1,000') :-
    format(atom(X), '~D', [1000]).
test(intD_2, X == '10.00') :-
    format(atom(X), '~2D', [1000]).
test(intD_3, X == '1,000.00') :-
    format(atom(X), '~2D', [100000]).
test(intr_1, X == '3e8') :-
    format(atom(X), '~16r', [1000]).
test(intR_1, X == '3E8') :-
    format(atom(X), '~16R', [1000]).
test(large_float_1, Len == 108) :-
    format(atom(X), '~6f', [1e100]),
    atom_length(X, Len).                     % 1 with 100 0s + ".000000" = 108.
test(oncodes_1) :-
    format(codes(C), 'hello ~w', [world]),
    atom_codes('hello world', C).
test(oncodes_2, C-T =@= C2-T2) :-
    format(codes(C,T), 'hello ~w', [world]),
    atom_codes('hello world', HW),
    append(HW, T2, C2).
test(onstring_1) :-
    format(string(S), 'hello ~w', [world]),
    string(S),
    atom_string('hello world', S).

:- end_tests(format).
