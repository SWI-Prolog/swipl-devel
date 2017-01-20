/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2012, University of Amsterdam
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

:- module(test_op, [test_op/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog operator handling

This module is a Unit test for  Prolog op/3, current_op/3, etc.

@author	Jan Wielemaker
*/

test_op :-
	run_tests([ current_op
		  ]).

:- begin_tests(current_op).

test(plus, all(P-T == [200-fy, 500-yfx])) :-
	current_op(P, T, +).
test(no_atom, error(type_error(atom, 1))) :-
	current_op(_, _, 1).
test(no_atom, error(type_error(atom, 1))) :-
	current_op(_, 1, _).
test(bad_type, error(domain_error(operator_specifier, xxx))) :-
	current_op(_, xxx, _).
test(bad_precedence, error(type_error(integer, x))) :-
	current_op(x, _, _).
test(bad_precedence, error(type_error(integer, 1.2))) :-
	current_op(1.2, _, _).
test(inherit, true) :-
	current_op(500, yfx, +),
	op(0, xfx, +),
	(   current_op(_, yfx, +)
	->  fail
	;   op(500, yfx, +)
	).

:- end_tests(current_op).
