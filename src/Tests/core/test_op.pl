/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2021, University of Amsterdam
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

:- module(test_op, [test_op/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog operator handling

This module is a Unit test for  Prolog op/3, current_op/3, etc.

@author	Jan Wielemaker
*/

test_op :-
    run_tests([ current_op,
		op_syntax
	      ]).

:- begin_tests(current_op).

test(plus, set(P-T == [200-fy, 500-yfx])) :-
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

:- begin_tests(op_syntax).

:- op(100,  xf,	 xf100).
:- op(200,  xf,	 xf200).
:- op(300,  xf,	 xf300).
:- op(400,  xf,	 xf400).
:- op(500,  xf,	 xf500).
:- op(600,  xf,	 xf600).
:- op(700,  xf,	 xf700).
:- op(800,  xf,	 xf800).
:- op(900,  xf,	 xf900).
:- op(1000, xf,	 xf1000).
:- op(1100, xf,	 xf1100).
:- op(1200, xf,	 xf1200).

:- op(100,  yf,	 yf100).
:- op(200,  yf,	 yf200).
:- op(300,  yf,	 yf300).
:- op(400,  yf,	 yf400).
:- op(500,  yf,	 yf500).
:- op(600,  yf,	 yf600).
:- op(700,  yf,	 yf700).
:- op(800,  yf,	 yf800).
:- op(900,  yf,	 yf900).
:- op(1000, yf,	 yf1000).
:- op(1100, yf,	 yf1100).
:- op(1200, yf,	 yf1200).

:- op(100,  fx,	 fx100).
:- op(200,  fx,	 fx200).
:- op(300,  fx,	 fx300).
:- op(400,  fx,	 fx400).
:- op(500,  fx,	 fx500).
:- op(600,  fx,	 fx600).
:- op(700,  fx,	 fx700).
:- op(800,  fx,	 fx800).
:- op(900,  fx,	 fx900).
:- op(1000, fx,	 fx1000).
:- op(1100, fx,	 fx1100).
:- op(1200, fx,	 fx1200).

:- op(100,  fy,	 fy100).
:- op(200,  fy,	 fy200).
:- op(300,  fy,	 fy300).
:- op(400,  fy,	 fy400).
:- op(500,  fy,	 fy500).
:- op(600,  fy,	 fy600).
:- op(700,  fy,	 fy700).
:- op(800,  fy,	 fy800).
:- op(900,  fy,	 fy900).
:- op(1000, fy,	 fy1000).
:- op(1100, fy,	 fy1100).
:- op(1200, fy,	 fy1200).

:- op(100,  xfx, xfx100).
:- op(200,  xfx, xfx200).
:- op(300,  xfx, xfx300).
:- op(400,  xfx, xfx400).
:- op(500,  xfx, xfx500).
:- op(600,  xfx, xfx600).
:- op(700,  xfx, xfx700).
:- op(800,  xfx, xfx800).
:- op(900,  xfx, xfx900).
:- op(1000, xfx, xfx1000).
:- op(1100, xfx, xfx1100).
:- op(1200, xfx, xfx1200).

:- op(100,  xfy, xfy100).
:- op(200,  xfy, xfy200).
:- op(300,  xfy, xfy300).
:- op(400,  xfy, xfy400).
:- op(500,  xfy, xfy500).
:- op(600,  xfy, xfy600).
:- op(700,  xfy, xfy700).
:- op(800,  xfy, xfy800).
:- op(900,  xfy, xfy900).
:- op(1000, xfy, xfy1000).
:- op(1100, xfy, xfy1100).
:- op(1200, xfy, xfy1200).

:- op(100,  yfx, yfx100).
:- op(200,  yfx, yfx200).
:- op(300,  yfx, yfx300).
:- op(400,  yfx, yfx400).
:- op(500,  yfx, yfx500).
:- op(600,  yfx, yfx600).
:- op(700,  yfx, yfx700).
:- op(800,  yfx, yfx800).
:- op(900,  yfx, yfx900).
:- op(1000, yfx, yfx1000).
:- op(1100, yfx, yfx1100).
:- op(1200, yfx, yfx1200).

parse(String, Term) :-
    context_module(M),
    term_string(Term, String, [module(M)]).

test(parse, Term == xf200(xf100)) :-
    parse("xf100 xf200", Term).
test(parse, error(syntax_error(_))) :-
    parse("xf100 xf200 xf100", _).
test(parse, Term == xf200(fx100(1))) :-
    parse("fx100 1 xf200", Term).
test(parse, Term == ','(p, xfy900(yf100(q),c))) :-
    parse("p, q yf100 xfy900 c", Term).

:-  end_tests(op_syntax).
