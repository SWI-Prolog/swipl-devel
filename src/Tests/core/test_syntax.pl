/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, SWI-Prolog Solutions b.v.
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

:- module(test_syntax,
          [ test_syntax/0
          ]).
:- use_module(library(plunit)).

test_syntax :-
    run_tests([ syntax,
                iso_op_table_6
              ]).

:- meta_predicate
    term_string_(-, :).

term_string_(Term, M:String) :-
    term_string(Term, String, [module(M)]).

:- begin_tests(syntax).

:- op(600, fx, fx600).
:- op(600, fy, fy600).
:- op(500, xf, xf500).
:- op(100, yf, af).

test(op_1) :-
    term_string(+(3,*(4,5)), "3+4*5").
test(op_2) :-
    term_string(+(+(1,2),3), "1+2+3").
test(op_3, error(syntax_error(operator_clash))) :-
    term_string(_, "a:-b:-c").
test(op_4) :-
    context_module(M),
    term_string(fx600(+(1,2)), "fx600 1+2", [module(M)]).
test(op_5, error(syntax_error(operator_clash))) :-
    context_module(M),
    term_string(_, "fx600 fx600 1", [module(M)]).
test(op_6) :-
    context_module(M),
    term_string(fy600(fy600(1)), "fy600 fy600 1", [module(M)]).
test(op_7) :-
    context_module(M),
    term_string(fy600(xf500(a)), "fy600 a xf500", [module(M)]).
test(op_8) :-                         % assume 200 fy and 500 yfx
    term_string(-(-(a)), "- - a").
test(atom_1) :-
    atom_codes('\003\\'\n\x80\', X),
    X == [3, 39, 10, 128].
test(char_1) :-
    10 == 0'\n.
test(char_2) :-
    52 == 0'\x34.
test(char_3) :-
    "\\" =:= 0'\\.
test(char_4) :-
    1-48 == 1-0'0.
test(cannot_start_term_1, error(syntax_error(cannot_start_term))) :-
    term_string(_, "p(]").

test(string_1) :-
    '\c ' == ''.
test(string_2) :-
    'x\c y' == xy.
test(quote_1, error(syntax_error(undefined_char_escape(x)))) :-
    term_string(_, '\'\\x\'').
test(quote_2) :-
    '\x61' == a.
test(quote_3) :-
    '\x61\' == a.
test(quote_4) :-
    char_code('\'', 39).
test(quote_5) :-
    0'\' == 0''.
test(quote_6) :-
    0'\' == 0'''.
test(quote_7, T == '') :-
    term_string(T, '\'\\\n\'').
test(base_1) :-
    1+1 == 1+1.
test(base_2) :-
    16'af == 175.
test(base_3) :-
    10 af == af(10).
test(base_4) :-
    A = 1, B is A+1, B == 2.
test(base_5) :-
    A is 1.0e+0+1, A == 2.0.
test(number_2, error(syntax_error(end_of_file))) :-
    term_string(_, '2\'').
test(zero_1) :-
    term_string(T, 'hello("\000\x")'),
    T = hello(A0),
    (   A0 == [0,120]               % depending on the double quotes flag
    ;   string_codes(A0, [0,120])
    ;   A0 == ['\u0000', x]
    ),
    !.
test(latin_1) :-
    atom_codes(A, [247]),
    term_string(T, A),
    atom_codes(T, [247]).

:- end_tests(syntax).

:- begin_tests(iso_op_table_6).

:- op(100, fy,  fy).
:- op(100, xfy, xfy).
:- op(100, yfx, yfx).
:- op(100, yf,  yf).

test(r1, T == fy(fy(1))) :-
    term_string_(T, "fy fy 1").
test(r2, T == xfy(1, xfy(2,3))) :-
    term_string_(T, "1 xfy 2 xfy 3").
test(r3, T == xfy(1, yfx(2,3))) :-
    term_string_(T, "1 xfy 2 yfx 3").
test(r4, T == fy(yf(2))) :-
    term_string_(T, "fy 2 yf").
test(r5, T == yf(yf(1))) :-
    term_string_(T, "1 yf yf").
test(r6, T == yfx(yfx(1,2),3)) :-
    term_string_(T, "1 yfx 2 yfx 3").

:- end_tests(iso_op_table_6).

