/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(test_read_attvar,
          [ test_read_attvar/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test suite for reading Var{...}

This construct is  read  as  attributed   variable  if  the  Prolog flag
`var_tag`  is  set  to  `attvar`.  In  addition  to  reading  attributed
variables, this implements the reserved attribute   `=`, which binds the
variable `Var`. This construct provides _labeled sub terms_ which allows
for creating cyclic terms  or,  more   in  general,  arbitrary  directed
graphs.
*/

test_read_attvar :-
    run_tests([ test_read_attvar,
                test_read_labeled,
                test_read_failure
              ]).

% Read Var{...} as an attributed variable.
:- set_prolog_flag(var_tag, attvar).

local_term_string(Term, String) :-
    context_module(M),
    term_string(Term, String, [module(M)]).

:- begin_tests(test_read_attvar).

test(attr, X == 1) :-
    local_term_string(T, "_{x:1}"),
    assertion(attvar(T)),
    get_attr(T, x, X).
test(attr, X == 1) :-
    local_term_string(T, "t(A{x:1}, A)"),
    arg(1, T, A1),
    arg(2, T, A2),
    assertion(A1 == A2),
    get_attr(A1, x, X).
test(attr, X == 1) :-
    local_term_string(T, "t(A, A{x:1})"),
    arg(1, T, A1),
    arg(2, T, A2),
    assertion(A1 == A2),
    get_attr(A1, x, X).
test(attr, X == B) :-
    local_term_string(T, "t(A, A{x:B}, B)"),
    arg(1, T, A1),
    arg(2, T, A2),
    arg(3, T, B),
    assertion(A1 == A2),
    get_attr(A1, x, X).

:- end_tests(test_read_attvar).

:- begin_tests(test_read_labeled).

test(label, T == 1) :-
    local_term_string(T, "X{= : 1}").
test(label, T == Y) :-
    local_term_string(T, "X{= : f(X)}"),
    Y = f(Y).
test(label, error(syntax_error(label_on_attvar))) :-
    local_term_string(_, "X{= : Y, x:1}").

:- end_tests(test_read_labeled).

:- begin_tests(test_read_failure).

test_read_fail:attr_unify_hook(V1, V2) :-
    get_attr(V1, test_read_fail, T),
    get_attr(V2, test_read_fail, T).

test(fail, error(syntax_error(duplicate_label_value))) :-
    local_term_string(_T, "t(X{= : 1}, X{= : 2})").
test(fail, error(syntax_error(duplicate_attribute))) :-
    local_term_string(_T, "t(X{test_read_fail: 1}, X{test_read_fail: 2})").

:- end_tests(test_read_failure).
