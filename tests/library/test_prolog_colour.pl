/*  Part of SWI-Prolog

    Author:        Eshel Yaron
    E-mail:        eshel@swi-prolog.org
    WWW:           www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(test_prolog_colour,
          [ test_prolog_colour/0
          ]).

:- use_module(library(debug), [assertion/1]).
:- use_module(library(plunit)).

/** <module> Test set for library(prolog_colour)

*/

test_prolog_colour :-
        run_tests([ prolog_colour
                  ]).

:- begin_tests(prolog_colour).
:- use_module(library(prolog_colour)).

:- dynamic range_class/3.

assert_range_class(Class, Beg, Len) :-
    asserta(range_class(Beg, Len, Class)).

test(function) :-
    retractall(range_class(_, _, _)),
    prolog_colourise_query("cos(pi) =:= - sin(pi/2) * _{foo:1}.foo / bar",
                           user,
                           assert_range_class),
    !,
    assertion(range_class(0,  3, function)),
    assertion(range_class(4,  2, function)),
    assertion(range_class(12, 1, function)),
    assertion(range_class(14, 3, function)),
    assertion(range_class(18, 2, function)),
    assertion(range_class(20, 1, function)),
    assertion(range_class(24, 1, function)),
    assertion(range_class(26, 8, dict)),
    assertion(range_class(34, 1, functor)),
    assertion(range_class(35, 3, atom)),
    assertion(range_class(39, 1, function)),
    assertion(range_class(41, 3, no_function)).

:- end_tests(prolog_colour).
