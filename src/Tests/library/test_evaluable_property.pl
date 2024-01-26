/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_evaluable_property,
          [ test_evaluable_property/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(prolog_evaluable)).
:- use_module(library(debug)).

test_evaluable_property :-
    run_tests(evaluable_property).

:- begin_tests(evaluable_property).

test(consistency) :-
    forall(prolog_evaluable:eval_type(X,_),
           assertion(current_arithmetic_function(X))).
test(consistency) :-
    forall(prolog_evaluable:iso_function(X),
           assertion(current_arithmetic_function(X))).
test(det) :-
    evaluable_property(_+_, built_in).
test(ndet, nondet) :-
    evaluable_property(_+_, Prop),
    Prop = built_in.
test(templ, [T,R] =@= [cos(_),float]) :-
    evaluable_property(cos(2), template(T,R)).
test(iso) :-
    evaluable_property(1+2, iso).
test(error, error(type_error(callable, 1))) :-
    evaluable_property(1, _).

:- end_tests(evaluable_property).
