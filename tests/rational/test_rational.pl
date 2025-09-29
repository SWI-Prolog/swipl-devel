/*  Part of SWI-Prolog

    Author:        Rick Workman and Jan Wielemaker
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
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



:- module(test_rational, [test_rational/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test Prolog core arithmetic functions

@author	Jan Wielemaker and Rick Workman
*/

test_rational :-
    run_tests([ rational,
                rationalize
	      ]).

set_prefer_rationals(Old, New) :-
    current_prolog_flag(prefer_rationals, Old),
    set_prolog_flag(prefer_rationals, New).

set_float_undefined(Old, New) :-
    current_prolog_flag(float_undefined, Old),
    set_prolog_flag(float_undefined, New).



:- begin_tests(rational,
	       [ condition(current_prolog_flag(bounded, false)),
                 setup(set_float_flags(Old,
                                       [ flag(prefer_rationals,true),
                                         flag(float_undefined,nan),
                                         flag(rational_syntax,compatibility)
                                       ])),
                 cleanup(set_float_flags(_, Old))
	       ]).
:- set_prolog_flag(rational_syntax, compatibility).

test(filters) :-
    X = 1r3,
    assertion(rational(X)),
    assertion(number(X)),
    assertion(atomic(X)),
    assertion(nonvar(X)),
    assertion(ground(X)),
    assertion(\+(integer(X))),
    assertion(\+(float(X))),
    assertion(\+(atom(X))),
    assertion(\+(compound(X))),
    assertion(\+(blob(X, _))),
    assertion(\+(string(X))),
    assertion(\+(var(X))).

test(construct) :-
    X = 1r3,
    assertion(functor(X,X,0)),
    assertion(X=..[X]).

test(compare) :-
    X = 1r3,
    assertion((X < 1, X > 0)),
    assertion((X @< 1, X @> 0)),
    assertion(sort([1,0,X],[0,X,1])).

test(keep_precision) :-
    assertion(3r2 is 1r2 + 1),
    assertion(5r6 is 1r2 + 1r3),
    assertion(-1r2 is 1r2 - 1),
    assertion(1r6 is 1r2 - 1r3),
    assertion(3r2 is 1r2 * 3),
    assertion(1r6 is 1r2 * 1r3),
    assertion(1r6 is 1r2 / 3),
    assertion(3r2 is 1r2 / 1r3),
    assertion(1r9 is 1r3 ** 2).

test(conversion) :-
    assertion(2 = 4r2),
    assertion(1 is floor(3r2)),
    assertion(2 is ceiling(3r2)),
    assertion(2 is round(5r3)),
    assertion(1 is truncate(3r2)),
    assertion(-1 is truncate(-3r2)),
    assertion(2 is integer(5r3)),
    assertion((R1 is float(5r3), R2 is (5)/(3), check_error(R1,R2))),
    assertion(rationalize((1)/(2)) =:= 1r2).

test(int_to_rat) :-
    assertion(div0err(0** -1)),  % additional test for negative integer exp.
    assertion(div0err(0** -1r2)),
    assertion(8 is 4**3r2),
    assertion(1.5NaN is -4**3r2),  % neg. base, even Q
    assertion(2 is 8**1r3),
    assertion(-2 is -8**1r3),
    assertion(4 is 8**2r3),
    assertion(4 is -8**2r3),
    assertion(1r2 is 8** -1r3),
    assertion(-1r2 is -8** -1r3),
    assertion(2.0**1r2 =:= sqrt(2.0)).

test(rat_to_rat) :-
    assertion(1r8 is 1r4**3r2),
    assertion(1.5NaN is -1r4**3r2),  % neg. base, even Q
    assertion(1r2 is 1r8**1r3),
    assertion(-1r2 is -1r8**1r3),
    assertion(1r4 is 1r8**2r3),
    assertion(1r4 is -1r8**2r3),
    assertion(2 is 1r8** -1r3),
    assertion(-2 is -1r8** -1r3).

test(pow_special) :-
    assertion(1 is 1r4**0),
    assertion(1 is 1**1r2),
    assertion(0 is 0**1r2),
    assertion(1 is -1**2),
    assertion(-1 is -1**3),
    assertion(div0err(0.0** -1r2)).

test(other_arith) :-
    assertion(div0err(1r2/0)),
    assertion(1r3 is abs(1r3)),
    assertion(1r3 is abs(-1r3)),
    assertion(1 is sign(1r3)),
    assertion(-1 is sign(-1r3)),
    assertion(1r3 is copysign(1r3,1r2)),
    assertion(-1r3 is copysign(1r3,-1r2)).

% Others?:
%	nth_integer_root_and_remainder/4	integers only
%	mod, rem, //, div, gcd, random, powm	functions on integers
%	sqrt, transcendentals, etc.		assumed to always produce a float.

test(strings) :-
    assertion(number_string(1r3,"1r3")),
    assertion(number_string(-1r3,"-1r3")),
    assertion(term_string(1r3,"1r3")),
    assertion(term_string(1,"3r3")),
    assertion(term_string(1234567890123456789012345678901234567890r3,
                          "1234567890_1234567890_1234567890_1234567890r3")),
    assertion(term_string(1r1234567890123456789012345678901234567890,
                          "1r1234567890_1234567890_1234567890_1234567890")),
    assertion(term_string(1, "1234567890_1234567890_1234567890_\c
                              1234567890r1234567890_1234567890_\c
                              1234567890_1234567890")).

test(io) :-
    assertion(read_term_from_atom('1r3',1r3,[])),
    assertion(read_term_from_atom('-1r3',-1r3,[])),
    assertion(with_output_to(string("1r3"),write(1r3))),
    assertion(with_output_to(string("-1r3"),write(-1r3))).

test(syntax_fail) :-
    assertion(bad_syntax("1rr2")),
    assertion(bad_syntax("1_r2")),
    assertion(bad_syntax("1r_2")),
    assertion(bad_syntax("1r_-2")),
    assertion(bad_syntax("1.0r2")).
%   assertion(bad_syntax("1r2.0")).            % dict syntax

:- end_tests(rational).

:- begin_tests(rationalize,
               [ condition(current_prolog_flag(bounded,false))
               ]).

test(trip) :-
	R is rationalize(5.1),
	assertion(rational(R, 51, 10)).

test(roundtrip_rational) :-
    forall(between(1,1000,_),
           ( rfloat(F),
             assertion(F =:= rational(F)))).
test(roundtrip_rationalize) :-
    forall(between(1,1000,_),
           ( rfloat(F),
             assertion(F =:= rationalize(F)))).

:- end_tests(rationalize).

bad_syntax(String) :-
    catch(term_string(R,String), error(syntax_error(_T), _C), true),
    var(R).		% test that exception happened

check_error(N1,N2) :- abs(N1-N2) < 1e-12.

rfloat(X) :-
    random_between(-308,308,E),
    X is random_float * 10**E.

div0err(Exp) :-
    catch(_X is Exp,Err,true),
    nonvar(Err),
    Err = error(evaluation_error(zero_divisor), _).

get_set_flag(Flag,Old,New) :-
    current_prolog_flag(Flag,Old),
    set_prolog_flag(Flag,New).

set_float_flags([], []).
set_float_flags([flag(Name,Old)|Olds], [flag(Name,New)|News]) :-
    get_set_flag(Name,Old,New),
    set_float_flags(Olds,News).
