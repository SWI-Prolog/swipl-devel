/*  Part of SWI-Prolog

    Author:        Rick Workman & Jan Wielemaker
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020-2023, University of Amsterdam
                              VU University Amsterdam
		              CWI, Amsterdam
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



:- module(test_real, [test_real/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test Prolog real arithmetic functions

*/

test_real :-
    run_tests([ cmpr,
                maxminr
	      ]).

set_prefer_rationals(Old, New) :-
    current_prolog_flag(prefer_rationals, Old),
    set_prolog_flag(prefer_rationals, New).

set_float_undefined(Old, New) :-
    current_prolog_flag(float_undefined, Old),
    set_prolog_flag(float_undefined, New).



:- begin_tests(cmpr,
	       [ condition(current_prolog_flag(bounded, false)),
                 setup(set_float_flags(Old,
                                       [ flag(prefer_rationals,true),
                                         flag(float_undefined,nan)
                                       ])),
                 cleanup(set_float_flags(_, Old))
	       ]).
:- set_prolog_flag(rational_syntax, compatibility).

/* Tests execution paths and return values rather than broad data coverage. */
test(compare_modes) :-
	assertion(-1 =:= cmpr(2,3)),            % int,int
	assertion( 0 =:= cmpr(2,2)),
	assertion( 1 =:= cmpr(-2,-3)),
	assertion(-1 =:= cmpr(2.0,inf)),        % float,float
	assertion( 0 =:= cmpr(-1.0,-1.0)),
	assertion( 1 =:= cmpr(-2.0,-3.0)),
	assertion(-1 =:= cmpr(2,inf)),          % integer,float
	assertion( 0 =:= cmpr(-1.0,-1)),
	assertion( 1 =:= cmpr(-3,-pi)),

	assertion(-1 =:= cmpr(2^65,2^65+1)),    % mpz, mpz
	assertion( 0 =:= cmpr(-2^65,-2^65)),
	assertion( 1 =:= cmpr(2^65,2^65-1)),
	assertion(-1 =:= cmpr(1r3,1r2)),        % mpq, mpq
	assertion( 0 =:= cmpr(11r10,11r10)),
	assertion( 1 =:= cmpr(-1r3,-1r2)),

	assertion(-1 =:= cmpr(2^60,2^64)),      % int, mpz
	%assertion( 0 =:= cmpr(?,?)),           %  can't happen?
	assertion( 1 =:= cmpr(0,-2^65)),
	assertion(-1 =:= cmpr(1,11r10)),        % int, mpq
	%assertion( 0 =:= cmpr(0.5,1r2)),       %  can't happen?
	assertion( 1 =:= cmpr(0,-11r10)),

	assertion(-1 =:= cmpr(2^1024,inf)),     % float, mpz
	assertion( 0 =:= cmpr(-2^65,-2^65)),
	assertion( 1 =:= cmpr(1e18,10^18-1)),
	assertion(-1 =:= cmpr(11r10,1.1)),      % float, mpq
	assertion( 0 =:= cmpr(0.5,1r2)),
	assertion( 1 =:= cmpr(22r7,pi)),

	assertion(-1 =:= cmpr(2^65/3,2^65+1)),  % mpz, mpq
	%assertion( 0 =:= cmpr(-2^65,-2^65)),   %  can't happen?
	assertion( 1 =:= cmpr(2^65,2^65/3)),

	X1 is cmpr(nan,-2), assertion(float_class(X1,nan)),     % nan combos - non error case
	X2 is cmpr(2.0,nan),assertion(float_class(X2,nan)),
	X3 is cmpr(nan,nan),assertion(float_class(X3,nan)).

test(compare_corners) :-
	assertion(-1 =:= cmpr(1e16,10^16+1)),
	assertion( 0 =:= cmpr(1e16,10^16)),
	assertion( 1 =:= cmpr(1e16,10^16-1)),
	assertion( 0 =\= cmpr(1.1,11r10)),
	assertion(-1 =:= cmpr(2^1024,inf)),
	assertion(-1 =:= cmpr(-inf,-2^1024)).

test(realerrors) :-
    assertion(fp_error(X is cmpr(nan,1),float_undefined)),
    assertion(fp_error(X is cmpr(1r2,nan),float_undefined)),
    assertion(fp_error(X is cmpr(nan,nan),float_undefined)).

test(neg) :-
    tcmpr(1),
    tcmpr(0),
    tcmpr(-1),
    tcmpr(1<<63),
    tcmpr(-(1<<63)).

tcmpr(Expr) :-
    I is Expr,
    PInf is float(1<<1000),
    NInf is -PInf,
    F is float(I),
    N is nexttoward(F, NInf),
    P is nexttoward(F, PInf),
    assertion(-1 =:= cmpr(I, P)),
    assertion(0 =:= cmpr(I, F)),
    assertion(1 =:= cmpr(I, N)).

:- end_tests(cmpr).

/* Assumes comparereal tested basic compare functionality */
:- begin_tests(maxminr,
               [ condition(current_prolog_flag(bounded,false))
               ]).

test(realmaxmin) :-
	assertion(1.0Inf is maxr(1,inf)),
	assertion(1 is maxr(1,-inf)),
	assertion(1 is maxr(1,1.0)),
	assertion(1 is maxr(1,nan)),
	assertion(1 is maxr(nan,1)),
	assertion(2.0 is maxr(1,2.0)),
	assertion(0 is maxr(0,-0.0)),
	assertion(0 is maxr(0,0.0)),
	assertion(0.0 is maxr(-0.0,0.0)),

	assertion(-1.0Inf is minr(-inf,1)),
	assertion(1 is minr(1,inf)),
	assertion(1 is minr(1,1.0)),
	assertion(1 is minr(1,nan)),
	assertion(1 is minr(nan,1)),
	assertion(2.0 is minr(3,2.0)),
	assertion(0 is minr(0,-0.0)),
	assertion(0 is minr(0,0.0)),
	assertion(-0.0 is minr(-0.0,0.0)),

	X1 is maxr(nan,nan), assertion(float_class(X1,nan)),
	X2 is minr(nan,nan), assertion(float_class(X2,nan)).


:- end_tests(maxminr).


fp_error(Exp,FP_flag) :-
    get_set_flag(FP_flag,Save,error),
    catch(call(Exp),Err,true),
    get_set_flag(FP_flag,_,Save),
    Err = error(evaluation_error(FP_exception),_),
    fp_exception(FP_flag,FP_exception).

fp_exception(float_overflow,FP_exception)  :- FP_exception==float_overflow.
fp_exception(float_zero_div,FP_exception)  :- FP_exception==zero_divisor.
fp_exception(float_undefined,FP_exception) :- FP_exception==undefined.

get_set_flag(Flag,Old,New) :-
    current_prolog_flag(Flag,Old),
    set_prolog_flag(Flag,New).

set_float_flags([], []).
set_float_flags([flag(Name,Old)|Olds], [flag(Name,New)|News]) :-
    get_set_flag(Name,Old,New),
    set_float_flags(Olds,News).
