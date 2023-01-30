/*  Part of SWI-Prolog

    Author:        Rick Workman
    WWW:           http://www.swi-prolog.org
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

:- module(test_ieee754, [test_ieee754/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_ieee754 :-
    run_tests([ ieee754
	      ]).

/** <module> Test IEEE754 float handling

Test IEEE754 arithmetic, notably rounding.

__Note__: the Emscripten (WASM) version fails most tests. Rounding modes
are not implemented and several operations   are very imprecise. For now
we disabled this test on WASM.

@tbd This test is only executed if GMP is   used as it contains a lot of
tests that involve rational numbers. Most   tests however do not require
rationals. This file should be split into  two and the non-rational part
should move back to the `core` tests.
*/

:- begin_tests(ieee754,
	       [ condition(( current_prolog_flag(bounded, false),
                             \+ current_prolog_flag(emscripten, true))),
                 setup(set_float_flags(Old,
                                       [ flag(float_overflow,infinity),
                                         flag(float_zero_div,infinity),
                                         flag(float_undefined,nan),
                                         flag(float_underflow,ignore),
                                         flag(float_rounding,to_nearest)
                                       ])),
                 cleanup(set_float_flags(_, Old))
	       ]).

test(ieee_flags) :-
    assertion((
        get_set_flag(float_overflow,Old_o,error),
        get_set_flag(float_overflow,error,infinity),
        get_set_flag(float_overflow,infinity,Old_o))),
    assertion((
        get_set_flag(float_zero_div,Old_z,error),
        get_set_flag(float_zero_div,error,infinity),
        get_set_flag(float_zero_div,infinity,Old_z))),
    assertion((
        get_set_flag(float_undefined,Old_n,error),
        get_set_flag(float_undefined,error,nan),
        get_set_flag(float_undefined,nan,Old_n))),
    assertion((
        get_set_flag(float_underflow,Old_u,error),
        get_set_flag(float_underflow,error,ignore),
        get_set_flag(float_underflow,ignore,Old_u))),
    assertion((
        get_set_flag(float_rounding,Old_r,to_nearest),
        get_set_flag(float_rounding,to_nearest,to_positive),
        get_set_flag(float_rounding,to_positive,to_negative),
        get_set_flag(float_rounding,to_negative,to_zero),
        get_set_flag(float_rounding,to_zero,Old_r))).

test(ieee_excp) :-
    assertion(fp_error(X is 2+inf,float_overflow)),
    assertion(fp_error(X is 2/0,float_zero_div)),
    assertion(fp_error(X is 2-nan,float_undefined)).


%% Basic Arithmetic Tests (from Eclipse paper) %%%%%%%%%%%%

test(ieee_cmp):-
    assertion( 0.0 =:= -0.0),
    assertion( 0.0 \== -0.0),
    assertion( inf  >  -inf),
    assertion( nan ==   nan),
    assertion( nan =\=  nan),
    assertion(\+(nan  <  nan)),
    assertion(\+(nan =<  nan)),
    assertion(\+(nan >=  nan)),
    assertion(\+(nan  >  nan)),
    assertion(\+(nan =:= nan)),
    assertion(1.5NaN is nan),
    assertion( nan =\=  0.0),
    assertion(\+(nan =:= 0.0)),
    assertion(\+(nan  <  0.0)),
    assertion(\+(nan =<  0.0)),
    assertion(\+(nan >=  0.0)),
    assertion(\+(nan  >  0.0)),
    assertion(-inf  <   -0.0),
    assertion( 0.0  <   inf),
    assertion(sqrt(-3) =\= sqrt(-2)).

test(ieee_tcmp ) :-  % placement in standard term order
    assertion(( 1.5NaN @< -1.0Inf,
               -1.0Inf @< -0.0,
               -0.0    @<  0.0,
                0.0    @<  1.0Inf)),
    assertion(( 1.0Inf @>  0.0,
                0.0    @> -0.0,
               -0.0    @> -1.0Inf,
               -1.0Inf @>  1.5NaN)),
    assertion(( 1.5NaN @< -1.0Inf,
               -1.0Inf @<  0,
                0      @<  1.0Inf)),
    assertion(( 1.0Inf @>  0,
                0      @> -1.0Inf,
               -1.0Inf @>  1.5NaN)),
    assertion((compare(O,1.5NaN,0.0),compare(O,1.5NaN,0))),
    assertion(sort([1.0Inf,    0.0, -0.0, -1.0Inf, 1.5NaN],
                   [1.5NaN,-1.0Inf, -0.0,    0.0,  1.0Inf])).

test(ieee_minus) :-
    assertion(    0.0 is - -0.0),
    assertion(   -0.0 is -  0.0),
    assertion( 1.0Inf is - -inf),
    assertion(-1.0Inf is -  inf),
    assertion( 1.5NaN is -  nan),
    assertion(fp_error(X is -(nan+1),float_undefined)),
    assertion(fp_error(X is -(inf+1),float_overflow)).

test(ieee_add) :-
    assertion(    0.0 =:= -0.0+  0.0),
    assertion(    0.0 =:=  0.0+ -0.0),
    assertion(   -0.0 =:= -0.0+ -0.0),
    assertion( 1.0Inf =:= -0.0+  inf),
    assertion( 1.5NaN is   inf+ -inf),
    assertion( 1.5NaN is  -inf+  inf),
    assertion(-1.0Inf is  -inf+  0.0),
    assertion( 1.5NaN is   nan+  inf),
    assertion(fp_error(X is 2+nan,float_undefined)),
    assertion(fp_error(X is inf+inf,float_overflow)).

test(ieee_sub) :-
    assertion(    0.0 =:=  0.0-  0.0),
    assertion(   -0.0 =:= -0.0-  0.0),
    assertion(    0.0 =:=  0.0- -0.0),
    assertion(    0.0 =:= -0.0- -0.0),
    assertion( 1.0Inf =:=  inf- -0.0),
    assertion( 1.5NaN is  -inf- -inf),
    assertion( 1.5NaN is   inf-  inf),
    assertion( 1.0Inf is   inf- -inf),
    assertion(-1.0Inf is  -inf-  inf),
    assertion(-1.0Inf is   1.0-  inf),
    assertion(-1.0Inf is  -inf-  1.0),
    assertion( 1.0Inf is   1.0- -inf),
    assertion( 1.5NaN is   inf-  nan),
    assertion(fp_error(X is inf-inf,float_undefined)),
    assertion(fp_error(X is inf-1,float_overflow)).

test(ieee_mul) :-
    assertion(   -0.0 =:= -0.0*  1.0),
    assertion(    0.0 =:= -0.0* -1.0),
    assertion( 1.0Inf is   inf*  inf),
    assertion(-1.0Inf is   inf* -inf),
    assertion( 1.0Inf is  -inf* -inf),
    assertion( 1.0Inf is   inf*  1.0),
    assertion(-1.0Inf is   1.0* -inf),
    assertion( 1.5NaN is   0.0* inf),
    assertion( 1.5NaN is   nan* inf),
    assertion(fp_error(X is 0*inf,float_undefined)),
    assertion(fp_error(X is inf*inf,float_overflow)).

test(ieee_div) :- %%, blocked(fail)) :-
    assertion( 1.5NaN is -0.0/  0.0),
    assertion( 1.5NaN is -0.0/ -0.0),
    assertion( 1.5NaN is  0.0/  0.0),
    assertion( 1.5NaN is  0.0/ -0.0),
    assertion(   -0.0 is -0.0/  1.0),
    assertion(    0.0 is -0.0/ -1.0),
    assertion(-1.0Inf is  1.0/ -0.0),
    assertion( 1.0Inf is -1.0/ -0.0),
    assertion( 1.5NaN is -inf/ -inf),
    assertion( 1.5NaN is  inf/  inf),
    assertion( 1.5NaN is  inf/ -inf),
    assertion( 1.5NaN is -inf/  inf),
    assertion( 1.0Inf is  inf/  1.0),
    assertion(    0.0 is  1.0/  inf),
    assertion(-1.0Inf is -inf/  1.0),
    assertion(   -0.0 is  1.0/ -inf),
    assertion(-1.0Inf is  inf/ -1.0),
    assertion(   -0.0 is -1.0/  inf),
    assertion( 1.0Inf is -inf/ -1.0),
    assertion(    0.0 is -1.0/ -inf),
    assertion( 1.5NaN is -1.0/ nan),
    assertion( 1.5NaN is  nan/ 1.0),
    assertion( 1.5NaN is  nan/ 0),
    assertion( 1.0Inf is  inf/ 0),
    assertion(-1.0Inf is -inf/ 0),
    assertion( 1.0Inf is    1/ 0),
    assertion(-1.0Inf is -1r2/ 0),
    assertion(fp_error(X is 1/0,float_zero_div)),
    assertion(fp_error(X is 1/0.0,float_zero_div)),
    assertion(fp_error(X is -inf/ -inf,float_undefined)),
    assertion(fp_error(X is -inf/1.0,float_overflow)).

test(ieee_sign) :-
    assertion( 0.0    is sign(-0.0)),
    assertion( 0.0    is sign( 0.0)),
    assertion( 1.0    is sign( inf)),
    assertion(-1.0    is sign(-inf)),
    assertion( 1.5NaN is sign( nan)).

test(ieee_parts) :-
    assertion(float_parts(-0.5,   -0.0,   -0.5)),
    assertion(float_parts( 0.5,    0.0,    0.5)),
    assertion(float_parts(-0.0,   -0.0,   -0.0)),
    assertion(float_parts( 0.0,    0.0,    0.0)),
    assertion(float_parts( inf, 1.0Inf,    0.0)),
    assertion(float_parts(-inf,-1.0Inf,   -0.0)),
    assertion(float_parts( nan, 1.5NaN, 1.5NaN)).

%% End of Basic Arithmetic Tests %%%%%%%%%%%%%%%%%%


%% Annex F Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(ieee_acos) :-					% C11 - F.10.1.1
    assertion(0.0 is acos(1)),
    assertion(1.5NaN is acos(2)),
    assertion(1.5NaN is acos(-2)),
    assertion(1.5NaN is acos(inf)),
    assertion(1.5NaN is acos(-inf)),
    assertion(1.5NaN is acos(nan)).

test(ieee_asin) :-                                      % C11 - F.10.1.2
    assertion(0.0 is asin(0.0)),
    assertion(-0.0 is asin(-0.0)),
    assertion(1.5NaN is asin(2)),
    assertion(1.5NaN is asin(-2)),
    assertion(1.5NaN is asin(inf)),
    assertion(1.5NaN is asin(-inf)),
    assertion(1.5NaN is asin(nan)).

test(ieee_atan) :-                                      % C11 - F.10.1.3
    assertion( 0.0  is  atan(0.0)),
    assertion(-0.0  is  atan(-0.0)),
    assertion( pi/2 =:= atan(inf)),
    assertion(-pi/2 =:= atan(-inf)),
    assertion(1.5NaN is atan(nan)).

test(ieee_atan2) :-                                     % C11 - F.10.1.4
    assertion( pi =:= atan2( 0.0,-0.0)),
    assertion(-pi =:= atan2(-0.0,-0.0)),
    assertion( 0.0 is atan2( 0.0, 0.0)),
    assertion(-0.0 is atan2(-0.0, 0.0)),
    assertion( pi =:= atan2( 0.0,-1.0)),
    assertion(-pi =:= atan2(-0.0,-1.0)),
    assertion( 0.0 is atan2( 0.0, 1.0)),
    assertion(-0.0 is atan2(-0.0, 1.0)),
    assertion(-pi/2 =:= atan2(-1.0, 0.0)),
    assertion(-pi/2 =:= atan2(-1.0,-0.0)),
    assertion( pi/2 =:= atan2( 1.0, 0.0)),
    assertion( pi/2 =:= atan2( 1.0,-0.0)),
    assertion( pi   =:= atan2( 1.0,-inf)),
    assertion(-pi   =:= atan2(-1.0,-inf)),
    assertion( 0.0  is  atan2( 1.0, inf)),
    assertion(-0.0  is  atan2(-1.0, inf)),
    assertion( pi/2 =:= atan2( inf, 1.0)),
    assertion(-pi/2 =:= atan2(-inf,-1.0)),
    assertion( 3*pi/4 =:= atan2( inf,-inf)),
    assertion(-3*pi/4 =:= atan2(-inf,-inf)),
    assertion( pi/4 =:= atan2( inf, inf)),
    assertion(-pi/4 =:= atan2(-inf, inf)).

test(ieee_cos) :-					% C11 - F.10.1.5
    assertion(   1.0 is cos( 0.0)),
    assertion(   1.0 is cos(-0.0)),
    assertion( 1.5NaN is  cos( inf)),
    assertion( 1.5NaN is  cos(-inf)),
    assertion( 1.5NaN is  cos( nan)).

test(ieee_sin) :-                                       % C11 - F.10.1.6
    assertion(   0.0 is sin( 0.0)),
    assertion(  -0.0 is sin(-0.0)),
    assertion( 1.5NaN is sin( inf)),
    assertion( 1.5NaN is sin(-inf)),
    assertion( 1.5NaN is sin( nan)).

test(ieee_tan) :-                                       % C11 - F.10.1.7
    assertion(   0.0 is tan( 0.0)),
    assertion(  -0.0 is tan(-0.0)),
    assertion( 1.5NaN is tan( inf)),
    assertion( 1.5NaN is tan(-inf)),
    assertion( 1.5NaN is tan( nan)).

test(ieee_acosh) :-                                     % C11 - F.10.2.1
    assertion(   0.0 is acosh( 1.0)),
    assertion(1.0Inf is acosh( inf)),
    assertion(1.5NaN is acosh( 0.5)),
    assertion(1.5NaN is acosh( nan)).

test(ieee_asinh) :-                                     % C11 - F.10.2.2
    assertion(    0.0 is asinh( 0.0)),
    assertion(   -0.0 is asinh(-0.0)),
    assertion( 1.0Inf is asinh( inf)),
    assertion(-1.0Inf is asinh(-inf)),
    assertion( 1.5NaN is asinh( nan)).

test(ieee_atanh) :-                                     % C11 - F.10.2.3
    assertion(    0.0 is atanh( 0.0)),
    assertion(   -0.0 is atanh(-0.0)),
    assertion( 1.0Inf is atanh( 1.0)),
    assertion(-1.0Inf is atanh(-1.0)),
    assertion( 1.5NaN is atanh( 2.0)),
    assertion( 1.5NaN is atanh(-2.0)),
    assertion( 1.5NaN is atanh( nan)).

test(ieee_cosh) :-                                      % C11 - F.10.2.4
    assertion(    1.0 is cosh( 0.0)),
    assertion(    1.0 is cosh(-0.0)),
    assertion( 1.0Inf is cosh( inf)),
    assertion( 1.0Inf is cosh(-inf)),
    assertion( 1.5NaN is cosh( nan)).

test(ieee_sinh) :-                                      % C11 - F.10.2.5
    assertion(    0.0 is sinh( 0.0)),
    assertion(   -0.0 is sinh(-0.0)),
    assertion( 1.0Inf is sinh( inf)),
    assertion(-1.0Inf is sinh(-inf)),
    assertion( 1.5NaN is sinh( nan)).

test(ieee_tanh) :-                                      % C11 - F.10.2.6
    assertion(    0.0 is tanh( 0.0)),
    assertion(   -0.0 is tanh(-0.0)),
    assertion(    1.0 is tanh( inf)),
    assertion(   -1.0 is tanh(-inf)),
    assertion( 1.5NaN is tanh( nan)).

test(ieee_exp) :-                                       % C11 - F.10.3.1
    assertion(    1.0 is exp( 0.0)),
    assertion(    1.0 is exp(-0.0)),
    assertion( 1.0Inf is exp( inf)),
    assertion(    0.0 is exp(-inf)),
    assertion( 1.5NaN is exp( nan)).

test(ieee_log) :-                                       % C11 - F.10.3.7
    assertion(-1.0Inf is log( 0.0)),
    assertion(-1.0Inf is log(-0.0)),
    assertion( 1.5NaN is log(-1.0)),
    assertion( 1.0Inf is log( inf)),
    assertion( 1.5NaN is log( nan)).

test(ieee_log10) :-                                     % C11 - F.10.3.8
    assertion(-1.0Inf is log10( 0.0)),
    assertion(-1.0Inf is log10(-0.0)),
    assertion( 1.5NaN is log10(-1.0)),
    assertion( 1.0Inf is log10( inf)),
    assertion( 1.5NaN is log10( nan)).


test(ieee_abs) :-                                       % C11 - F.10.4.2
    assertion(    0.0 is abs( 0.0)),
    assertion(    0.0 is abs(-0.0)),
    assertion( 1.0Inf is abs( inf)),
    assertion( 1.0Inf is abs(-inf)),
    assertion( 1.5NaN is abs( nan)).

test(ieee_pow) :-                                       % C11 - F.10.4.4
    assertion( 1.0Inf is  0.0** -1),
    assertion(-1.0Inf is -0.0** -1),
    assertion( 1.0Inf is  0.0** -2),
    assertion( 1.0Inf is -0.0** -2),
    assertion( 1.0Inf is  0.0** (-inf)),
    assertion( 1.0Inf is -0.0** (-inf)),
    assertion(    0.0 is  0.0**  1),
    assertion(   -0.0 is -0.0**  1),
    assertion(    0.0 is  0.0**  2),
    assertion(    0.0 is -0.0**  2),

    assertion(    1.0 is  1.0**  inf),
    assertion(    1.0 is  1.0** (-inf)),
    assertion(    1.0 is  1.0**  nan),
    assertion( 1.5NaN is  0**  nan),
    assertion(      1 is  inf**  0.0),
    assertion(      1 is  (-inf)**  0.0),
    assertion(      1 is  nan**  0.0),
    assertion(      1 is  inf** -0.0),
    assertion(      1 is  (-inf)** -0.0),
    assertion(      1 is  nan** -0.0),
    assertion( 1.5NaN is -1.0**  1.5),

    assertion( 1.0Inf is -0.5** (-inf)),
    assertion(    0.0 is  1.5** (-inf)),
    assertion(    0.0 is -0.5**  inf),
    assertion( 1.0Inf is  1.5**  inf),

    assertion(   -0.0 is (-inf)** -1),
    assertion(    0.0 is (-inf)** -2),
    assertion(-1.0Inf is (-inf)**  3),
    assertion( 1.0Inf is (-inf)**  2),
    assertion(    0.0 is  inf** -1),
    assertion( 1.0Inf is  inf**  2).

% (see opengroup.org sqrt)
test(ieee_sqrt) :-                                      % C11 - F.10.4.4
    assertion(    0.0 is sqrt( 0.0)),
    assertion(   -0.0 is sqrt(-0.0)),
    assertion( 1.0Inf is sqrt( inf)),
    assertion( 1.5NaN is sqrt(-inf)),
    assertion( 1.5NaN is sqrt(-1.0)),
    assertion( 1.5NaN is sqrt( nan)).

test(ieee_erf) :-                                       % C11 - F.10.5.1
    assertion(    0.0 is erf( 0.0)),
    assertion(   -0.0 is erf(-0.0)),
    assertion(    1.0 is erf( inf)),
    assertion(   -1.0 is erf(-inf)),
    assertion( 1.5NaN is erf( nan)).

test(ieee_erfc) :-                                      % C11 - F.10.5.2
    assertion(    0.0 is erfc( inf)),
    assertion(    2.0 is erfc(-inf)),
    assertion( 1.5NaN is erfc( nan)).

test(ieee_lgamma) :-                                    % C11 - F.10.5.3
    assertion(    0.0 is lgamma( 1.0)),
    assertion(    0.0 is lgamma( 2.0)),
    assertion( 1.0Inf is lgamma( 0.0)),
    assertion( 1.0Inf is lgamma(-0.0)),
    assertion( 1.0Inf is lgamma(-1)),
    assertion( 1.0Inf is lgamma( inf)),
    assertion( 1.0Inf is lgamma(-inf)),
    assertion( 1.5NaN is lgamma( nan)).

test(ieee_ceil) :-                                      % C11 - F.10.6.1
    assertion(      0 is ceiling( 0.0)),
    assertion(      0 is ceiling(-0.0)),
    assertion( 1.0Inf is ceiling( inf)),
    assertion(-1.0Inf is ceiling(-inf)),
    assertion( 1.5NaN is ceiling( nan)).

test(ieee_floor) :-                                     % C11 - F.10.6.2
    assertion(      0 is floor( 0.0)),
    assertion(      0 is floor(-0.0)),
    assertion( 1.0Inf is floor( inf)),
    assertion(-1.0Inf is floor(-inf)),
    assertion( 1.5NaN is floor( nan)).

test(ieee_round) :-                                     % C11 - F.10.6.6
    assertion(      0 is round( 0.0)),
    assertion(      0 is round(-0.0)),
    assertion( 1.0Inf is round( inf)),
    assertion(-1.0Inf is round(-inf)),
    assertion( 1.5NaN is round( nan)).

test(ieee_trunc) :-                                     % C11 - F.10.6.8
    assertion(      0 is truncate( 0.0)),
    assertion(      0 is truncate(-0.0)),
    assertion( 1.0Inf is truncate( inf)),
    assertion(-1.0Inf is truncate(-inf)),
    assertion( 1.5NaN is truncate( nan)).


test(ieee_copysign) :-                                  % C11 - F.10.8.1
    assertion(-1.0Inf is copysign( inf, -0.0)),
    assertion( 1.0Inf is copysign(-inf,  0.0)),
    assertion(    0.0 is copysign(-0.0,  inf)),
    assertion(   -0.0 is copysign( 0.0, -inf)),
    assertion(    0.0 is copysign( 0.0,  nan)),
    assertion( 1.5NaN is copysign( nan, -0.0)).

test(ieee_nexttoward) :-                                % C11 - F.10.8.4
    assertion(-1.0Inf is nexttoward(-inf, -inf)),
    assertion( 1.0Inf is nexttoward( inf,  inf)),
    assertion(-1.0Inf <  nexttoward(-inf,  inf)),
    assertion( 1.0Inf >  nexttoward( inf, -inf)),
    assertion(    0.0 is nexttoward(-0.0,  0.0)),
    assertion(   -0.0 is nexttoward( 0.0, -0.0)),
    assertion(    0.0 <  nexttoward(-0.0,  inf)),
    assertion(   -0.0 >  nexttoward( 0.0, -inf)),
    assertion( 1.5NaN is nexttoward( 0.0,  nan)),
    assertion( 1.5NaN is nexttoward( nan, -0.0)).

test(ieee_max) :-                                       % C11 - F.10.9.2
    assertion(-1.0Inf is max(-inf, -inf)),
    assertion( 1.0Inf is max( inf,  inf)),
    assertion( 1.0Inf is max( inf, -inf)),
    assertion(    0.0 is max(-0.0,  0.0)),
    assertion(    0.0 is max( 0.0, -0.0)),
    assertion(      0 is max(-0.0,  0)),
    assertion(      0 is max(   0, -0.0)),
    assertion( 1.5NaN is max( nan, -inf)),
    assertion( 1.5NaN is max( nan,  nan)).

test(ieee_min) :-                                       % C11 - F.10.9.3
    assertion(-1.0Inf is min(-inf, -inf)),
    assertion( 1.0Inf is min( inf,  inf)),
    assertion(-1.0Inf is min( inf, -inf)),
    assertion(   -0.0 is min(-0.0,  0.0)),
    assertion(   -0.0 is min( 0.0, -0.0)),
    assertion(   -0.0 is min(-0.0,    0)),
    assertion(   -0.0 is min(   0, -0.0)),
    assertion( 1.5NaN is min( inf,  nan)),
    assertion( 1.5NaN is min( nan,  nan)).

%% End of Annex F Tests %%%%%%%%%%%%%%%%%%%%%%%%%

test(ieee_rmode) :-
    assertion(test_rounding(-1.0/(3))),
    assertion(test_rounding(float(1r3))),
    assertion(test_rounding(float(-1r3))),
    assertion(test_rounding(sqrt(2))),
    assertion(test_rounding(exp(log(2)))),
    assertion(test_rounding(-2.0** 1r3)),
    assertion(test_rounding( 2.0** 1r3)),
    assertion(test_rounding(pi)),
    assertion(test_rounding(e)).

% rational exponent power tests (3)
test(float_to_rat) :-
    assertion(test_value(8.0 is 4.0**3r2)),
    assertion(1.5NaN is -4.0**3r2),  % neg. base, even Q
    assertion(test_value(2.0 is 8.0**1r3)),
    assertion(test_value(-2.0 is -8.0**1r3)),
    assertion(test_value(4.0 is 8.0**2r3)),
    assertion(test_value(4.0 is -8.0**2r3)),
    assertion(test_value(0.5 is 8.0** -1r3)),
    assertion(test_value(-0.5 is -8.0** -1r3)),
    assertion(2**1r2 =:= sqrt(2)).

test(ieee_rndto) :-
    assertion(test_roundto(-1.0/(3))),
    assertion(test_roundto(float(1r3))),
    assertion(test_roundto(float(-1r3))),
    assertion(test_roundto(sqrt(2))),
    assertion(test_roundto(exp(log(2)))),
    assertion(test_roundto(2**0.5)),
    assertion(test_roundto(-2.0002** 2)),
    assertion(test_roundto( 2.0002** 2)),
    assertion(test_roundto( 2.0002** 1r2)),
    assertion(test_roundto(-2.0002** 1r3)),
    assertion(test_roundto( 2.0002** 1r3)),
    assertion(test_roundto(-2.0002** 2r3)),
    assertion(test_roundto( 2.0002** 2r3)),
    assertion(test_roundto( 0.0002 ** 1r3)),
    assertion(test_roundto(-0.0002 ** 1r3)),
    assertion(test_roundto( 0.0002 ** 2r3)),
    assertion(test_roundto(-0.0002 ** 2r3)),
    assertion(test_roundto( 0.0002 ** -1r3)),
    assertion(test_roundto(-2.0002 ** -1r3)),
    assertion(test_roundto( 0.0002 ** -2r3)),
    assertion(test_roundto(-2.0002 ** -2r3)),
    assertion(test_roundto(pi)),
    assertion(test_roundto(e)),
    assertion(test_roundto(sin(pi/2))),
    assertion(test_roundto(atan2(1,-1))),
    assertion(test_roundto(cosh(-0.5))),
      current_prolog_flag(max_tagged_integer,MTI),
    assertion(test_roundto(float(MTI))),
    assertion(test_roundto(float(-MTI))),
      ExtInt is 2**62-1,
    assertion(test_roundto(float(ExtInt))),
    assertion(test_roundto(float(-ExtInt))),
      MPZInt is 2**65,
    assertion(rounding(float( MPZInt),r(R,R,R,R))),  % exact result
    assertion(rounding(float(-MPZInt),r(R,R,R,R))),  % exact result
    assertion(test_roundto(float(MPZInt+1))),
    assertion(test_roundto(float(-(MPZInt+1)))),
    assertion(test_roundto(float(MPZInt-1))),
    assertion(test_roundto(float(-(MPZInt-1)))),
      Overflow is 2^1024, current_prolog_flag(float_max,FMax), NMax is -FMax,  % all results exact
    assertion(rounding(float( Overflow),r( 1.0Inf, 1.0Inf, 1.0Inf, 1.0Inf))),
    assertion(rounding(float( Overflow+1),r( 1.0Inf, 1.0Inf, 1.0Inf, 1.0Inf))),
    assertion(rounding(float( Overflow-1),r( 1.0Inf, 1.0Inf, FMax, FMax))),
    assertion(rounding(float(-Overflow),r(-1.0Inf,-1.0Inf,-1.0Inf,-1.0Inf))),
    assertion(rounding(float(-(Overflow+1)),r(-1.0Inf,-1.0Inf,-1.0Inf,-1.0Inf))),
    assertion(rounding(float(-(Overflow-1)),r(-1.0Inf,NMax,-1.0Inf,NMax))).

test(bounded) :-
    assertion(bounded_number(0,10,1)),
    assertion(\+(bounded_number(0,1r2,1))),
    assertion(bounded_number(0.0,10.0,1)),
    assertion(bounded_number(0,10,1.0)),
    assertion(bounded_number(0.0,10.0,1.0)),
    assertion(bounded_number(0,10,1r2)),
    assertion(bounded_number(0.0,10.0,1r2)),
    assertion(bounded_number(1r3,2r3,1r2)),
    assertion(bounded_number(-2r3,-1r3,-1r2)),
    assertion((bounded_number(L,H,-0.0),bounded_number(L,H,0.0))),
    assertion((bounded_number(L,H,-1.0),bounded_number(L,H,-1.0))),
    assertion((bounded_number(L,H,-1),[L,H]=[-2,0])),
    assertion((bounded_number(L,H,-1.0),[L,H]=[-1.0000000000000002,-0.9999999999999999])),
    assertion((bounded_number(L,H,-1r2),[L,H]=[-0.5000000000000001,-0.49999999999999994])),
    assertion((bounded_number(L,H,10000000000000000000), [L,H]=[9999999999999999999,10000000000000000001])),
    assertion(\+(bounded_number(L,H,1.0Inf))),
    assertion(\+(bounded_number(L,H,-1.0Inf))),
    assertion(\+(bounded_number(L,H,1.5NaN))).


:- end_tests(ieee754).

%% Auxiliary predicates %%%%%%%%%%%%%%%%%%%%%%%%%

get_set_flag(Flag,Old,New) :-
    current_prolog_flag(Flag,Old),
    set_prolog_flag(Flag,New).

set_float_flags([], []).
set_float_flags([flag(Name,Old)|Olds], [flag(Name,New)|News]) :-
    get_set_flag(Name,Old,New),
    set_float_flags(Olds,News).

float_parts(F,Ip,Fp) :-
    Ip is float_integer_part(F),
    Fp is float_fractional_part(F).

test_value(R is Exp) :-
	Rp is roundtoward(Exp,to_positive),
    Rn is roundtoward(Exp,to_negative),
    Rn =< R,R =< Rp.

test_rounding(Exp) :-
    rounding(Exp, Rounded),
    check_round(Exp, Rounded).

rounding(Exp,r(Rc,Rp,Rn,Rz)) :-                 % for non-precise Exp
    current_prolog_flag(float_rounding, Save),
    set_prolog_flag(float_rounding,to_nearest),  Rc is Exp,
    set_prolog_flag(float_rounding,to_positive), Rp is Exp,
    set_prolog_flag(float_rounding,to_negative), Rn is Exp,
    set_prolog_flag(float_rounding,to_zero),     Rz is Exp,
    set_prolog_flag(float_rounding, Save).

test_roundto(Exp) :-
    roundto(Exp, Rounded),
    check_round(Exp, Rounded).

roundto(Exp,r(Rc,Rp,Rn,Rz)) :-                  % for non-precise Exp
    Rc is roundtoward(Exp,to_nearest),
    Rp is roundtoward(Exp,to_positive),
    Rn is roundtoward(Exp,to_negative),
    Rz is roundtoward(Exp,to_zero).

%!  check_round(+Exp, +Round)
%
%   Round is a term r(Nearest, Positive, Negative, Zero)

check_round(_Exp, r(R,R,R,R)) :-
	float(R), float_class(R,infinite),
	!.
check_round(Exp, r(Rc,Rp,Rn,Rz)) :-
    abs((Rp-Rc+Rn-Rz)/Rc) < 4e-15,
    Rn =< Rc, Rc =< Rp,
    (   Rc < 0
    ->  Rz >= Rc, expect_less_then(Exp, n=Rn, z=Rz)
    ;   Rz =< Rc, expect_less_then(Exp, z=Rz, p=Rp)
    ),
    expect_less_then(Exp, n=Rn, p=Rp).

expect_less_then(Exp, A=X, B=Y) :-
    (   X < Y
    ->  true
    ;   X == Y
    ->  print_message(warning, ieee754_bounds(Exp, A-X, B-Y))
    ).

fp_error(Exp,FP_flag) :-
    get_set_flag(FP_flag,Save,error),
    catch(call(Exp),Err,true),
    get_set_flag(FP_flag,_,Save),
    Err = error(evaluation_error(FP_exception),_),
    fp_exception(FP_flag,FP_exception).

fp_exception(float_overflow,FP_exception)  :- FP_exception==float_overflow.
fp_exception(float_zero_div,FP_exception)  :- FP_exception==zero_divisor.
fp_exception(float_undefined,FP_exception) :- FP_exception==undefined.

:- multifile prolog:message//1.

prolog:message(ieee754_bounds(Exp, A-X, B-Y)) -->
    [ 'IEEE 754 dubious rounding: ~p: ~w = ~q, ~w = ~q'-
      [Exp, A, X, B, Y]
    ].
