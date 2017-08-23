/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_arith, [test_arith/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core arithmetic functions

@author	Jan Wielemaker
*/

test_arith :-
	run_tests([ div,
		    gdiv,
		    rem,
		    mod,
		    pow,
		    gcd,
		    shift,
		    errors,
		    ar_builtin,
		    eval,
		    hyperbolic,
		    rationalize,
                    minint,
                    minint_promotion,
                    maxint,
                    maxint_promotion,
		    float_overflow,
		    float_zero,
		    float_special,
		    arith_misc
		  ]).

:- begin_tests(div).

div_ok(X, Y) :-
	Q is div(X, Y),
	M is mod(X, Y),
	(   X =:= Y*Q+M
	->  true
	;   format(user_error, 'Failed for X=~w,Y=~w~n', [X,Y])
	).

test(mod, true) :-
	forall(between(-10, 10, X),
	       forall((between(-10, 10, Y), Y =\= 0),
		      div_ok(X, Y))).

test(minint, condition(current_prolog_flag(bounded, false))) :-
	div_ok(-9223372036854775808, 4294967297).

test(minint, A == -2147483648) :-
	A is -9223372036854775808 div 4294967297.

:- end_tests(div).

:- begin_tests(gdiv).

:- if(current_prolog_flag(bounded,false)).

test(minint, X == 9223372036854775808) :-
	X is -9223372036854775808 // -1.

:- else.

test(minint, error(evaluation_error(int_overflow))) :-
	X is -9223372036854775808 // -1,
	writeln(X).

:- endif.

:- end_tests(gdiv).

:- begin_tests(rem).

test(small, R == 2) :-
	R is 5 rem 3.
test(small_divneg, R == 2) :-
	R is 5 rem -3.
test(small_neg, R == -2) :-
	R is -5 rem 3.
test(big, [condition(current_prolog_flag(bounded, false)), R == 6]) :-
	R is (1<<100) rem 10.
test(big_neg, [condition(current_prolog_flag(bounded, false)), R == -6]) :-
	R is -(1<<100) rem 10.
test(exhaust,all((N rem D =:= M) == [-3 rem-3=:=0,-3 rem-2=:= -1,-3 rem-1=:=0,-3 rem 1=:=0,-3 rem 2=:= -1,-3 rem 3=:=0,-2 rem-3=:= -2,-2 rem-2=:=0,-2 rem-1=:=0,-2 rem 1=:=0,-2 rem 2=:=0,-2 rem 3=:= -2,-1 rem-3=:= -1,-1 rem-2=:= -1,-1 rem-1=:=0,-1 rem 1=:=0,-1 rem 2=:= -1,-1 rem 3=:= -1,0 rem-3=:=0,0 rem-2=:=0,0 rem-1=:=0,0 rem 1=:=0,0 rem 2=:=0,0 rem 3=:=0,1 rem-3=:=1,1 rem-2=:=1,1 rem-1=:=0,1 rem 1=:=0,1 rem 2=:=1,1 rem 3=:=1,2 rem-3=:=2,2 rem-2=:=0,2 rem-1=:=0,2 rem 1=:=0,2 rem 2=:=0,2 rem 3=:=2,3 rem-3=:=0,3 rem-2=:=1,3 rem-1=:=0,3 rem 1=:=0,3 rem 2=:=1,3 rem 3=:=0])) :-
	maplist(between(-3,3),[N,D]),
	D =\= 0,
	M is N rem D.
test(big, [condition(current_prolog_flag(bounded, false)), R =:= -3]) :-
	R is -3 rem (10^50).
test(allq,[fail]) :-
	maplist(between(-50,50),[X,Y]),
	Y =\= 0, X =\= (X rem Y) + (X // Y) * Y.
:- end_tests(rem).

:- begin_tests(mod).

% mod(X, Y) = X - (floor(X/Y) * Y)

test(small, R == 2) :-
	R is 5 mod 3.
test(small_divneg, R == -1) :-
	R is 5 mod -3.
test(small_neg, R == 1) :-
	R is -5 mod 3.
test(big, [condition(current_prolog_flag(bounded, false)), R == 6]) :-
	R is (1<<100) mod 10.
test(big_neg, [condition(current_prolog_flag(bounded, false)), R == 4]) :-
	R is -(1<<100) mod 10.
test(exhaust,all((N mod D =:= M) == [-3 mod-3=:=0,-3 mod-2=:= -1,-3 mod-1=:=0,-3 mod 1=:=0,-3 mod 2=:=1,-3 mod 3=:=0,-2 mod-3=:= -2,-2 mod-2=:=0,-2 mod-1=:=0,-2 mod 1=:=0,-2 mod 2=:=0,-2 mod 3=:=1,-1 mod-3=:= -1,-1 mod-2=:= -1,-1 mod-1=:=0,-1 mod 1=:=0,-1 mod 2=:=1,-1 mod 3=:=2,0 mod-3=:=0,0 mod-2=:=0,0 mod-1=:=0,0 mod 1=:=0,0 mod 2=:=0,0 mod 3=:=0,1 mod-3=:= -2,1 mod-2=:= -1,1 mod-1=:=0,1 mod 1=:=0,1 mod 2=:=1,1 mod 3=:=1,2 mod-3=:= -1,2 mod-2=:=0,2 mod-1=:=0,2 mod 1=:=0,2 mod 2=:=0,2 mod 3=:=2,3 mod-3=:=0,3 mod-2=:= -1,3 mod-1=:=0,3 mod 1=:=0,3 mod 2=:=1,3 mod 3=:=0])) :-
	maplist(between(-3,3),[N,D]),
	D =\= 0,
	M is N mod D.
test(big, [condition(current_prolog_flag(bounded, false)), R =:= 10^50-3]) :-
	R is -3 mod (10^50).

:- end_tests(mod).

:- begin_tests(pow).

:- if(current_prolog_flag(bounded, false)).
test(rat, X == 32 rdiv 243) :-
	X is (2 rdiv 3)^(5).
test(rat, X == 1) :-
	X is (2 rdiv 3)^(0).
test(rat, X == 243 rdiv 32) :-
	X is (2 rdiv 3)^(-5).
:- endif.

:- end_tests(pow).

:- begin_tests(shift).

test(shift_right_large, X == 0) :-
	X is 5>>64.
test(shift_right_large, X == -1) :-
	X is -5>>64.
test(shift_right_large, X == 0) :-
	X is 5>>(1<<62).
:- if(current_prolog_flag(bounded, false)).
test(shift_right_large, X == 0) :-
	X is 5>>(1<<100).
test(shift_left_large, X == -18446744073709551616) :-
	X is (-1<<40)<<24.
:- endif.

:- end_tests(shift).

:- begin_tests(gcd).

test(gcd, X == 4) :-
	X is gcd(100, 24).
test(gcd, X == 4) :-
	X is gcd(24, 100).		% seems to be some argument ordering
:- if(current_prolog_flag(bounded,false)).
test(gcd, X == 9223372036854775808) :-
	X is gcd(-9223372036854775808, -9223372036854775808).
:-endif.
:- end_tests(gcd).

:- begin_tests(errors).

test(cyclic, [sto(rational_trees), error(type_error(expression, T))]) :-
	T = T+1,
	A is T,
	number(A).			% avoid singleton

:- end_tests(errors).


:- begin_tests(ar_builtin).

a1(A, R) :-
	B is A+1,
	B =:= R.
a2 :-
	X = 7,
	Y is 10 - X,
	Y == 3.

test(a_add_fc_int) :-
	a1(1, 2).
test(a_add_fc_float) :-
	a1(0.1, 1.1).
test(a_fc_minus) :-
	a2.

:- end_tests(ar_builtin).


:- begin_tests(eval).

test(ref, R==6) :-			% Bug#12
	term_to_atom(Expr, 'X = 2+3, Y is X+1'),
	call(Expr),
	Expr = (_,R is _).

:- end_tests(eval).

:- begin_tests(hyperbolic).

round(X, R) :- R is round(X*1000)/1000.

test(sinh, V =:= 1.175) :- X is sinh(1.0), round(X,V).
test(cosh, V =:= 1.543) :- X is cosh(1.0), round(X,V).
test(tanh, V =:= 0.762) :- X is tanh(1.0), round(X,V).
test(asinh, V =:= 1.0) :- X is asinh(sinh(1.0)), round(X,V).
test(acosh, V =:= 1.0) :- X is acosh(cosh(1.0)), round(X,V).
test(atanh, V =:= 1.0) :- X is atanh(tanh(1.0)), round(X,V).

:- end_tests(hyperbolic).

:- begin_tests(rationalize).

:- if(current_prolog_flag(bounded,false)).

test(trip, R = 51 rdiv 10) :-
	R is rationalize(5.1).

:- endif.

:- end_tests(rationalize).

:- begin_tests(minint).

test_minint(N) :-
        integer(N),
        format(atom(A), '~w', [N]),
        A == '-9223372036854775808'.

test(decimal) :- test_minint(-9223372036854775808).
test(spaced_decimal) :- test_minint(-9 223 372 036 854 775 808).

test(binary) :- test_minint( -0b1000000000000000000000000000000000000000000000000000000000000000).
test(spaced_binary) :- test_minint(-0b10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000).

test(octal) :- test_minint(-0o1000000000000000000000).
test(spaced_octal) :- test_minint(-0o10 0000 0000 0000 0000 0000).

test(hexadecimal) :- test_minint(-0x8000000000000000).
test(spaced_hexadecimal) :- test_minint(-0x8000_0000_0000_0000).

:- end_tests(minint).

:- begin_tests(minint_promotion).

test_minint_promotion(N) :-
        (   current_prolog_flag(bounded,true)
        ->  float(N)
        ;   integer(N),
            format(atom(A), '~w', [N]),
            A == '-9223372036854775809'
        ).

test(decimal) :- test_minint_promotion(-9223372036854775809).
test(spaced_decimal) :- test_minint_promotion(-9 223 372 036 854 775 809).

test(binary) :- test_minint_promotion(-0b1000000000000000000000000000000000000000000000000000000000000001).
test(spaced_binary) :- test_minint_promotion(-0b10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001).

test(octal) :- test_minint_promotion(-0o1000000000000000000001).
test(spaced_octal) :- test_minint_promotion(-0o10 0000 0000 0000 0000 0001).

test(hexadecimal) :- test_minint_promotion(-0x8000000000000001).
test(spaced_hexadecimal) :- test_minint_promotion(-0x8000_0000_0000_0001).

:- if(\+current_prolog_flag(bounded,true)).
test(mpz_to_int64, A == -9223372036854775808) :-
	A is -9223372036854775808-1+1.
:- endif.

test(addition) :-
	X is -9223372036854775808 + -1,
	test_minint_promotion(X).
test(addition) :-
	X is -1 + -9223372036854775808,
	test_minint_promotion(X).
test(subtraction) :-
	X is -9223372036854775808 - 1,
	test_minint_promotion(X).
:- if(current_prolog_flag(bounded,false)).
test(multiplication) :-
	X is -9223372036854775808 * 2 - -9223372036854775807,
	test_minint_promotion(X).
test(multiplication) :-
	X is 2 * -9223372036854775808 - -9223372036854775807,
	test_minint_promotion(X).
test(multiplication) :-
	X is -4294967296 * 4294967296 - -9223372036854775807,
	test_minint_promotion(X).
:- endif.

:- end_tests(minint_promotion).

:- begin_tests(maxint).

test_maxint(N) :-
        integer(N),
        format(atom(A), '~w', [N]),
        A == '9223372036854775807'.

test(decimal) :- test_maxint(9223372036854775807).
test(spaced_decimal) :- test_maxint(9 223 372 036 854 775 807).

test(binary) :- test_maxint(0b111111111111111111111111111111111111111111111111111111111111111).
test(spaced_binary) :- test_maxint(0b1111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111).

test(octal) :- test_maxint(0o777777777777777777777).
test(spaced_octal) :- test_maxint(0o7 7777 7777 7777 7777 7777).

test(hexadecimal) :- test_maxint(0x7fffffffffffffff).
test(spaced_hexadecimal) :- test_maxint(0x7fff_ffff_ffff_ffff).

:- end_tests(maxint).

:- begin_tests(maxint_promotion).

test_maxint_promotion(N) :-
        (   current_prolog_flag(bounded,true)
        ->  float(N)
        ;   integer(N),
            format(atom(A), '~w', [N]),
            A == '9223372036854775808'
        ).

test(decimal) :- test_maxint_promotion(9223372036854775808).
test(spaced_decimal) :- test_maxint_promotion(9 223 372 036 854 775 808).

test(binary) :- test_maxint_promotion(0b1000000000000000000000000000000000000000000000000000000000000000).
test(spaced_binary) :- test_maxint_promotion(0b10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000).

test(octal) :- test_maxint_promotion(0o1000000000000000000000).
test(spaced_octal) :- test_maxint_promotion(0o10 0000 0000 0000 0000 0000).

test(hexadecimal) :- test_maxint_promotion(0x8000000000000000).
test(spaced_hexadecimal) :- test_maxint_promotion(0x8000_0000_0000_0000).

test(addition) :-
	X is 9223372036854775807 + 1,
	test_maxint_promotion(X).
test(addition) :-
	X is 1 + 9223372036854775807,
	test_maxint_promotion(X).
test(subtraction) :-
	X is 9223372036854775807 - -1,
	test_maxint_promotion(X).
:- if(current_prolog_flag(bounded,false)).
test(multiplication) :-
	X is 9223372036854775807 * 2 - 9223372036854775806,
	test_maxint_promotion(X).
test(multiplication) :-
	X is 2 * 9223372036854775807 - 9223372036854775806,
	test_maxint_promotion(X).
test(multiplication) :-
	X is 4294967295 * 4294967297 - 9223372036854775807,
	test_maxint_promotion(X).
:- endif.

:- end_tests(maxint_promotion).

:- begin_tests(float_overflow).

:- if(current_prolog_flag(bounded,false)).

test(max) :-
	A is max(1.0, 1<<10000),
	A is 1<<10000.

test(add, error(evaluation_error(float_overflow))) :-
	A is 1<<10000 + 10.0,
	writeln(A).
test(minus, error(evaluation_error(float_overflow))) :-
	A is 1<<10000 - 10.0,
	writeln(A).
test(mul, error(evaluation_error(float_overflow))) :-
	A is 1<<10000 * 10.0,
	writeln(A).
test(div, error(evaluation_error(float_overflow))) :-
	A is 1<<10000 / 10.0,
	writeln(A).
test(div, error(evaluation_error(float_overflow))) :-
	A is 1<<10000000 / ((1<<10000)+19),
	writeln(A).

:- endif.

:- end_tests(float_overflow).

:- begin_tests(float_zero).

min_zero(X) :-
	X is -1.0/10e300/10e300.

test(eq) :-
	min_zero(X),
	X =:= 0.0.
test(lt, fail) :-
	min_zero(X),
	X < 0.0.
test(gt, fail) :-
	min_zero(X),
	X > 0.0.
test(eq, fail) :-
	min_zero(X),
	X == 0.0.
test(lt) :-
	min_zero(X),
	X @< 0.0.
test(gt, fail) :-
	min_zero(X),
	X > 0.0.
test(cmp, D == (<)) :-
	min_zero(X),
	compare(D, X, 0.0).
test(cmp, D == (>)) :-
	min_zero(X),
	compare(D, 0.0, X).

:- end_tests(float_zero).

:- begin_tests(float_special).

test(cmp, fail) :-
	(   nan > nan
	;   nan =:= nan
	;   nan < nan
	).

:- end_tests(float_special).

:- begin_tests(arith_misc).

test(string) :-
	0'a =:= "a".

:- end_tests(arith_misc).
