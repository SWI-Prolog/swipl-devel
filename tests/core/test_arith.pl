/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2024, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.

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
:- use_module(library(debug)).

/** <module> Test Prolog core arithmetic functions

@author	Jan Wielemaker
*/

test_arith :-
	run_tests([ arith_basics,
		    div,
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
		    bigint,
                    minint,
                    minint_promotion,
                    maxint,
                    maxint_promotion,
		    round,
		    float_misc,
		    float_overflow,
		    float_zero,
		    float_special,
		    float_compare,
		    arith_misc,
		    max_integer_size,
		    moded_int
		  ]).

:- begin_tests(arith_basics).

test(arith_1, A == 10) :-
    A is 5 + 5.
test(arith_2) :-
    0 =:= -5 + 2.5 * 2.
test(arith_3, B =:= -1) :-
    A is pi,
    B is cos(A).
test(arith_4) :-
    0 =:= 10 - 3.4 - 6.6.
test(arith_5) :-
    1 =:= integer(0.5).
test(arith_6) :-
    4.5 =:= abs(-4.5),
    4 =:= abs(4),
    4 =:= abs(-4).
test(arith_7) :-
    5.5 =:= max(1, 5.5).
test(arith_8) :-
    -6 is min(-6, -5.5).
test(arith_9) :-
    4000 =:= integer(10000 * float_fractional_part(1.0e10 + 0.4)).
test(arith_10) :-
    -4000 =:= integer(10000 * float_fractional_part(-1.0e10 - 0.4)).
test(float_fractional_part_1) :-
    0 is float_fractional_part(4).
test(arith_11) :-
    1.0 is sin(pi/2),
    \+ 1 is sin(pi/2).
test(arith_12) :-
    1.0 is float(sin(pi/2)).
test(arith_13) :-
    1.0 =:= sin(pi/2).
test(sign_1) :-
    -1   is sign(-1),   0   is sign(0),   1   is sign(1).
test(sign_2) :-
    -1.0 is sign(-1.5), 0.0 is sign(0.0), 1.0 is sign(pi).
test(sign_3) :-
    0.0 is sign(-0.0).
test(copysign_1) :-
    -1 is copysign(1, -0.0),
     1 is copysign(1, 0.0).
test(copysign_2) :-
    -2.0 is copysign(2.0, -0.0),
    2.0 is copysign(2.0, 0.0).
test(copysign_3) :-
    1  is copysign(1, 1),
    -1 is copysign(1, -1).
test(abs_1, A == 0.0) :-
    A is abs(-0.0).
test(floor_1) :-
    0 is floor(0.0),
    0 is floor(0.9),
    -1 is floor(-0.1),
    -1 is floor(-0.9).
test(ceil_1) :-
    0 is ceil(0.0),
    1 is ceil(0.9),
    0 is ceil(-0.1),
    0 is ceil(-0.9).
test(truncate_1) :-
    1 is truncate(1.1),
    1 is truncate(1.9),
    -1 is truncate(-1.1),
    -1 is truncate(-1.9).
:- if(current_prolog_flag(bounded, false)).
test(floor_2, A == 9223372036854775808) :-
    A is floor(9223372036854775808.000000).
test(ceil_2, A == 9223372036854775808) :-
    A is ceil(9223372036854775808.000000).
:- endif.
test(round_2, A == 9223372036854775807) :-
    A is round(9223372036854775807.000000).
test(integer_2, A == 9223372036854775807) :-
    A is integer(9223372036854775807.000000).
test(int_1) :-
    A is 1<<31, integer(A).
test(cmp_1) :-
    A is 100.0e6, 67 < A.

:- end_tests(arith_basics).

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
:- if(current_prolog_flag(prefer_rationals, true)).
test(rat, X =:= 32/243) :-
	X is (2/3)^5,
	assertion(atomic(X)).
test(rat, X =:= 1) :-
	X is (2/3)^0,
	assertion(atomic(X)).
test(rat, X =:= 243/32) :-
	X is (2/3)^(-5),
	assertion(atomic(X)).
:- else.
test(rat) :-
	X is (2 rdiv 3)^(5),
	assertion(rational(X, 32, 243)).
test(rat, X == 1) :-
	X is (2 rdiv 3)^(0).
test(rat) :-
	X is (2 rdiv 3)^(-5),
	assertion(rational(X, 243, 32)).
:- endif.

% tests LibBF mpz_ui_pow_ui() overflow from ulong to mpz handling
test('2^65', A == 36893488147419103232) :-
	A is 2^65.

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

round(X, R) :- R is round(X*1000)/1000.0.

test(sinh, V =:= 1.175) :- X is sinh(1.0), round(X,V).
test(cosh, V =:= 1.543) :- X is cosh(1.0), round(X,V).
test(tanh, V =:= 0.762) :- X is tanh(1.0), round(X,V).
test(asinh, V =:= 1.0) :- X is asinh(sinh(1.0)), round(X,V).
test(acosh, V =:= 1.0) :- X is acosh(cosh(1.0)), round(X,V).
test(atanh, V =:= 1.0) :- X is atanh(tanh(1.0)), round(X,V).

:- end_tests(hyperbolic).

:- begin_tests(bigint,
	       [ condition(current_prolog_flag(bounded, false))
	       ]).

:- dynamic
    gmp_clause/2.

fac(1,1) :- !.
fac(X,N) :-
    X > 1,
    X2 is X - 1,
    fac(X2, N0),
    N is N0 * X.

oefac(X, Fac) :-                        % produce large pos and neg ints
    fac(X, F0),
    odd_even_neg(X, F0, Fac).

odd_even_neg(X, V0, V) :-
    (   X mod 2 =:= 0
    ->  V = V0
    ;   V is -V0
    ).

:- if(current_prolog_flag(bounded, false)). % GMP implies rational

ratp(C, X, X ) :-
    C =< 0,
    !.
ratp(Count, In, Out) :-
    succ(Count0, Count),
    T is In + (In rdiv 2),
    ratp(Count0, T, Out).

dec(X, Y) :-
    Y is X - 1.

unbound(_).

test(add_promote1) :-
    A is 1 + 9223372036854775807,
    A =:= 9223372036854775808.
test(add_promote2) :-
    A is -9223372036854775808 + -1,
    A =:= -9223372036854775809.
test(neg_1) :-                           % check conversion of PLMININT
    A is -9223372036854775808,
    -A =:= 9223372036854775808.
test(neg_2) :-
    A is -(1<<63+1), A == -9223372036854775809.
test(neg_promote) :-
    A is 0 - -9223372036854775808,
    A =:= 9223372036854775808.
test(abs_1) :-
    A is -9223372036854775808,
    abs(A) =:= 9223372036854775808.
test(sign_1) :-
    -1 =:= sign(-5 rdiv 3),
    0 =:= sign(0 rdiv 1),
    1 =:= sign(2 rdiv 7),
    fac(60, X),
    -1 =:= sign(-X),
    1 =:= sign(X).
test(floor_1) :-
    A is floor(1.0e20),
    integer(A),
    1.0e20 =:= float(A).
test(floor_2) :-
    0 is floor(9 rdiv 10),
    -1 is floor(-1 rdiv 10),
    -1 is floor(-9 rdiv 10).
test(ceil_1) :-
    A is ceil(1.0e20),
    integer(A),
    1.0e20 =:= float(A).
test(ceil_2) :-
    1 is ceil(9 rdiv 10),
    0 is ceil(-1 rdiv 10),
    0 is ceil(-9 rdiv 10).
test(msb_0) :-
    catch(0 =:= msb(0), E, true),
    E = error(domain_error(not_less_than_one, 0), _).
test(msb_1) :-
    10 =:= msb(1<<10).
test(msb_2) :-
    100 =:= msb(1<<100).
test(lsb_0) :-
    catch(0 =:= lsb(0), E, true),
    E = error(domain_error(not_less_than_one, 0), _).
test(lsb_1) :-
    10 =:= lsb(1<<10).
test(lsb_2) :-
    100 =:= lsb(1<<100).
test(popcount_1) :-
    1 =:= popcount(1<<5).
test(popcount_2) :-
    1 =:= popcount(1<<100).
test(shift_1) :-
    A is 1<<54, B is A<<8,
    B =:= 4611686018427387904.
test(shift_2) :-
    A is 1<<55, B is A<<8,
    B =:= 9223372036854775808.
test(shift_3) :-
    unbound(A),
    forall(between(1, 100, X), % either resource error or representation error.
           catch(A is 1<<(1<<X), error(_, _), true)).
test(fac_1) :-
    fac(25, X),
    X == 15511210043330985984000000. % computed with bc
test(arith_1) :-
    A = 12345678901234567890123456789,
    B = 7070707070707070707,
    X is A * (A * B * A * B * A * B),
    integer(X),
    Y is B * A * B * A * B * A,
    integer(Y),
    R is X / Y,
    R == A.
test(pow_1) :-
    A is 10**50, integer(A).
test(pow_2) :-
    A is -10**3, integer(A), A = -1000.
test(pow_3) :-
    A is 0**0, A = 1.
test(pow_4) :-
    A is -1**0, A = 1.
test(pow_5) :-
    A is -1**0, A = 1.
test(pow_6) :-
    A is -1**((1<<100)+1), A == -1.
test(pow_7) :-
    A is -100**5,
    A < 0,
    abs(A) =:= 100**5.
test(powm_1) :-
    A is powm(2,4,2),
    A == 0.
test(powm_2) :-
    A is powm(2,4,3),
    A == 1.
test(integer_1) :-                       % rounding integer conversion
    0 =:= integer(1 rdiv 3),
    1 =:= integer(2 rdiv 3),
    0 =:= integer(-1 rdiv 3),
       -1 =:= integer(-2 rdiv 3).
test(integer_2) :-
    0 =:= integer(1/3),
    1 =:= integer(2/3),
    0 =:= integer(-1/3),
       -1 =:= integer(-2/3).
test(rational_1) :-                              % IEEE can represent 0.25
    rational(1/4) =:= 1 rdiv 4.
test(rational_2) :-
    A is 2 rdiv 4,
    rational(A, 1, 2).
test(rational_3) :-
    dec(6 rdiv 5, X),
    rational(X, 1, 5).
test(rational_4) :-
    X is 5 rdiv 3,
    rational(X, 5, 3).
test(rationalize_1) :-
    A is rationalize(0.0), A == 0,
    B is rationalize(0.1), rational(B, 1, 10),
    C is rationalize(10.0), C == 10,
    D is rationalize(-0.1), rational(D, -1, 10).
test(rationalize_2) :-
    pi =:= float(rationalize(pi)).
test(number_1) :-
    A is 1 rdiv 3,
    rational(A).
test(float_1) :-
    1/3 =:= float(1 rdiv 3).
test(cmp_1) :-
    0 < 3,
    -1 < 0,
    -1 < 1 rdiv 3,
    1 > 0.3,
    fac(100, F100), fac(200, F200), F100 < F200,
    pi > 5 rdiv 2.
test(clause_1) :-
    Clause = (gmp_clause(X,Y) :-
                   X is Y + 3353461138769748319272960000),
    assert(Clause, Ref),
    clause(H,B,Ref),
    erase(Ref),
    (H:-B) =@= Clause.
test(comp_1) :-
    retractall(gmp_clause(_,_)),
    forall(between(1, 50, X),
           (   oefac(X, Fac),
               assert(gmp_clause(Fac, X))
           )),
    forall(between(1, 50, X),
           (   oefac(X, Fac),
               gmp_clause(Fac, Y),
               X == Y,
               clause(gmp_clause(Fac, Z), true),
               X == Z
           )),
    retractall(gmp_clause(_,_)).
test(comp_2) :-
    X is ((1 rdiv 2)*2) rdiv 3,
    rational(X, 1, 3).
test(rec_1) :-
    forall(recorded(gmp_fac, _, R), erase(R)),
    forall(between(1, 50, X),
           (   oefac(X, Fac),
               recordz(gmp_fac, X-Fac)
           )),
    forall(between(1, 50, X),
           (   oefac(X, Fac),
               recorded(gmp_fac, X-Y),
               Fac == Y
           )),
    forall(recorded(gmp_fac, _, R), erase(R)).
test(number_codes_1) :-
    fac(25, X),
    number_codes(X, Codes),         % write
    number_codes(Y, Codes),         % read
    X == Y.
test(atom_number_1) :-
    fac(100, X),
    atom_number(Atom, X),           % write
    atom_number(Atom, Y),           % read
    X == Y.
test(hex_1) :-
    atom_number('0xFFFFFFFFFFFFFFFF', 18446744073709551615).
test(fmtd_1) :-
    format(atom(X), '~d', [12345678901234567890123456]),
    X == '12345678901234567890123456'.
test(fmtd_2) :-
    format(atom(X), '~2d', [12345678901234567890123456]),
    X == '123456789012345678901234.56'.
test(fmtD_1) :-
    format(atom(X), '~D', [12345678901234567890123456]),
    X == '12,345,678,901,234,567,890,123,456'.
test(fmtD_2) :-
    format(atom(X), '~2D', [12345678901234567890123456]),
    X ==  '123,456,789,012,345,678,901,234.56'.
test(fmtf_1) :-
    ratp(999, 1, X),
    format(atom(S), '~5f', [X]),
    sub_atom(S, _, _, 0, '935376.65824').
test(random) :-
    A is random((1<<200)-((1<<200)-20)),
    A < 20.
test(length) :-
    N is 1<<66,
    catch(length(_L, N), Error, true),
    Error = error(resource_error(stack), _).
test(ar_add_ui) :-                       % check realloc of gmp number
    A = 1000000000000000000000,
    X is A+1,
    X == 1000000000000000000001.
test(bf_trig_alloc, Len == 7634) :-
    exponential_div(3,16000,R),
    format(string(S), '~d', [R]),
    string_length(S, Len).

% From Ciao playground
% Mostly testing mememory management that was broken for in
% LibBF get_trig() which maintains a cache.
exponential_div(_Base, 0, 1) :-
    !.
exponential_div(Base, Exp, Res):-
    Exp > 0,
    HalfExp is Exp // 2,
    exponential_div(Base, HalfExp, HalfRes),
    (   Exp mod 2 =:= 0
    ->  Res is HalfRes*HalfRes
    ;   Res is HalfRes*HalfRes*Base
    ).

:- endif.

:- end_tests(bigint).

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

:- begin_tests(round).

test(half_down, N == 0) :-
	X is nexttoward(0.5,-10), N is round(X).
test(nhalf_up, N == 0) :-
	X is nexttoward(-0.5,10), N is round(X).
test(maxint, A == 9223372036854775807 ) :-
	A is round(9223372036854775807.000000).
test(minint, A == -9223372036854775808 ) :-
	A is round(-9223372036854775808.000000).

:- end_tests(round).

:- begin_tests(float_misc).

foverflow(X) :-
    X2 is X * pi * pi,
    foverflow(X2),
    1 = 1.                  % avoid tail-recursion to force termination

ftest(4.5).
ftest :-
    ftest(4.5).

erase_all(Key) :-
    recorded(Key, _, Ref),
    erase(Ref),
    fail.
erase_all(_).

test(float_1, X == 4.5) :-
    ftest(X).
test(float_2) :-
    ftest.
test(float_3, X == 6.7) :-
    erase_all(f),                   % play safe
    recorda(f, 6.7, Ref),
    recorded(f, X),
    erase(Ref).
test(float_4, X == 10.67) :-
    X is 10.67.
test(float_5, X == 4.5) :-
    clause(ftest(X), true).
test(float_6, X == 4.5) :-
    clause(ftest, ftest(X)).
test(float_7, error(evaluation_error(float_overflow))) :-
    foverflow(2.2).
test(float_8, error(evaluation_error(float_overflow))) :-
    foverflow(-2.2).

:- end_tests(float_misc).

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
	A is float(1<<10000000) / float((1<<10000)+19),
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

:- begin_tests(float_compare).

test(max_nan, X == 1.5NaN) :-
    X is max(1, nan).
test(max_nan, X == 1.5NaN) :-
    X is max(nan, 1).

test(min_nan, X == 1.5NaN) :-
    X is min(1, nan).
test(min_nan, X == 1.5NaN) :-
    X is min(nan, 1).

:- end_tests(float_compare).

:- begin_tests(max_integer_size,
	       [ condition(current_prolog_flag(bounded,false))
	       ]).

cleanup :-
	set_prolog_flag(max_integer_size, infinite).

test(set, [Max == 10 000, cleanup(cleanup)]) :-
	set_prolog_flag(max_integer_size, 10 000),
	current_prolog_flag(max_integer_size, Max),
	set_prolog_flag(max_integer_size, infinite),
	assertion(\+current_prolog_flag(max_integer_size,_)).

% Note that this tests the _allocated_ size.  LibBF does not store
% the least significant zero bits, so we must make sure the least
% significant bit is set.
%
% Emscripten does not handle the longjmp() correctly, so we disable
% this test for now.

test(overflow,
     [ error(resource_error(tripwire(max_integer_size, 1000))),
       cleanup(cleanup)
     ]):-
	set_prolog_flag(max_integer_size, 1000),
	A is 1<<10000 + 1,
	integer(A).

:- end_tests(max_integer_size).

:- begin_tests(moded_int).

test(between_1) :-
    between(0, 10, 5).
test(between_2, fail) :-
    between(0, 10, 20).
test(between_3, Xs == [1, 2, 3, 4, 5, 6]) :-
    findall(X, between(1, 6, X), Xs).
test(between_4, Xs == [-4, -3, -2, -1]) :-
    findall(X, between(-4, -1, X), Xs).
test(succ_1, X == 1) :-
    succ(0, X).
test(succ_2, fail) :-
    succ(_, 0).
test(succ_3, error(domain_error(not_less_than_zero, -1))) :-
    succ(_, -1).
test(plus_1) :-
    plus(1, 2, 3).

:- end_tests(moded_int).

:- begin_tests(arith_misc).

test(string) :-
	0'a =:= "a".
test(float_rval) :-
	6.5 is max(6.5,3).
:- set_prolog_flag(optimise, true).
test(float_rval) :-
	6.5 is max(6.5,3).

:- end_tests(arith_misc).

% No tests below here because the set_prolog_flag(optimise, true) above
% causes them not to be loaded.
