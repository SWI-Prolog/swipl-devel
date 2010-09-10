/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
		    gcd,
		    shift,
		    errors,
		    ar_builtin
		  ]).

:- begin_tests(div).

test(mod, true) :-
	forall(between(-10, 10, X),
	       forall((between(-10, 10, Y), Y =\= 0),
		      (	  Q is div(X, Y),
			  M is mod(X, Y),
			  X =:= Y*Q+M
		      ))).

:- end_tests(div).

:- begin_tests(gdiv).

:- if(current_prolog_flag(bounded,false)).

test(minint, X == 9223372036854775808) :-
	X is -9223372036854775808 // -1.

:- else.

test(minint, error(evaluation_error(integer_overflow))) :-
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

:- begin_tests(shift).

test(shift_right_large, X == 0) :-
	X is 5>>64.
test(shift_right_large, X == 0) :-
	X is 5>>(1<<62).
test(shift_right_large,
     [condition(current_prolog_flag(bounded, false)), X == 0]) :-
	X is 5>>(1<<100).

:- end_tests(shift).

:- begin_tests(gcd).

test(gcd, X == 4) :-
	X is gcd(100, 24).
test(gcd, X == 4) :-
	X is gcd(24, 100).		% seems to be some argument ordering

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
