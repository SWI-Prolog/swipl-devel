/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1996-2017, University of Amsterdam
                              VU University Amsterdam
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

%:- set_prolog_flag(optimise, true).
:- set_prolog_flag(optimise_debug, false).
%:- set_prolog_flag(trace_gc, true).
:- asserta(user:file_search_path(library, '../packages/plunit')).
:- [library(plunit)].
:- set_test_options([load(always), silent(true), sto(true), cleanup(true)]).

:- use_module(library(lists)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog test file.  A test is a clause of the form:

	<TestSet>(<Name>-<Number>) :- Body.

If the body fails, an appropriate  error   message  is  printed. So, all
goals are supposed to  succeed.  The   predicate  testset/1  defines the
available test sets. The public goals are:

	?- runtest(+TestSet).
	?- test.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- format(user_error,
	  'SWI-Prolog test suite.  To run all tests run ?- test.~n~n', []).

% Required to get this always running regardless of user LANG setting.
% Without this the tests won't run on machines with -for example- LANG=ja
% according to NIDE Naoyuki, nide@ics.nara-wu.ac.jp.  Thanks!
%
% NOTE: In 5.5.x we should not need this any longer

% :- getenv('LANG', _) -> setenv('LANG', 'C'); true.

% The test-suite Tests/library/test_date.pl depends on  the timezone. As
% correct results are only  provided  for   the  CET  (Central European)
% timezine we use this. Timezone cannot be  changed at runtime, so we do
% this early.

:- setenv('TZ', 'CET').

		 /*******************************
		 *	      SYNTAX		*
		 *******************************/

syntax(op-1) :-
	atom_to_term("3+4*5", +(3,*(4,5)), []).
syntax(op-2) :-
	atom_to_term("1+2+3", +(+(1,2),3), []).
syntax(op-3) :-
	catch(atom_to_term("a:-b:-c", _, _), E, true),
	E = error(syntax_error(operator_clash), _).
syntax(op-4) :-
	op(600, fx, op1),
	atom_to_term("op1 1+2", op1(+(1,2)), []).
syntax(op-5) :-
	op(600, fx, op1),
	catch(atom_to_term("op1 op1 1", _, _), E, true),
	E = error(syntax_error(operator_clash), _).
syntax(op-6) :-
	op(600, fy, op1),
	atom_to_term("op1 op1 1", op1(op1(1)), []).
syntax(op-7) :-
	op(600, fy, op1),
	op(500, xf, op2),
	catch(atom_to_term("op1 a op2", op1(op2(a)), []), E, true),
	E = error(syntax_error(operator_clash), _).
syntax(op-8) :-				% assume 200 fy and 500 yfx
	atom_to_term("- - a", -(-(a)), []).
syntax(atom-1) :-
	atom_codes('\003\\'\n\x80\', X),
	X == [3, 39, 10, 128].
syntax(char-1) :-
	10 == 0'\n.
syntax(char-2) :-
	52 == 0'\x34.
syntax(char-3) :-
	"\\" =:= 0'\\.
syntax(char-4) :-
	1-48 == 1-0'0.
syntax(cannot_start_term-1) :-
	catch(term_to_atom(_T, 'p(]'), E, true),
	E = error(syntax_error(cannot_start_term), _).


:- op(100, yf, af).

syntax(string-1) :-
	'\c ' == ''.
syntax(string-2) :-
	'x\c y' == xy.
syntax(quote-1) :-
	catch(atom_to_term('\'\\x\'', _, _), E, true),
	E = error(syntax_error(undefined_char_escape(x)), _).
syntax(quote-2) :-
	'\x61' == a.
syntax(quote-3) :-
	'\x61\' == a.
syntax(quote-4) :-
	char_code('\'', 39).
syntax(quote-5) :-
	0'\' == 0''.
syntax(quote-6) :-
	0'\' == 0'''.
syntax(quote-7) :-
	atom_to_term('\'\\\n\'', T, _),
	T == ''.
syntax(base-1) :-
	1+1 == 1+1.
syntax(base-2) :-
	16'af == 175.
syntax(base-3) :-
	10 af == af(10).
syntax(base-4) :-
	A = 1, B is A+1, B == 2.
syntax(base-5) :-
	A is 1.0e+0+1, A == 2.0.
syntax(number-2) :-
	catch(atom_to_term('2\'', _, _), E, true),
	E = error(syntax_error(end_of_file), _).
syntax(zero-1) :-
	term_to_atom(T, 'hello("\000\x")'),
	T = hello(A0),
	(   A0 == [0,120]		% depending on the double quotes flag
	;   string_codes(A0, [0,120])
	;   A0 == ['\u0000', x]
	), !.
syntax(latin-1) :-
	atom_codes(A, [247]),
	atom_to_term(A, T, []),
	atom_codes(T, [247]).

:- op(0, yf, af).


		 /*******************************
		 *	       WRITE		*
		 *******************************/

:- op(200, fx, op_fa).

write_test(q-1) :-
	T = -(0),
	term_to_atom(T, X),
	term_to_atom(T2, X),
	T == T2.
write_test(q-2) :-
	T = +(0),
	term_to_atom(T, X),
	term_to_atom(T2, X),
	T == T2.
write_test(q-3) :-
	term_to_atom(+(a), X), X == '+a'.
write_test(q-4) :-
	term_to_atom('/*', X), X == '\'/*\''.	%0'
write_test(q-5) :-
	term_to_atom('/**', X), X == '\'/**\''.
write_test(q-6) :-
	term_to_atom('*/*', X), X == '*/*'.
write_test(q-7) :-
	term_to_atom(p((0|a)), X), X == 'p((0|a))'.
write_test(q-8) :-
	term_to_atom(p((a|b)), X), X == 'p((a|b))'.
write_test(q-9) :-
	term_to_atom(., X), X == '\'.\''.
write_test(op-1) :-
	term_to_atom(-((a,b)), X), X == '- (a,b)'.
write_test(op-2) :-
	term_to_atom(op_fa((a,b)), X), X == 'op_fa (a,b)'.
write_test(op-3) :-
	term_to_atom(dynamic((a,b)), X), X == 'dynamic a,b'.
write_test(c-1) :-
	T = [a,b,c|T],
	term_to_atom(T, X),
	term_to_atom(@(T2,S2), X),
	maplist(call, S2),
	T2 =@= T.
write_test(s-1) :-
	term_to_atom([(a,b)], X),
	X = '[(a,b)]'.


		 /*******************************
		 *	      FORMAT		*
		 *******************************/

format_test(intD-1) :-
	format(atom(X), '~D', [1000]),
	X == '1,000'.
format_test(intD-2) :-
	format(atom(X), '~2D', [1000]),
	X == '10.00'.
format_test(intD-3) :-
	format(atom(X), '~2D', [100000]),
	X == '1,000.00'.
format_test(intr-1) :-
	format(atom(X), '~16r', [1000]),
	X == '3e8'.
format_test(intR-1) :-
	format(atom(X), '~16R', [1000]),
	X == '3E8'.
format_test(oncodes-1) :-
	format(codes(C), 'hello ~w', [world]),
	atom_codes('hello world', C).
format_test(oncodes-2) :-
	format(codes(C,T), 'hello ~w', [world]),
	atom_codes('hello world', HW),
	append(HW, T2, C2),
	C-T =@= C2-T2.
format_test(onstring-1) :-
	format(string(S), 'hello ~w', [world]),
	string(S),
	atom_string('hello world', S).


		 /*******************************
		 *	       UNIFY		*
		 *******************************/

%	Some cyclic unification tests (normal unification should be fixed
%	already).

unify(cycle-1) :-			% Kuniaki Mukai
	X = f(Y), Y=f(X), X=Y.
unify(cycle-2) :-			% Kuniaki Mukai
	X = f(X), Y=f(Y), X=f(Y).


		 /*******************************
		 *    UNIFY-WITH-OCCURS-CHECK	*
		 *******************************/

occurs_check(simple-1) :-
	\+ unify_with_occurs_check(A, list(A)).
occurs_check(simple-2) :-
	unify_with_occurs_check(_A, _B).
occurs_check(attvar-1) :-		% test wakeup
	freeze(X, X = Y),
	unify_with_occurs_check(X, a),
	Y == a.
occurs_check(attvar-2) :-		% test occurs-check
	freeze(A, true),
	\+ unify_with_occurs_check(A, list(A)).
occurs_check(attvar-3) :-
	freeze(A, true),
	unify_with_occurs_check(A, A).
occurs_check(attvar-4) :-
	freeze(A, true),
	freeze(B, true),
	unify_with_occurs_check(A, B).


		 /*******************************
		 *	       CYCLIC		*
		 *******************************/

cyclic(term_hash-1) :-
	X = f(X), term_hash(X, T),
	integer(T).
cyclic(streq-1) :-
	X = [X], Y = [Y], X =@= Y.
cyclic(test-1) :-
	X = f(X),
	cyclic_term(X).
cyclic(test-2) :-
	X = f(X),
	cyclic_term(a(X, a)).
cyclic(test-3) :-
	X = f(X),
	cyclic_term(a(a, X)).
cyclic(test-4) :-
	acyclic_term(f(x)).
cyclic(test-5) :-
	acyclic_term(_).
cyclic(test-6) :-
	X = f(a), acyclic_term(a(X, X)).
cyclic(list-1) :-
	L = [a|L], \+ is_list(L).
cyclic(sort-1) :-
	L = [a,b,c|L],
	sort(L, List),
	List == [a,b,c].


		 /*******************************
		 *	    UNIFIABLE		*
		 *******************************/

unifiable(unifiable-1) :-
	unifiable(X, 1, S),
	S == [X=1].
unifiable(unifiable-2) :-
	unifiable(a(X,X), a(Y,Z), S),
	S == [Z=X, Y=X].
unifiable(gc-1) :-
	trim_stacks,
	(   between(2, 10, N),
	    Len is 1<<N,
	    numlist(1, Len, L1),
	    attvar_list(Len, L2),
	    unifiable(L1, L2, _Unifier),
	    fail
	;   true
	).

attvar_list(0, []) :- !.
attvar_list(N, [H|T]) :-
	freeze(H, integer(H)),
	N2 is N - 1,
	attvar_list(N2, T).


		 /*******************************
		 *      INTEGER ARITHMETIC	*
		 *******************************/

arithmetic(between-1) :-
	between(0, 10, 5).
arithmetic(between-2) :-
	\+ between(0, 10, 20).
arithmetic(between-3) :-
	findall(X, between(1, 6, X), Xs),
	Xs == [1, 2, 3, 4, 5, 6].
arithmetic(between-4) :-
	findall(X, between(-4, -1, X), Xs),
	Xs == [-4, -3, -2, -1].
arithmetic(succ-1) :-
	succ(0, X), X == 1.
arithmetic(succ-2) :-
	\+ succ(_, 0).
arithmetic(succ-3) :-
	catch(succ(_, -1), E, true),
	E = error(domain_error(not_less_than_zero, -1), _).
arithmetic(plus-1) :-
	plus(1, 2, 3).

		 /*******************************
		 *	  SIMPLE THINGS		*
		 *******************************/

arithmetic(arith-1) :-
	A is 5 + 5,
	A == 10.
arithmetic(arith-2) :-
	0 =:= -5 + 2.5 * 2.
arithmetic(arith-3) :-
	A is pi,
	B is cos(A),
	B =:= -1.
arithmetic(arith-4) :-
	0 =:= 10 - 3.4 - 6.6.
arithmetic(arith-5) :-
	1 =:= integer(0.5).
arithmetic(arith-6) :-
	4.5 =:= abs(-4.5),
	4 =:= abs(4),
	4 =:= abs(-4).
arithmetic(arith-7) :-
	5.5 =:= max(1, 5.5).
arithmetic(arith-8) :-
	-6 is min(-6, -5.5).
arithmetic(arith-9) :-
	4000 =:= integer(10000 * float_fractional_part(1.0e10 + 0.4)).
arithmetic(arith-10) :-
	-4000 =:= integer(10000 * float_fractional_part(-1.0e10 - 0.4)).
arithmetic(float_fractional_part-1) :-
	0 is float_fractional_part(4).
arithmetic(arith-11) :-
	1.0 is sin(pi/2),
	\+ 1 is sin(pi/2).
arithmetic(arith-12) :-
	1.0 is float(sin(pi/2)).
arithmetic(arith-13) :-
	1.0 =:= sin(pi/2).
arithmetic(sign-1) :-
	-1   is sign(-1),   0   is sign(0),   1   is sign(1).
arithmetic(sign-2) :-
	-1.0 is sign(-1.5), 0.0 is sign(0.0), 1.0 is sign(pi).
arithmetic(sign-3) :-
	0.0 is sign(-0.0).
arithmetic(copysign-1) :-
	-1 is copysign(1, -0.0),
	 1 is copysign(1, 0.0).
arithmetic(copysign-2) :-
	-2.0 is copysign(2.0, -0.0),
	2.0 is copysign(2.0, 0.0).
arithmetic(copysign-3) :-
	1  is copysign(1, 1),
	-1 is copysign(1, -1).
arithmetic(abs-1) :-
	A is abs(-0.0),
	A == 0.0.
arithmetic(floor-1) :-
	0 is floor(0.0),
	0 is floor(0.9),
	-1 is floor(-0.1),
	-1 is floor(-0.9).
arithmetic(ceil-1) :-
	0 is ceil(0.0),
	1 is ceil(0.9),
	0 is ceil(-0.1),
	0 is ceil(-0.9).
arithmetic(truncate-1) :-
	1 is truncate(1.1),
	1 is truncate(1.9),
	-1 is truncate(-1.1),
	-1 is truncate(-1.9).
:- if(current_prolog_flag(bounded, false)).
arithmetic(floor-2) :-
	A is floor(9223372036854775808.000000),
	A == 9223372036854775808.
arithmetic(ceil-2) :-
	A is ceil(9223372036854775808.000000),
	A == 9223372036854775808.
:- endif.
arithmetic(round-2) :-
	A is round(9223372036854775807.000000),
	A == 9223372036854775807.
arithmetic(integer-2) :-
	A is integer(9223372036854775807.000000),
	A == 9223372036854775807.


		 /*******************************
		 *	    BIG NUMBERS		*
		 *******************************/

arithmetic(int-1) :-
	A is 1<<31, integer(A).
arithmetic(cmp-1) :-
	A is 100.0e6, 67 < A.


		 /*******************************
		 *	      FLOATS		*
		 *******************************/

foverflow(X) :-
	X2 is X * pi * pi,
	foverflow(X2),
	1 = 1.			% avoid tail-recursion to force termination

ftest(4.5).
ftest :-
	ftest(4.5).

floattest(float-1) :-
	ftest(X),
	X == 4.5.
floattest(float-2) :-
	ftest.
floattest(float-3) :-
	erase_all(f),			% play safe
	recorda(f, 6.7, Ref),
	recorded(f, X),
	X == 6.7,
	erase(Ref).
floattest(float-4) :-
	X is 10.67,
	X == 10.67.
floattest(float-5) :-
	clause(ftest(X), true),
	X == 4.5.
floattest(float-6) :-
	clause(ftest, ftest(X)),
	X == 4.5.
floattest(float-7) :-
	catch(foverflow(2.2), E, true),
	E = error(evaluation_error(float_overflow), _).
floattest(float-8) :-
	catch(foverflow(-2.2), E, true),
	E = error(evaluation_error(float_overflow), _).


		 /*******************************
		 *     UNBOUNDED ARITHMETIC	*
		 *******************************/

:- dynamic
	gmp_clause/2.

fac(1,1) :- !.
fac(X,N) :-
	X > 1,
	X2 is X - 1,
	fac(X2, N0),
	N is N0 * X.

oefac(X, Fac) :-			% produce large pos and neg ints
	fac(X, F0),
	odd_even_neg(X, F0, Fac).

odd_even_neg(X, V0, V) :-
	(   X mod 2 =:= 0
	->  V = V0
	;   V is -V0
	).

:- if(current_prolog_flag(bounded, false)). % GMP implies rational

ratp(C, X, X ) :-
	C =< 0, !.
ratp(Count, In, Out) :-
        succ(Count0, Count),
        T is In + (In rdiv 2),
	ratp(Count0, T, Out).

dec(X, Y) :-
	Y is X - 1.

gmp(add-promote1) :-
	A is 1 + 9223372036854775807,
	A =:= 9223372036854775808.
gmp(add-promote2) :-
	A is -9223372036854775808 + -1,
	A =:= -9223372036854775809.
gmp(neg-1) :-				% check conversion of PLMININT
	A is -9223372036854775808,
	-A =:= 9223372036854775808.
gmp(neg-2) :-
	A is -(1<<63+1), A == -9223372036854775809.
gmp(neg-promote) :-
	A is 0 - -9223372036854775808,
	A =:= 9223372036854775808.
gmp(abs-1) :-
	A is -9223372036854775808,
	abs(A) =:= 9223372036854775808.
gmp(sign-1) :-
	-1 =:= sign(-5 rdiv 3),
	0 =:= sign(0 rdiv 1),
	1 =:= sign(2 rdiv 7),
	fac(60, X),
	-1 =:= sign(-X),
	1 =:= sign(X).
gmp(floor-1) :-
	A is floor(1.0e20),
	integer(A),
	1.0e20 =:= float(A).
gmp(floor-2) :-
	0 is floor(9 rdiv 10),
	-1 is floor(-1 rdiv 10),
	-1 is floor(-9 rdiv 10).
gmp(ceil-1) :-
	A is ceil(1.0e20),
	integer(A),
	1.0e20 =:= float(A).
gmp(ceil-2) :-
	1 is ceil(9 rdiv 10),
	0 is ceil(-1 rdiv 10),
	0 is ceil(-9 rdiv 10).
gmp(msb-0) :-
	catch(0 =:= msb(0), E, true),
	E = error(domain_error(not_less_than_one, 0), _).
gmp(msb-1) :-
	10 =:= msb(1<<10).
gmp(msb-2) :-
	100 =:= msb(1<<100).
gmp(lsb-0) :-
	catch(0 =:= lsb(0), E, true),
	E = error(domain_error(not_less_than_one, 0), _).
gmp(lsb-1) :-
	10 =:= lsb(1<<10).
gmp(lsb-2) :-
	100 =:= lsb(1<<100).
gmp(popcount-1) :-
	1 =:= popcount(1<<5).
gmp(popcount-2) :-
	1 =:= popcount(1<<100).
gmp(shift-1) :-
	A is 1<<54, B is A<<8,
	B =:= 4611686018427387904.
gmp(shift-2) :-
	A is 1<<55, B is A<<8,
	B =:= 9223372036854775808.
gmp(shift-3) :-
	unbound(A),
	forall(between(1, 100, X),
	       catch(A is 1<<(1<<X), error(resource_error(stack), _), true)).
gmp(fac-1) :-
	fac(25, X),
	X == 15511210043330985984000000. % computed with bc
gmp(arith-1) :-
	A = 12345678901234567890123456789,
	B = 7070707070707070707,
	X is A * (A * B * A * B * A * B),
	integer(X),
	Y is B * A * B * A * B * A,
	integer(Y),
	R is X / Y,
	R == A.
gmp(pow-1) :-
	A is 10**50, integer(A).
gmp(pow-2) :-
	A is -10**3, integer(A), A = -1000.
gmp(pow-3) :-
	A is 0**0, A = 1.
gmp(pow-4) :-
	A is -1**0, A = 1.
gmp(pow-5) :-
	A is -1**0, A = 1.
gmp(pow-6) :-
	A is -1**((1<<100)+1), A == -1.
gmp(pow-7) :-
	A is -100**5,
	A < 0,
	abs(A) =:= 100**5.
gmp(powm-1) :-
	A is powm(2,4,2),
	A == 0.
gmp(powm-2) :-
	A is powm(2,4,3),
	A == 1.
gmp(integer-1) :-			% rounding integer conversion
	0 =:= integer(1 rdiv 3),
	1 =:= integer(2 rdiv 3),
	0 =:= integer(-1 rdiv 3),
       -1 =:= integer(-2 rdiv 3).
gmp(integer-2) :-
	0 =:= integer(1/3),
	1 =:= integer(2/3),
	0 =:= integer(-1/3),
       -1 =:= integer(-2/3).
gmp(rational-1) :-				% IEEE can represent 0.25
	rational(1/4) =:= 1 rdiv 4.
gmp(rational-2) :-
	A is 2 rdiv 4,
	rational(A, 1, 2).
gmp(rational-3) :-
	dec(6 rdiv 5, X),
	X == 1 rdiv 5.
gmp(rational-4) :-
	X is 5 rdiv 3,
	X == (5 rdiv 3).
gmp(rational-5) :-
	5 rdiv 3 is 5 rdiv 3.
gmp(rational-6) :-
	5 rdiv X is 5 rdiv 3,
	X == 3.
gmp(rationalize-1) :-
	A is rationalize(0.0), A == 0,
	B is rationalize(0.1), B == 1 rdiv 10,
	C is rationalize(10.0), C == 10,
	D is rationalize(-0.1), D == -1 rdiv 10.
gmp(rationalize-2) :-
	pi =:= float(rationalize(pi)).
gmp(number-1) :-
	A is 1 rdiv 3,
	rational(A).
gmp(float-1) :-
	1/3 =:= float(1 rdiv 3).
gmp(cmp-1) :-
	0 < 3,
	-1 < 0,
	-1 < 1 rdiv 3,
	1 > 0.3,
	fac(100, F100), fac(200, F200), F100 < F200,
	pi > 5 rdiv 2.
gmp(clause-1) :-
	Clause = (gmp_clause(X,Y) :-
		       X is Y + 3353461138769748319272960000),
	assert(Clause, Ref),
	clause(H,B,Ref),
	erase(Ref),
	(H:-B) =@= Clause.
gmp(comp-1) :-
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
gmp(comp-2) :-
	X is ((1 rdiv 2)*2) rdiv 3,
	X == 1 rdiv 3.
gmp(rec-1) :-
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
gmp(number_codes-1) :-
	fac(25, X),
	number_codes(X, Codes),		% write
	number_codes(Y, Codes),		% read
	X == Y.
gmp(atom_number-1) :-
	fac(100, X),
	atom_number(Atom, X),		% write
	atom_number(Atom, Y),		% read
	X == Y.
gmp(hex-1) :-
	atom_number('0xFFFFFFFFFFFFFFFF', 18446744073709551615).
gmp(fmtd-1) :-
	format(atom(X), '~d', [12345678901234567890123456]),
	X == '12345678901234567890123456'.
gmp(fmtd-2) :-
	format(atom(X), '~2d', [12345678901234567890123456]),
	X == '123456789012345678901234.56'.
gmp(fmtD-1) :-
	format(atom(X), '~D', [12345678901234567890123456]),
	X == '12,345,678,901,234,567,890,123,456'.
gmp(fmtD-2) :-
	format(atom(X), '~2D', [12345678901234567890123456]),
	X ==  '123,456,789,012,345,678,901,234.56'.
gmp(fmtf-1) :-
	ratp(999, 1, X),
	format(atom(S), '~5f', [X]),
	sub_atom(S, _, _, 0, '935376.65824').
gmp(random) :-
	A is random((1<<200)-((1<<200)-20)),
	A < 20.
gmp(length) :-
	N is 1<<66,
	catch(length(_L, N), Error, true),
	Error = error(resource_error(stack), global).
gmp(ar_add_ui) :-			% check realloc of gmp number
	A = 1000000000000000000000,
	X is A+1,
	X == 1000000000000000000001.

:- endif.


		 /*******************************
		 *	     CHARACTERS		*
		 *******************************/

chars(chars-1) :-
	A is "a",
	A == 97.
chars(chars-2) :-
	A is [a],			% if "a" --> [a]
	A == 97.


		 /*******************************
		 *	 WIDE CHARACTERS	*
		 *******************************/

wchar_string("abc").				% ISO Latin-1
wchar_string([1097, 1098, 1099]).		% UCS
wchar_string([97, 98, 99, 1097, 1098, 1099]).   % Mixed
wchar_string([1097, 1098, 1099, 97, 98, 99]).	% Mixed

wchars(cmp-1) :-
	forall(( wchar_string(S1),
		 wchar_string(S2)),
	       ( atom_codes(A1, S1),
		 atom_codes(A2, S2),
		 compare(Diff, A1, A2),
		 compare(Diff, S1, S2))).
wchars(cmp-2) :-
	forall(( wchar_string(S1),
		 wchar_string(S2)),
	       ( string_codes(A1, S1),
		 string_codes(A2, S2),
		 compare(Diff, A1, A2),
		 (   compare(Diff, S1, S2)
		 ->  true
		 ;   format(user_error, 'CMP ~w ~w FAILED~n', [S1, S2])
		 ))).


		 /*******************************
		 *	   META CALLING		*
		 *******************************/

foo:hello(world).

ten(10).

meta(call-1) :-
	call(ten(X)),
	X == 10.
meta(call-2) :-
	\+ call(ten(20)).
meta(call-3) :-
	\+ call((between(0,3,X), !, X = 2)).
meta(call-4) :-
	length(X, 50000), call((is_list(X) -> true ; fail)).
meta(call-5) :-
	call((X=a;X=b)), X = b.
meta(call-6) :-
	call((foo:hello(X)->true)), X = world.
meta(call-7) :-
	call((X=a,x(X)=Y)), Y == x(a).
meta(call-8) :-
	string_codes(S, "hello world"),
	call((string(S), true)).
meta(call-9) :-
	call((foo:true, true)).
meta(call-10) :-
	call((A=x, B=x, A==B)).		% avoid I_CALL_FVX for dynamic call
meta(call-11) :-
	A = (	member(_,[1,2,3]),
		flag(a, F, F+1),
		(   F >= 999999
		->  fail
		;   true
		)
	    ),
	flag(a, Old, 0),
	forall(A, true),
	flag(a, 3, Old).
meta(call-12) :-
	G = 1,				% avoid in-line expansion
	catch(call(G), E, true),
	error(E, type_error(callable, _)).
meta(apply-1) :-
	apply(=, [a,a]).
meta(apply-2) :-
	apply(=(a), [a]).
meta(apply-3) :-
	apply(a=a, []).


		 /*******************************
		 *	      CLEANUP		*
		 *******************************/

:- dynamic
	clean_rval/1.

cleanup_1.
cleanup_2(a).
cleanup_2(b).
cleanup_3 :-
	fail.

ex(error("Nice term")).

genex1(1) :- ex(Ex), throw(Ex).
genex1(2).

genex2(1).
genex2(2) :- ex(Ex), throw(Ex).
genex2(3).

genex3(1).
genex3(2).
genex3(3) :- ex(Ex), throw(Ex).

cleanup(clean-1) :-
	retractall(clean_rval(_)),
	call_cleanup(cleanup_1, R, assert(clean_rval(R))),
	retract(clean_rval(exit)).
cleanup(clean-2) :-
	retractall(clean_rval(_)),
	call_cleanup(cleanup_2(_), R, assert(clean_rval(R))), !,
	retract(clean_rval(!)).
cleanup(clean-3) :-
	retractall(clean_rval(_)),
	\+ call_cleanup(cleanup_3, R, assert(clean_rval(R))),
	retract(clean_rval(fail)).
cleanup(clean-4) :-
	catch(call_cleanup(throw(a), true), E, true),
	E == a.
cleanup(clean-5) :-
	catch(call_cleanup(throw(a), throw(b)), E, true),
	E == a.				% ISO Proposal
cleanup(clean-6) :-
	catch(call_cleanup(true, throw(b)), E, true),
	E == b.
cleanup(clean-7) :-
	catch(call_cleanup(fail, throw(b)), E, true),
	E == b.
cleanup(clean-8) :-			% check handling of CHP_TOP
					% notrace/1 does a call-back via C
	notrace(call_cleanup(true, true)).
cleanup(clean-9) :-
	retractall(clean_rval(_)),
	call_cleanup(bagof(x, cleanup_1, _Xs), Reason,
		     assert(clean_rval(Reason))),
	retract(clean_rval(exit)).
cleanup(findall-1) :-
	catch(findall(X, genex1(X), _), E, true),
	ex(Ex), E == Ex.
cleanup(findall-2) :-
	catch(findall(X, genex2(X), _), E, true),
	ex(Ex), E == Ex.
cleanup(findall-3) :-
	catch(findall(X, genex3(X), _), E, true),
	ex(Ex), E == Ex.
cleanup(gc) :-
	garbage_collect.


		 /*******************************
		 *	    DEPTH-LIMIT		*
		 *******************************/

dl_det(1) :- !.
dl_det(N) :-
	NN is N - 1,
	dl_det(NN).

dl_ndet(1).
dl_ndet(N) :-
	NN is N - 1,
	dl_ndet(NN).

dl_fail(1) :- !, fail.
dl_fail(N) :-
	NN is N - 1,
	dl_fail(NN).

depth_limit(depth-1) :-
	G = dl_det(1),
	call_with_depth_limit(G, 10, 1),
	deterministic(true).
depth_limit(depth-2) :-
	G = dl_det(10),
	call_with_depth_limit(G, 10, 10).
depth_limit(depth-3) :-
	G = dl_det(10),
	call_with_depth_limit(G, 9, depth_limit_exceeded).
depth_limit(ndet-1) :-
	G = dl_ndet(5),
	findall(X,
		call_with_depth_limit(G, 10, X),
		L),
	L = [5, depth_limit_exceeded].
depth_limit(fail-1) :-
	\+ call_with_depth_limit(dl_fail(2), 10, _).


		 /*******************************
		 *	    TYPE TESTS		*
		 *******************************/

type_test(type-1) :-
	X = Y, var(X), Y = a, nonvar(X).
type_test(type-2) :-
	atom(hello), \+ atom(10), \+ atom("hello").
type_test(type-3) :-
	call(callable, atom),
	call(callable, term(a)),
	\+ call(callable, _Var).
type_test(type-4) :-				% a blob is not an atom.
	setup_call_cleanup(
	    open_null_stream(X),
	    \+ atom(X),
	    close(X)).


		 /*******************************
		 *	   TERM-HACKING		*
		 *******************************/

term(functor-1) :-
	functor(test(a, b), N, A), N == test, A == 2.
term(functor-2) :-
	functor(test(a, b), test, 2).
term(functor-3) :-
	functor(X, test, 2),
	forall(arg(_, X, A), var(A)).
term(arg-1) :-
	findall(N=A, arg(N, hello(a,b,c), A), T),
	T == [ 1=a, 2=b, 3=c ].
term(setarg-1) :-
	Term = foo(a, b),
	(   setarg(1, Term, c)
	->  Term == foo(c, b)
	).
term(setarg-2) :-
	Term = foo(a, b),
	(   setarg(1, Term, c),
	    garbage_collect,
	    fail
	;   Term == foo(a, b)
	).
term(univ-1) :-
	A =.. [a, B, B], A =@= a(C,C).
term(univ-2) :-
	A =.. [4.5], A == 4.5.
term(univ-3) :-
	3.4 =.. X, X == [3.4].
term(univ-4) :-
	a(a,b,c) =.. [a, a | L], L == [b,c].
term(univ-5) :-
	L = [a|L],
	catch(_T =.. L, error(E, _), true),
	E == type_error(list, L).



		 /*******************************
		 *	       LIST		*
		 *******************************/

list(memberchk-1) :-
	memberchk(a, [b, a]).
list(memberchk-2) :-
	\+ memberchk(a, []).
list(memberchk-3) :-
	memberchk(a, L), memberchk(b, L), L =@= [a,b|_].
list(sort-1) :-
	sort([], []).
list(sort-2) :-
	sort([x], [x]).
list(sort-3) :-
	sort([e,b,c,e], [b,c,e]).
list(sort-4) :-
	msort([e,b,c,e], [b,c,e,e]).
list(sort-5) :-
	keysort([e-2,b-5,c-6,e-1], [b-5,c-6,e-2,e-1]).
list(sort-6) :-
	sort([a,g,b], [a,b|G]), G == [g].
list(sort-7) :-
	sort([X], [Y]), X == Y.
list(sort-8) :-
	sort([_X, _Y], [_,_]).


		 /*******************************
		 *	       SETS		*
		 *******************************/

foo(1, a).
foo(2, b).
foo(3, c).
foo(1, d).
foo(2, e).
foo(3, f).

type(atom).
type(S) :- atom_string(string, S).
type(42).
type(3.14).
type([a, list]).
type(compound(1)).
type(compound(A, A)).
type(compound(_A, _B)).

set(X, 1) :- type(X).
set(X, 2) :- type(X).

sets(setof-1) :-
	setof(A-Pairs, setof(B, foo(A,B), Pairs), Result0),
	keysort(Result0, Result),
	Result = [ 1 - [a,d],
		   2 - [b,e],
		   3 - [c,f]
		 ].
sets(setof-2) :-
	setof(X-Ys, setof(Y, set(X,Y), Ys), R0),
	atom_string(string, S),
	keysort(R0, R),
	(   R =@= [3.14-[1, 2],
		   42-[1, 2],
		   S-[1, 2],
		   atom-[1, 2],
		   compound(1)-[1, 2],
		   [a, list]-[1, 2],
		   compound(_A0, _B0)-[1, 2], % order is not defined
		   compound(A, A)-[1, 2]]
	->  true
	;   R =@= [3.14-[1, 2],
		   42-[1, 2],
		   S-[1, 2],
		   atom-[1, 2],
		   compound(1)-[1, 2],
		   [a, list]-[1, 2],
		   compound(A, A)-[1, 2],
		   compound(_A1, _B1)-[1, 2]]
	->  true
	;   R =@= [3.14-[1, 2],
		   42-[1, 2],
		   S-[1, 2],
		   atom-[1, 2],
		   compound(1)-[1, 2],
		   compound(A, A)-[1, 2],
		   compound(_A1, _B1)-[1, 2],
		   [a, list]-[1, 2]]		% using `.`
	->  true
	;   R =@= [3.14-[1, 2],
		   42-[1, 2],
		   S-[1, 2],
		   atom-[1, 2],
		   compound(1)-[1, 2],
		   compound(_A1, _B1)-[1, 2],
		   compound(A, A)-[1, 2],
		   [a, list]-[1, 2]]
	->  true
	;   format(user_error, 'ERROR: Got ~q~n', [R])
	).
sets(vars-1) :-
	'$free_variable_set'(X^(m:Y^hello(X,Y)), G, V),
	G == m:hello(X,Y),
	V == v.
sets(bagof-1) :-
	List = [_,_,_],
	bagof(X, member(X, List), Xs),
	Xs == List.
sets(bagof-2) :-
	Goal = member(_, [1,2]),
	bagof(Goal, Goal, Xs),
	Xs == [member(1, [1,2]), member(2, [1,2])].
sets(setof-3001) :-
	List = [_,_,_],
	setof(X, member(X, List), Xs),
	Xs = List.
sets(setof-3002) :-
	List = [_,_,_],
	setof(X, member(X, List), Xs),
	sort(List, LS),
	Xs == LS.
sets(setof-4) :-
	findall(f(X,Y,Z), setof(t,(X=Y;Y=Z;Z=X),_), Fs),
	Fs = [_,_,_],
	Ms = [f(X,X,_),f(_,Y,Y),f(X,_,X)],
	\+ ( member(M, Ms), \+ ( member(F, Fs), F =@= M ) ).
sets(setof-5) :- % Bart Demoen's example
	findall(f(X,Z,A,B,Out), ( Given = [f(X,1),f(2,Z)], bagof(A,member(f(A,B),Given),Out) ), Fs),
	Fs = [_,_],
	Ms = [f(X, Z, A, Z, [2]), f(X, Z, A, 1, [X])],
	\+ ( member(M, Ms), \+ ( member(F, Fs), F =@= M ) ).


		 /*******************************
		 *	       NAME		*
		 *******************************/

atom_handling(name-1) :-
	name(hello, X),
	atom_codes(hello, X).
atom_handling(name-2) :-
	name(V, "5"), V == 5.
atom_handling(name-3) :-
	name(V, "5.0e4"), V =:= 50000.
atom_handling(name-4) :-
	name(V, "5.0e4a"), V == '5.0e4a'.
atom_handling(name-5) :-
	name(V, ""), V == ''.

atom_handling(atom-1) :-
	atom_length('hello', X), X == 5.
atom_handling(concat-1) :-
	atom_concat(gnu, gnat, gnugnat).
atom_handling(concat-2) :-
	atom_concat(X, gnat, gnugnat), X == gnu.
atom_handling(concat-3) :-
	atom_concat(gnu, X, gnugnat), X == gnat.
atom_handling(concat-4) :-
	atom_concat('', X, ''), X == ''.
atom_handling(concat-5) :-
	findall(X-Y, atom_concat(X, Y, 'abc'), Pairs),
	Pairs == [''-abc, a-bc, ab-c, abc-''].
atom_handling(concat-6) :-
	atom_concat(X, a, a),
	X == ''.
atom_handling(concat-7) :-
	atom_concat(X, Y, ''),
	X == '',
	Y == ''.

atom_handling(number-1) :-
	atom_number('42', X), X == 42.
atom_handling(number-2) :-
	atom_number('1.0', X), float(X).
atom_handling(number-3) :-
	atom_number(X, 1.0), X == '1.0'.
atom_handling(number-4) :-
	atom_number(X, 42), X == '42'.
atom_handling(number-5) :-
	A is 1<<42,
	number_codes(A, Codes),
	number_codes(A2, Codes),
	A == A2.

atom_handling(sub_atom-1) :-
	\+ sub_atom(a, _, _, 3, _).
atom_handling(sub_atom-2) :-
	\+ sub_atom(a, _, 3, _, _).
atom_handling(sub_atom-3) :-
	\+ sub_atom(a, 3, _, _, _).
atom_handling(sub_atom-4) :-		% sharing variables
	findall(t(B,L,S), sub_atom(ab, B, L, L, S), List),
	List == [t(0, 1, a), t(2, 0, '')].

atom_handling(current-1) :-
	findall(X, current_atom(X), Atoms),
	maplist(atom, Atoms),
	member(atom, Atoms),
	member(testset, Atoms),
	member('', Atoms),
	member(foobar, Atoms),
	length(Atoms, L),
	L > 100.			% else something is wrong!
atom_handling(complete-1) :-
	'$atom_completions'(stat, List),
	length(List, Len),
	Len > 2.


		 /*******************************
		 *	      STRINGS		*
		 *******************************/

:- dynamic obq/1.
:- current_prolog_flag(back_quotes, Old),
   assertz(obq(Old)).
:- set_prolog_flag(back_quotes, string).

string_handling(sub-1) :-
	\+ sub_string(`HTTP/1.1 404 Not Found`, _, _, _, `OK`).
string_handling(cmp-1) :-
	`hello` == `hello`.

:- retract(obq(Old)),
   set_prolog_flag(back_quotes, Old).

string_handling(atom-1) :-
	atom_string(X, an_atom),
	X == an_atom.
string_handling(list-1) :-
	atom_codes(a_list, Codes),
	atom_string(X, Codes),
	X == a_list.
string_handling(string-1) :-
	atom_string(a_string, String),
	atom_string(X, String),
	X == a_string.


		 /*******************************
		 *	       DYNAMIC		*
		 *******************************/

cpxx.					% for test current_predicate-1
cpxx(_,_).

proc(retractall-1) :-
	forall(foo(A,B), assert(myfoo(A,B))),
	retractall(myfoo(2, _)),
	findall(A-B, myfoo(A,B), L1),
	L1 == [1-a, 3-c, 1-d, 3-f],
	retractall(myfoo(_,_)),
	findall(A-B, myfoo(A,B), L2),
	L2 == [].
proc(retract-1) :-
	forall(foo(A,B), assert(myfoo(A,B))),
	findall(X, retract(myfoo(1, X)), Xs),
	Xs == [a, d],
	forall(retract(myfoo(_,_)), true),
	\+ clause(myfoo(_,_), _).
proc(retract-2) :-
	assert((test(X, Y) :- X is Y + 3)),
	retract((test(A, B) :- Body)),
	Body == (A is B + 3).
proc(retract-3) :-
	assert(myunit(1)),
	assert((myunit(2) :- x)),
	retract((myunit(2) :- X)),
	X == x,
	retractall(myunit(_)).		% cleanup
proc(current_predicate-1) :-
	setof(X, current_predicate(cpxx/X), L), % order is not defined!
	L == [0, 2].
proc(compile_predicate-1) :-		% Bug#152
	cp_one, !, cp_one.
proc(erase-static) :-
	clause(cpxx(_,_), true, Ref),
	catch(erase(Ref), E, true),
	nonvar(E).
proc(current_predicate-2) :-
	catch(current_predicate(foo(bar) : baz/0), E, true),
	E = error(type_error(atom, foo(bar)), _).

cp_one :-
	assert(cp_foo(a)),
	assert(cp_foo(b)),
	cp_foo(_),
	compile_predicates([cp_foo/1]),
	abolish(cp_foo/1).


		 /*******************************
		 *	       CLAUSE		*
		 *******************************/

:- dynamic
	tcl/1.

tcl(a).
tcl(b) :- true.
tcl(c) :- write(hello).
tcl(a(X)) :- b(X).
tcl(x(G)) :- G.
tcl(a(X,X)) :- a(X).
tcl(scut) :- ( a *-> true ; b *-> c ).
tcl(cut) :-  ( a -> true  ; b -> c ).
tcl(vf) :- v(X), X = Y, v(Y).		% B_UNIFY_VF argument ordering

v(_).

mtcl:tcl(a) :- a.
mtcl:tcl(b) :- a, b.
mtcl:(tcl(c) :- a, b).

cl(call-1) :-
	clause(tcl(x(G)), X), X == call(G).
cl(clause-1) :-
	clause(tcl(a), X), X == true.
cl(clause-2) :-
	clause(tcl(b), X), X == true.
cl(clause-3) :-
	clause(tcl(c), X), X == write(hello).
cl(clause-4) :-
	clause(tcl(a(X)), B), B == b(X).
cl(clause-5) :-
	clause(tcl(H), b(a)), H == a(a).
cl(clause-6) :-
	clause(tcl(a(a,X)), B),
	X == a,
	B == a(a).
cl(clause-7) :-
	clause(mtcl:tcl(H), user:a), H == a.
cl(clause-8) :-
	clause(tcl(scut), Body),
	Body == ( a *-> true ; b *-> c ).
cl(clause-9) :-
	clause(tcl(cut), Body),
	Body == ( a -> true ; b -> c ).
cl(clause-10) :-
	clause(tcl(vf), Body),
	Body =@= (v(X), X = Y, v(Y)).


		 /*******************************
		 *	       RECORDS		*
		 *******************************/

mkterm(T) :-
	string_codes(S, "hello"),
	current_prolog_flag(max_tagged_integer, X),
	BigNum is X * 3,
	NegBigNum is -X*5,
	T = term(atom,			% an atom
		 S,			% a string
		 1,			% an integer
		 BigNum,		% large integer
		 -42,			% small negative integer
		 NegBigNum,		% large negative integer
		 3.4,			% a float
		 _,			% a singleton
		 A, A,			% a shared variable
		 [a, list]).		% a list

erase_all(Key) :-
	recorded(Key, _, Ref),
	erase(Ref),
	fail.
erase_all(_).

record(recorda-1) :-
	erase_all(r1),
	mkterm(T0),
	recorda(r1, T0, Ref),
	recorded(r1, T1),
	T0 =@= T1,
	erase(Ref).
record(recorda-2) :-
	erase_all(r2),
	mkterm(T0),
	recorda(r2, T0, Ref),
	recorded(K, T1, Ref),
	K == r2,
	T0 =@= T1,
	erase(Ref).
record(recorda-3) :-
	erase_all(r3),
	\+ current_key(r3),
	recorda(r3, test, Ref),
	current_key(r3),
	erase(Ref).
record(recorda-4) :-
	erase_all(r4),
	recorda(r4, aap, R1),
	recorda(r4, noot, R2),
	recordz(r4, mies, R3),
	findall(X, recorded(r4, X), Xs),
	Xs = [noot, aap, mies],
	erase(R1), erase(R2), erase(R3).
record(recorda-5) :-
	recorda(bla,sign(a,(b,c),d), Ref),
	\+ recorded(bla, sign(_,(B,B),_)),
	\+ (recorded(bla,S),
	    S=sign(_,(B,B),_)),
	erase(Ref).
record(erase-1) :-
	erase_all(r5),
	recorda(r5, aap, R),
	recorda(r5, noot, R2),
	erase(R),
	findall(X, recorded(r5, X), Xs),
	Xs = [noot],
	erase(R2).
record(erase-2) :-
	retractall(a(_)),
	assert(a(1), Ref),
	erase(Ref),
	findall(X, a(X), Xs),
	Xs = [].


		 /*******************************
		 *	    ASSERT/RETRACT	*
		 *******************************/

%	compiler
%
%	This suite tests whether all data-types can be compiled properly
%	and handled by the decompiler.

:- dynamic
	compiler_test/1.

compiler(assert-1) :-
	mkterm(T0),			% verify head compile mode
	assert(compiler_test(T0)),
	compiler_test(T),
	T0 =@= T,
	retractall(compiler_test(_)).
compiler(assert-2) :-
	mkterm(T0),			% verify head compile mode
	assert(compiler_test(T0)),
	compiler_test(T0),
	retractall(compiler_test(_)).
compiler(assert-3) :-			% verify body compile mode
	mkterm(T0),
	assert((compiler_test(X) :- unify(X, T0))),
	compiler_test(T),
	T0 =@= T,
	retractall(compiler_test(_)).
compiler(assert-4) :-
	mkterm(T0),
	assert(compiler_test(T0)),
	retract(compiler_test(T)),
	T0 =@= T.
compiler(assert-5) :-
	mkterm(T0),
	assert(compiler_test(T0)),
	retract(compiler_test(T0)).
compiler(assert-6) :-
	numlist(0, 100, L),
	append(L, [_], A),
	T =.. [x|A],
	assert(T, Ref),
	once(T),
	erase(Ref).
compiler(assert-7) :-
	Body = (a(A), A=[B|C], a(B), a(C)),
	Clause = (at:-Body),
	assert(Clause, Ref),
	clause(_H, BC, Ref),
	BC =@= Body,
	erase(Ref).

unify(X, X).

		 /*******************************
		 *	       FLAGS		*
		 *******************************/

flag(arith-1) :-
	flag(f, Old, 0),
	flag(f, V, V+1),
	flag(f, NV, Old),
	NV == 1.


		 /*******************************
		 *	    UPDATE-VIEW		*
		 *******************************/

:- dynamic
	a/1.

update(assert-1) :-
	retractall(a(_)),
	\+ ( assert(a(1)),
	     assert(a(2)),
	     a(X),
	     assert(a(3)),
	     X = 3
	   ),
	retractall(a(_)).
update(retract-1) :-
	retractall(a(_)),
	(   assert(a(1)),
	    assert(a(2)),
	    retract(a(_)),
	    assert(a(3)),
	    fail
	;   findall(X, a(X), Xs),
	    Xs = [3,3]
	),
	retractall(a(_)).
update(retract-2) :-
	retractall(a(_)),
	assert(a(1)),
	assert(a(2)),
	a(X),
	ignore(retract(a(2))),
	X = 2,
	retractall(a(_)).


		 /*******************************
		 *	       CONTROL		*
		 *******************************/

softcut1(A) :-
	(   between(1, 2, A)
	*-> true
	;   A = 3
	).
softcut2(A) :-
	(   between(3, 2, A)
	*-> true
	;   A = 1
	).

/* c*: tests for handling !
*/

c1 :-
	\+ ( true, !, fail ).

c2 :-
	(   true
	->  !, fail
	;   true
	).
c2.

c3 :-
	\+ (true, !, fail).

c4 :-					% ! in (A, ! -> B) must cut A
	\+ c4_body,
	flag(c4, 1, 0).
c4_body :-
	flag(c4, _, 0),
	(   c4(_), !,
	    flag(c4, X, X+1),
	    fail
	->  writeln('OOPS')
	).

c4(1).
c4(2).
c4(3).

/* test data for variable allocation in control-structures
*/

p(f(a,d)).
p(f(b,c)).

control(softcut-1) :-
	findall(A, softcut1(A), [1,2]).
control(softcut-2) :-
	findall(A, softcut2(A), [1]).
control(cut-1) :-
	c1.
control(cut-2) :-
	\+ c2.
control(cut-3) :-
	c3.
control(cut-4) :-
	c4.
:- style_check(-singleton).
control(not-1) :-			% 2-nd call must generate FIRSTVAR
	( fail ; \+ \+ p(f(X,Y)) ), p(f(X,Y)).
:- style_check(+singleton).
control(not-2) :-			% see comments with compileBody()
	garbage_collect,		% may crash if wrong
	prolog_current_frame(F),
	prolog_frame_attribute(F, argument(4), Y),
	var(Y),				% additional test whether it is reset
	(   fail
	;   \+ A\=A
	).
control(ifthen-1) :-			% Must be the same
	( fail
	; ((p(f(X,Y))->fail;true)->fail;true)
	),
	p(f(X,Y)).


		 /*******************************
		 *	     EXCEPTIONS		*
		 *******************************/

do_exception_1 :-
	unbound(A),
	A.

unbound(_).

rethrow(G) :-
	catch(G, E, throw(E)).

throwit :-
	throw(foo(_)).

catchme :-
	catch(throwit, _, true).

undef :-
	'this is not defined'.
undef.					% avoid last-call optimization for context

tcatch :-
	catch(ierror, E, throw(ok(E))).

ierror :-
	garbage_collect,
	atom_codes(_,_).

exception(call-1) :-
	catch(do_exception_1, E, true),
	error(E, instantiation_error).
exception(call-2) :-
	\+ catch(do_exception_1, _, fail).
exception(call-3) :-
	catch(rethrow(do_exception_1), E, true),
	error(E, instantiation_error).
exception(call-4) :-
	catch(throwit, foo(X), X = a),
	X = a.
exception(call-5) :-
	catch(throwit, _, catchme).
exception(context-1) :-
	catch(functor(_,_,_), E, true),
	error_context(E, functor/3).
exception(context-2) :-
	catch(undef, E, true),
	error_context(E, undef/0).
exception(catch-gc) :-
	catch(tcatch, E, true),
	subsumes_term(ok(error(_,_)), E).


		 /*******************************
		 *	       GC		*
		 *******************************/

make_data(0, []) :- !.
make_data(N, s(X)) :-
	NN is N - 1,
	make_data(NN, X).

gc(gc-1) :-
	garbage_collect.
gc(gc-2) :-			% Beautiful crash.  See compilation of \+
	\+( x(X,2) == x(X,1) ),
	garbage_collect,
	true.
gc(gc-3) :-
	\+ \+ ( gc_data(X),
		garbage_collect,
		X == a
	      ).
gc(gc-4) :-
	catch(_,_,garbage_collect).
gc(gc-5) :-
	G = 25,
	catch(G,_,garbage_collect).
gc(gc-6) :-
	G = 1.25,
	catch(G,_,garbage_collect).
gc(agc-1) :-
	garbage_collect_atoms.
gc(agc-2) :-		% not if concurrent: this is too simple.  There
			% are enough tests for AGC in the rest of the suite.
	(   current_prolog_flag(agc_margin, Margin),
	    Margin > 0,
	    \+ current_prolog_flag(test_concurrent, true)
	->  garbage_collect_atoms,
	    UpTo is Margin*10+400,
	    statistics(agc, AGC0),
	    statistics(agc_gained, Gained0),
	    forall(between(0, UpTo, X), atom_concat(foobar, X, _)),
	    (	between(1, 6, X),
		(   statistics(agc, AGC1),
		    AGC is AGC1 - AGC0,
		    AGC > 5,
		    statistics(agc_gained, Gained1),
		    Gained is Gained1 - Gained0,
		    Gained > AGC*Margin*0.7
		->  true
		;   Time is 0.01*(2^X),
		    sleep(Time),
		    fail
		)
	    ->	true
	    )
	;   true			% no atom-gc
	).

gc_data(a).

		 /*******************************
		 *	INTEGER OVERFLOW	*
		 *******************************/

intoverflow(add-1) :-
	current_prolog_flag(max_integer, MI),
	ToHigh is MI + 10000,
	float(ToHigh).
intoverflow(syntax-1) :-
	(   current_prolog_flag(max_integer, 2147483647)
	->  term_to_atom(X, 2147483648)
	;   current_prolog_flag(max_integer, 9223372036854775807)
	->  term_to_atom(X, 9223372036854775808)
	),
	float(X).
intoverflow(syntax-2) :-
	(   current_prolog_flag(max_integer, 2147483647)
	->  Chars = "41234567891",
	    Float = 41234567891.0
	;   Chars = "9223372036854775900",
	    Float = 9223372036854775900.0
	),
	name(X, Chars),
	float(X),
	X =:= Float.


		 /*******************************
		 *	ATTRIBUTED VARIABLES	*
		 *******************************/

:- dynamic
	woken/2.

test:attr_unify_hook(_Att, _Val).
woken:attr_unify_hook(Att, Var) :-
	assert(woken(Att, Var)).

u_predarg(predarg).
u_termarg(f(termarg)).
u_nil([]).
u_list([a]).
u_args(X, X).

u(predarg, X) :-
	u_predarg(X).
u(termarg, X) :-
	u_termarg(f(X)).
u(nil, X) :-
	u_nil(X).
u(list, X) :-
	u_list(X).
u(unify, X) :-
	X = unify.
u(arith, X) :-
	X is 3.
u(args, X) :-
	u_args(X, args).
u(functor, X) :-
	functor(X, foo, 1).

test_wakeup(How) :-
	freeze(X, Y=ok),
	u(How, X),
	Y == ok.

avar(access-1) :-			% very basic access
	put_attr(X, test, hello),
	get_attr(X, test, H),
	H == hello.
avar(backtrack-1) :-			% test backtracking
	retractall(mark(_)),
	put_attr(X, test, hello),
	(   put_attr(X, test, world),
	    get_attr(X, test, A),
	    A == world,
	    assert(mark(1)),		% point must be reached
	    fail
	;   get_attr(X, test, A),
	    A == hello
	),
	retract(mark(1)).
avar(rec-1) :-
	put_attr(X, test, hello),
	recorda(x, x(X,X), Ref),
	recorded(_, x(A,B), Ref),
	erase(Ref),
	A == B.
avar(rec-2) :-
	put_attr(X, test, hello),
	recorda(x, X, Ref),
	recorded(_, Y, Ref),
	erase(Ref),
	var(Y),
	get_attr(Y, test, A),
	A == hello.
avar(wakeup-1) :-
	test_wakeup(predarg).
avar(wakeup-2) :-
	test_wakeup(termarg).
avar(wakeup-3) :-
	test_wakeup(nil).
avar(wakeup-4) :-
	test_wakeup(unify).
avar(wakeup-5) :-
	test_wakeup(arith).
avar(wakeup-6) :-
	test_wakeup(args).
avar(wakeup-7) :-
	test_wakeup(functor).
avar(type-1) :-
	put_attr(X, test, a),
	attvar(X).
avar(type-2) :-
	put_attr(X, test, a),
	var(X).
avar(type-3) :-
	put_attr(X, test, a),
	\+ nonvar(X).
avar(type-4) :-
	put_attr(X, test, a),
	\+ ground(X).
avar(type-5) :-
	put_attr(X, test, a),
	\+ atomic(X).
avar(term_hash-1) :-
	freeze(X, write(a)),
	term_hash(X, H),
	var(H).
avar(findall-1) :-
	retractall(avar_findall(_)),
	findall(A, freeze(A, assert(avar_findall(A))), L),
	L=[aap],
	retract(avar_findall(X)),
	X == aap.
avar(bagof-1) :-
	CVars = [X1,X2],
	put_attr(X1, test, x),
	put_attr(X2, test, x),
        bagof(CVar, member(CVar,CVars), All),
	All = [C1, C2],
	get_attr(C1, test, x),
	get_attr(C2, test, x).
avar(streq-1) :-
	freeze(X, write(x)), freeze(Y, write(x)), X =@= Y.
avar(streq-2) :-
	freeze(X, write(x)), freeze(Y, write(y)), X \=@= Y.
avar(streq-3) :-
	freeze(X, write(X)), freeze(Y, write(Y)), X =@= Y.
avar(streq-4) :-
	freeze(X, write(X)), freeze(Y, write(_Z)), X \=@= Y.
avar(throw-1) :-
	freeze(X, write(X)),
	T = x(X),
	catch(throw(T), Ex, true),
	Ex =@= T.			% should be ==
avar(throw-2) :-
	freeze(X, write(X)),
	freeze(Y, write(Y)),
	T = x(X,_,Y),
	catch(throw(T), Ex, true),
	Ex =@= T.
avar(order-1) :-			% attributes do not change standard
	_ = foo(A,B),			% order of terms
	(   A @< B
	->  put_attr(A, test, x),
	    A @< B
	;   put_attr(A, test, x),
	    A @> B
	).
avar(nowake-1) :-
	retractall(woken(_,_)),
	put_attr(V, woken, 10),
	\+ (V-a = 10-b),
	\+ woken(_,_).


		 /*******************************
		 *	  GLOBAL VARIABLES	*
		 *******************************/

nogvar(Var) :-
	catch(nb_getval(Var, _), E, true),
	error(E, existence_error(variable, Var)).

gvar(set-1) :-
	nb_setval(gnu, gnat),
	nb_getval(gnu, gnat),
	\+ nb_getval(gnu, x),
	nb_getval(gnu, X),
	X == gnat,
	nb_delete(gnu),
	nogvar(gnu).
gvar(set-2) :-
	nb_setval(gnu, gnat(1)),
	(   b_setval(gnu, gnat(2)),
	    garbage_collect,
	    b_getval(gnu, gnat(2)),
	    fail
	;   b_getval(gnu, gnat(1))
	),
	nb_delete(gnu).
gvar(set-3) :-
	nb_setval(gnu, gnat),
	(   b_setval(gnu, gnat),
	    garbage_collect,
	    b_getval(gnu, gnat),
	    fail
	;   b_getval(gnu, gnat)
	),
	nb_delete(gnu).
gvar(set-4) :-
	(   b_setval(gnu, 1),
	    fail
	;   b_getval(gnu, [])
	),
	nb_delete(gnu).
gvar(avar-1) :-
	freeze(A, fail),
	nb_setval(gvar1, A),
	nb_getval(gvar1, B),
	A =@= B.



		 /*******************************
		 *	     COPY-TERM		*
		 *******************************/

copy_term(rct-1) :-
	copy_term(a, X), X == a.
copy_term(rct-2) :-
	copy_term(X, Y), X \== Y.
copy_term(rct-3) :-
	copy_term(f(a), Y), Y == f(a).
copy_term(rct-4) :-
	copy_term(f(X), Y), Y = f(Z), X \== Z.
copy_term(rct-5) :-
	copy_term(f(X, X), Y), Y = f(A,B), A == B.
copy_term(rct-6) :-
	X = f(X),
	copy_term(X, Y),
	X = Y.
copy_term(ct-1) :-
	T = (A=foo(bar(A), y:x(A, b, c))),
	copy_term(T, B),
	numbervars(B, 0, _),
	\+ ground(T).
copy_term(av-1) :-			% copy attributed variables
	X = foo(V),
	put_attr(V, test, y),
	copy_term(X, Y),
	Y = foo(Arg),
	get_attr(Arg, test, A),
	A == y,
	put_attr(Arg, test, z),
	get_attr(V, test, y).
copy_term(av-2) :-
	X = foo(V,V),
	put_attr(V, test, y),
	copy_term(X, Y),
	Y = foo(A,B),
	A == B,
	get_attr(A, test, y).
copy_term(av-3) :-
	put_attr(X, test, f(X)),
	copy_term(X, Y),
	get_attr(Y, test, A),
	A = f(Z),
	Y == Z.
copy_term(av-4) :-
	G1 = true,
	G2 = (Done = true),
	freeze(X, G1),
	freeze(X, G2),
	copy_term(X, Y),
	X = ok,
	Done == true,
	get_attr(Y, freeze, Att),
	Att = '$and'(user:true, user:(D2=true)),
	var(D2),
	Y = ok,
	D2  == true.
copy_term(reset-1) :-			% reset cycle resetting for shared
	A = [a:b,c:d|_],		% terms.
	copy_term(A,_B),
	A = [a:b,c:d|_].
copy_term(nat-1) :-			% shared variables
	put_attr(X, foo, y),
	T = x(X,X),
	copy_term_nat(T, Y),
	Y = x(A,B), A == B, var(A), \+ attvar(A),
	T = x(C,D), C == D, var(C), get_attr(C, foo, y).
copy_term(nat-2) :-			% cyclic term
	put_attr(X, foo, x),
	T = t(T,X),
	copy_term_nat(T, Y),
	A = t(A,B),
	Y = A,
	var(B), \+ attvar(B),
	arg(2, T, X2),
	get_attr(X2, foo, x).


		 /*******************************
		 *	     TERM-HASH		*
		 *******************************/

term_hash(simple-1) :-
	term_hash(aap, X),
	memberchk(X, [ 9270206,		% little endian
		       16674642		% big endian
		     ]).

term_hash(simple-2) :-			% small int
	term_hash(42, X),
	memberchk(X, [ 12280004,	% little endian
		       9594725		% big endian
		     ]).
term_hash(simple-3) :-			% not tagged int
	term_hash(2000000000, X),
	memberchk(X, [ 13691282,	% little endian
		       10072710		% big endian
		     ]).
term_hash(simple-4) :-
	A is pi,
	term_hash(A, X),
	memberchk(X, [ 15717536,	% little endian
		       14888348		% big endian
		     ]).
term_hash(simple-5) :-
	string_codes(S, "hello world"),
	term_hash(S, 13985775).
term_hash(compound-1) :-
	term_hash(hello(world), X),
	memberchk(X, [ 2512014,		% little endian
		       4285241		% big endian
		     ]).
term_hash(compound-2) :-
	A = x(a),
	term_hash(hello(A, A), X),
	memberchk(X, [ 6171734,		% little endian
		       13034251		% big endian
		     ]).
term_hash(compound-3) :-
	term_hash(hello(x(a), x(a)), X),
	memberchk(X, [ 6171734,		% little endian
		       13034251		% big endian
		     ]).


		 /*******************************
		 *    BIG TERMS, ATOM-TO-TERM	*
		 *******************************/

s(0, 0) :- !.
s(N, s(S)) :-
	NN is N - 1,
	s(NN, S).

termtest(N) :-
	s(N, S),
	term_to_atom(S, A),
	atom_length(A, L),
	L =:= 3*N+1,
	atom_to_term(A, S2, []),
	S == S2.

term_atom(term_to_atom-1) :-
	termtest(10).
term_atom(term_to_atom-2) :-
	termtest(1000).


		 /*******************************
		 *	        OS		*
		 *******************************/

os(getenv-1) :-
	getenv('PATH', _).		% should be around on most OSes
os(setenv-1) :-
	setenv(pltestsetenv, yes),
	getenv(pltestsetenv, X),
	X == yes.


		 /*******************************
		 *		I/O		*
		 *******************************/

io(tell-1) :-
	tell(test_x),
	format('~q.~n', [a]),
	tell(test_y),
	format('~q.~n', [b]),
	tell(test_x),
	format('~q.~n', [c]),
	told,
	tell(test_y),
	told,
	read_file_to_terms(test_x, [a,c], []),
	read_file_to_terms(test_y, [b], []),
	delete_file(test_x),
	delete_file(test_y),
	\+ stream_property(_, file_name(test_x)),
	\+ stream_property(_, file_name(test_y)).

io(tell-2) :-
	current_output(OrgOut),
	open_null_stream(Out),
	set_output(Out),
	write(Out, x),
	telling(Old), tell(test_y),
	format('~q.~n', [b]),
	told, tell(Old),
	write(Out, y),
	flush_output(Out),
	character_count(Out, 2),
	close(Out),
	set_output(OrgOut),
	read_file_to_terms(test_y, [b], []),
	delete_file(test_y).


		 /*******************************
		 *	       POPEN		*
		 *******************************/

popen(pwd-1) :-
	(   current_prolog_flag(windows, true)
	->  Command = 'cmd /c cd'
	;   Command = pwd
	),
	open(pipe(Command), read, Fd),
	collect_line(Fd, String),
	close(Fd),
	atom_codes(Pwd, String),
	same_file(Pwd, '.').
popen(cat-1) :-
	(   current_prolog_flag(windows, true)
	->  true		% there is *no* cmd.exe command like cat!?
	;   File = 'pltest.txt',
	    Text = 'Hello World',
	    Cmd = cat,
	    atomic_list_concat([Cmd, ' > ', File], Command),
	    open(pipe(Command), write, Fd),
	    format(Fd, '~w', [Text]),
	    close(Fd),
	    open(File, read, Fd2),
	    collect_data(Fd2, String),
	    close(Fd2),
	    delete_file(File),
	    atom_codes(A, String),
	    A == Text
	).
popen(cat-2) :-
	(   current_prolog_flag(windows, true)
	->  Cmd = 'cmd /c rem true'
	;   Cmd = true
	),
	open(pipe(Cmd), write, Pipe),
	catch(forall(between(1, 10000, _), format(Pipe, '0123456789~n', [])),
	      E,
	      true),
	catch(close(Pipe), _, true),	% ???
	(   var(E)
	->  format(user_error, 'No exception?~n', []),
	    fail
					% if signalling is enabled
	;   E = error(signal(pipe, _), context(copy_stream_data/2, _))
	->  true
					% otherwise
	;   E = error(io_error(write, _), context(_, 'Broken pipe'))
	->  true
	;   E = error(io_error(write, _), _),
	    current_prolog_flag(windows, true)
	->  true
	;   format(user_error, 'Wrong exception: ~p~n', [E]),
	    fail
	).

collect_line(Fd, String) :-
	get0(Fd, C0),
	collect_line(C0, Fd, String).

collect_line(-1, _, []) :- !.
collect_line(10, _, []) :- !.
collect_line(13, _, []) :- !.
collect_line(C, Fd, [C|T]) :-
	get0(Fd, C2),
	collect_line(C2, Fd, T).

collect_data(Fd, String) :-
	get0(Fd, C0),
	collect_data(C0, Fd, String).

collect_data(-1, _, []) :- !.
collect_data(C, Fd, [C|T]) :-
	get0(Fd, C2),
	collect_data(C2, Fd, T).


		 /*******************************
		 *	      TIMEOUT		*
		 *******************************/

timeout(pipe-1) :-
	(   current_prolog_flag(pipe, true),
	    \+ current_prolog_flag(windows, true) % cannot wait on pipes
	->  open(pipe('echo + && sleep 1 && echo xx.'), read, In,
		 [ bom(false)
		 ]),
	    set_stream(In, timeout(0.2)),
	    wait_for_input([In], [In], infinite),
	    catch(read(In, Term1), E1, true),
	    (	nonvar(E1),
		E1 = error(timeout_error(read, _), _)
	    ->	wait_for_input([In], [In], infinite),
		catch(read(In, Term), E2, true),
		(   var(E2)
		->  (   Term == xx
		    ->	close(In)
		    ;	format(user_error, 'Term == ~q~n', [Term]),
			true
		    )
		;   format(user_error, 'E2 == ~q~n', [E2]),
		    fail
		)
	    ;   var(E1)
	    ->	(   Term1 == (+ xx)
		->  close(In),
		    format(user_error,
			   'timeout(pipe-1): ~q (machine heavy loaded?)~n',
			   [Term1])
		;   format(user_error,
			   'var(E1) && Term == ~q~n', [Term1]),
		    fail
		)
	    ;	format(user_error, 'E1 == ~q~n', [E1]),
		fail
	    )
	;   true
	).


		 /*******************************
		 *	      FILES		*
		 *******************************/

:- dynamic
	testfile/1.
:- prolog_load_context(file, File),
   assert(testfile(File)).

root(Root) :-
	(   current_prolog_flag(windows, true)
	->  atom_concat(L, ':/', Root),
	    char_type(L, alpha)
	;   Root == (/)
	).

file(canonicalise-1) :-
	absolute_file_name('/foo/..', X),
	root(X).
file(canonicalise-2) :-
	absolute_file_name('/foo/../..', X),
	root(X).
file(canonicalise-3) :-
	absolute_file_name('/foo/../../..', X),
	root(X).
file(canonicalise-4) :-
	absolute_file_name('/foo/../../../', X),
	root(X).
file(exists-1) :-
	\+ exists_file(foobar26).
file(exists-2) :-
	testfile(File),
	exists_file(File).
file(exists-3) :-
	\+ exists_file('.').
file(dir-1) :-
	exists_directory('.').
file(dir-2) :-
	testfile(File),
	\+ exists_directory(File).
file(cwd-1) :-
	working_directory(CWD, CWD),
	exists_directory(CWD),
	same_file(CWD, '.').
file(absfile-2) :-			% canonicaliseDir() caching issues
	X = 'pl-test-x',
	Y = 'pl-test-y',
	atom_concat(X, '/file', XF),
	atom_concat(Y, '/file', YF),
	make_directory(X),
	touch(XF),
	absolute_file_name(XF, Abs1),
	atom_concat(_, XF, Abs1),
	delete_file(XF),
	delete_directory(X),
	make_directory(Y),
	touch(YF),
	absolute_file_name(YF, Abs2),
	delete_file(YF),
	delete_directory(Y),
	atom_concat(_, YF, Abs2).
file(ext-2) :-
	\+ file_name_extension(foo, _, 'bar.pl').	% Bug#69
file(open-1) :-
	catch(open(foobar, read, _, [lock(qqq)]), E, true),
	E =@= error(domain_error(lock, qqq), context(system:open/4, _)).


touch(File) :-
	open(File, update, Out),
	close(Out).

		 /*******************************
		 *	UNICODE FILENAMES	*
		 *******************************/

unicode_file(mkdir-1) :-			% create Cyrillic directory
	atom_codes(Dir, [1074, 1086, 1079, 1076, 1091, 1093, 1072]),
	catch(delete_directory(Dir), _, true),
	make_directory(Dir),
	exists_directory(Dir),
	working_directory(Old, Dir),
	working_directory(O2, '..'),
	same_file(Old, '.'),
	same_file(O2, Dir),
	delete_directory(Dir).
unicode_file(file-1) :-				% create Cyrillic file
	atom_codes(File, [1074, 1086, 1079, 1076, 1091, 1093, 1072]),
	Term = hello(world),
	catch(delete_file(File), _, true),
	open(File, write, Out),
	format(Out, '~q.~n', [Term]),
	close(Out),
	exists_file(File),
	open(File, read, In),
	read(In, Read),
	close(In),
	Read =@= Term,
	delete_file(File).
unicode_file(absfile-1) :-
	atom_codes(File, [1074, 1086, 1079, 1076, 1091, 1093, 1072]),
	absolute_file_name(File, Path),
	file_directory_name(Path, Dir),
	same_file(Dir, '.'),
	file_base_name(Path, Base),
	Base == File.
unicode_file(ext-1) :-
	atom_codes(File, [1074, 1086, 1079, 1076, 0'., 1091, 1093, 1072]),
	file_name_extension(Base, Ext, File),
	atom_codes(Base, [1074, 1086, 1079, 1076]),
	atom_codes(Ext, [1091, 1093, 1072]).


		 /*******************************
		 *		SEEK		*
		 *******************************/

seek(write-1) :-
	tmp_file(seek, File),
	open(File, write, S, [type(binary)]),
	Max = 999,
	forall(between(0, Max, _),
	       format(S, '1234567890~n', [])),
	forall(between(0, Max, N),
	       (   Pos is N * 11 + 6,
		   seek(S, Pos, bof, _),
		   format(S, 'x', [])
	       )),
	close(S),

	open(File, read, In, [type(binary)]),
	atom_codes('123456x890\n', Bytes),
	forall(between(0, Max, N),
	       must_read(Bytes, In)),
	close(In),
	delete_file(File).

must_read([], _) :- !.
must_read([H|T], In) :-
	get_byte(In, Byte),
	H == Byte,
	must_read(T, In).



		 /*******************************
		 *	   CODE/CHAR-TYPE	*
		 *******************************/

ctype(code_type-1) :-
	code_type(97, to_lower(97)),
	code_type(97, to_lower(65)).
ctype(code_type-2) :-
	findall(X, code_type(X, lower), Lower),
	string_codes("abcdefghijklmnopqrstuvwxyz", AZ),
	subset(AZ, Lower).
ctype(code_type-3) :-
	code_type(48, digit(0)).
ctype(code_type-4) :-
	code_type(X, digit(0)),
	X == 48.
ctype(code_type-5) :-
	code_type(48, digit(W)),
	W == 0.
ctype(code_type-6) :-
	\+ code_type(0, quote).

		 /*******************************
		 *	       WCTYPE		*
		 *******************************/

wctype(code_type-6) :-
	char_code(X, 1080),
	code_type(X, alnum).


		 /*******************************
		 *	      CONSULT		*
		 *******************************/

mk_include :-
	open('test_included.pl', write, Out1),
	format(Out1, ':- dynamic foo/1.\n', []),
	close(Out1),

	open('test_include.pl', write, Out2),
	format(Out2, ':- include(test_included).\n', []),
	format(Out2, 'foo(a).\n', []),
	close(Out2).

load_program(include-1) :-
	mk_include,
	abolish(foo, 1),
	load_files(test_include, [silent(true)]),
	assert(foo(b)),
	findall(X, retract(foo(X)), [a,b]),
	delete_file('test_included.pl'),
	delete_file('test_include.pl').


		 /*******************************
		 *	    THREADING		*
		 *******************************/

:- dynamic
	th_data/1,
        at_exit_called/0.

at_exit_work :-
        thread_at_exit(assert(at_exit_called)),
        thread_exit(true).

th_do_something :-
	forall(between(1, 5, X),
	       assert(th_data(X))).
th_check_done :-
	findall(X, retract(th_data(X)), [1,2,3,4,5]).

thread(queue-1) :-			% crash if no queue exists?
	findall(Q, message_queue_property(Q,_), _).
thread(join-1) :-
	thread_create(th_do_something, Id, []),
	thread_join(Id, Exit),
	Exit == true,
	th_check_done.
thread(message-1) :-
	thread_self(Me),
	thread_create(thread_send_message(Me, hello), Id, []),
	thread_get_message(hello),
	thread_join(Id, true).
thread(signal-1) :-
	thread_create((repeat, fail), Id, []),
	thread_signal(Id, throw(stopit)),
	thread_join(Id, Exit),
	Exit == exception(stopit).
thread(at_exit-1) :-
        retractall(at_exit_called),
        thread_create(at_exit_work, Id, []),
        thread_join(Id, exited(true)),
	retract(at_exit_called).
thread(status-1) :-
	thread_create(true, Id, []),
	between(0, 100, _),
	sleep(0.01),
	thread_property(Id2, status(Status)),
	Id2 == Id,
	Status == true, !,
	thread_join(Id2, _).
thread(create_error-1) :-
	catch(thread_create(true, _, [local(a)]), E, true),
	E = error(type_error(integer, a), _).


		 /*******************************
		 *	 MUTEX HANDLING		*
		 *******************************/

mutex(trylock-1) :-
	gensym(mutex, Mutex),
	thread_self(Main),
	thread_create(( mutex_lock(Mutex),
			thread_send_message(Main, locked),
			thread_get_message(_),
			mutex_unlock(Mutex)),
		      Id, []),
	thread_get_message(locked),
	\+ mutex_trylock(Mutex),
	thread_send_message(Id, done),
	thread_join(Id, true),
	mutex_destroy(Mutex).
mutex(unlock-1) :-
	gensym(mutex, Mutex),
	mutex_lock(Mutex),
	mutex_unlock(Mutex),
	catch(mutex_unlock(Mutex), E, true),
	E == error(permission_error(unlock, mutex, Mutex),
		   context(mutex_unlock/1, 'not locked')),
	mutex_destroy(Mutex).
mutex(destroy-1) :-
	gensym(mutex, Mutex),
	mutex_create(Mutex),
	mutex_destroy(Mutex).


		 /*******************************
		 *	      SCRIPTS		*
		 *******************************/


:- dynamic
	script_dir/1.

set_script_dir :-
	script_dir(_), !.
set_script_dir :-
	find_script_dir(Dir),
	assert(script_dir(Dir)).

find_script_dir(Dir) :-
	prolog_load_context(file, File),
	follow_links(File, RealFile),
	file_directory_name(RealFile, Dir).

follow_links(File, RealFile) :-
	read_link(File, _, RealFile), !.
follow_links(File, File).


:- set_script_dir.

run_test_script(Script) :-
	file_base_name(Script, Base),
	file_name_extension(Pred, _, Base),
	load_files(Script, [silent(true), if(changed)]),
	(   current_prolog_flag(verbose, normal)
	->  format(user_error, '(~w)', [Base]), flush_output
	;   true
	),
	call_test(Pred, script).

run_test_scripts(Directory) :-
	(   script_dir(ScriptDir),
	    atomic_list_concat([ScriptDir, /, Directory], Dir),
	    exists_directory(Dir)
	->  true
	;   Dir = Directory
	),
	atom_concat(Dir, '/*.pl', Pattern),
	expand_file_name(Pattern, Files),
	file_base_name(Dir, BaseDir),
	format(user_error, '~NRunning scripts from ~w ', [BaseDir]),
	run_scripts(Files),
	format(user_error, ' done~n', []).

run_scripts([]).
run_scripts([H|T]) :-
	(   catch(run_test_script(H), Except, true)
	->  (   var(Except)
	    ->  put_ok
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(H, Reason)),
		put_blocked
	    ;   script_failed(H, Except)
	    )
	;   script_failed(H, fail)
	),
	run_scripts(T).

script_failed(File, fail) :-
	format(user_error, '~NScript ~w failed~n', [File]),
	assert(failed(script(File))).
script_failed(File, Except) :-
	message_to_string(Except, Error),
	format(user_error, '~NScript ~w failed: ~w~n', [File, Error]),
	assert(failed(script(File))).


		 /*******************************
		 *        TEST MAIN-LOOP	*
		 *******************************/

testset(syntax).
testset(write_test).
testset(format_test).
testset(unify).
testset(occurs_check).
testset(unifiable).
testset(arithmetic).
testset(arithmetic_functions).
testset(floattest).
testset(gmp) :-
	current_prolog_flag(bounded, false).
testset(chars).
testset(wchars).
testset(depth_limit) :-
	current_predicate(_, user:call_with_depth_limit(_,_,_)).
testset(type_test).
testset(meta).
testset(avar).
testset(gvar).
testset(copy_term).
testset(term_hash).
testset(cyclic).
testset(cleanup).
testset(term).
testset(list).
testset(sets).
testset(atom_handling).
testset(string_handling).
testset(proc).
testset(cl).
testset(record).
testset(compiler).
testset(flag).
testset(update).
testset(gc).
testset(intoverflow) :-
	current_prolog_flag(iso, false),
	current_prolog_flag(max_integer, _).
testset(control).
testset(exception).
testset(term_atom).
testset(os).
testset(io).
testset(popen) :-
	current_prolog_flag(pipe, true).
testset(timeout).
testset(file).
testset(unicode_file) :-
	unicode_file_locale.
testset(seek).
testset(load_program).
testset(ctype).
testset(wctype) :-
	wide_character_types.
testset(thread) :-
	current_prolog_flag(threads, true).
testset(mutex) :-
	current_prolog_flag(threads, true).

%	unicode_file_locale/0
%
%	True if out filesystem can   handle Unicode filenames. Difficult
%	to have a good test.

unicode_file_locale :-
	current_prolog_flag(encoding, utf8), !.
unicode_file_locale :-
	catch(file_name_extension(_,_,[1050]), E, true),
	(   var(E)
	->  true
	;   E \= error(representation_error(encoding), _)
	).

%%	wide_character_types
%
%	True if the  character  classification   routines  work  on wide
%	characters. Hard to say when this is  the case. On some machines
%	the wide character versions always work,  on others only for the
%	codepages covered by the locale.

wide_character_types :-
	current_prolog_flag(encoding, utf8), !.

%%	testdir(Dir)
%
%	Enumerate directories holding tests.

testdir('Tests/unprotected').
testdir('Tests/core').
testdir('Tests/attvar').
testdir('Tests/library').
testdir('Tests/charset').
testdir('Tests/eclipse').
testdir('Tests/clp').
testdir('Tests/GC').
testdir('Tests/thread') :-
	current_prolog_flag(threads, true).
testdir('Tests/save').

:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	forall(testset(Set), runtest(Set)),
	scripts,
	garbage_collect,
	garbage_collect_atoms,
	trim_stacks,
	statistics,
	report_blocked,
	report_failed.

scripts :-
	forall(testdir(Dir), run_test_scripts(Dir)).


report_blocked :-
	findall(Head-Reason, blocked(Head, Reason), L),
	(   L \== []
        ->  format(user_error, '~NThe following tests are blocked:~n', []),
	    (	member(Head-Reason, L),
		format(user_error, '    ~p~t~40|~w~n', [Head, Reason]),
		fail
	    ;	true
	    )
        ;   true
	).
report_failed :-
	findall(X, failed(X), L),
	length(L, Len),
	(   Len > 0
        ->  format(user_error, '~N*** ~w tests failed ***~n', [Len]),
	    fail
        ;   format(user_error, '~NAll tests passed~n', [])
	).

%%	call_test(:Goal, +Line)
%
%	Call the actual test. If dmalloc/3 is provided, call through
%	this leak-detection hook.
%
%	@see test/dmalloc.pl

:- meta_predicate
	call_test(0, +).

:- if(current_predicate(dmalloc/3)).
call_test(Goal, script) :- !,
	dmalloc((Goal->true), '*** Script ~w ***', [Goal]).
call_test(Goal, Line) :-
	dmalloc((Goal->true), '*** Line ~d: ~p ***', [Line, Goal]).
:- elif(current_prolog_flag(test_concurrent, true)).
call_test(Goal, _Line) :-
	test_name(Goal, Name),
	with_mutex(Name, Goal).

test_name(M:G, Name) :- !,
	functor(G, Pred, Arity),
	format(atom(Name), 'test ~w:~w/~d', [M,Pred,Arity]).
test_name(G, Name) :- !,
	functor(G, Pred, Arity),
	format(atom(Name), 'test ~w/~d', [Pred,Arity]).
:- else.
call_test(Goal, _Line) :-
	Goal, !.
:- endif.

runtest(Name) :-
	format(user_error, '~NRunning test set "~w" ', [Name]),
	functor(Head, Name, 1),
	findall(Head-R, nth_clause_head(Head, R), Heads),
	unique_heads(Heads),
	member(Head-R, Heads),
	clause_property(R, line_count(Line)),
	(   current_prolog_flag(verbose, normal)
	->  format(user_error, '(~w)', [Line])
	;   true
	),
	(   catch(call_test(Head, Line), Except, true)
	->  (   var(Except)
	    ->  put_ok
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(Head, Reason)),
		put_blocked
	    ;   test_failed(R, Except)
	    )
	;   test_failed(R, fail)
	),
	fail.
runtest(_) :-
	format(user_error, ' done.~n', []).

nth_clause_head(Head, R) :-
	nth_clause(Head, _N, R),
	clause(Head, _, R).

unique_heads(Heads) :-
	keysort(Heads, Sorted),
	check_uniqye(Sorted).

check_uniqye([]).
check_uniqye([Head-R1,Head-R2|T]) :- !,
	clause_property(R1, line_count(Line1)),
	clause_property(R1, file(File1)),
	clause_property(R2, line_count(Line2)),
	clause_property(R2, file(File2)),
	format('~N~w:~d: test ~w duplicated at ~w:~d~n',
	       [File2, Line2, Head, File1, Line1]),
	check_uniqye([Head-R1|T]).
check_uniqye([_|T]) :-
	check_uniqye(T).


:- if(current_prolog_flag(test_concurrent, true)).
put_ok :-
	thread_self(Me),
	atom_concat(tester_, Id, Me), !,
	write(user_error, Id).
:- endif.
put_ok :-
	write(user_error, .).

put_blocked :-
	write(user_error, !).

test_failed(R, Except) :-
	clause(Head, _, R),
	functor(Head, Name, 1),
	arg(1, Head, TestName),
	clause_property(R, line_count(Line)),
	clause_property(R, file(File)),
	(   Except == fail
	->  format(user_error, '~N~w:~d: Test ~w(~w) failed~n',
		   [File, Line, Name, TestName])
	;   message_to_string(Except, Error),
	    format(user_error, '~N~w:~d: Test ~w(~w):~n~t~8|ERROR: ~w~n',
		   [File, Line, Name, TestName, Error])
	),
	assert(failed(Head)).

blocked(Reason) :-
	throw(blocked(Reason)).


%%	error(+Exception, +Expected)
%
%	Check whether the correct exception  is thrown, disregarding the
%	2nd context argument.

error(error(Ex, _Ctx), Expected) :-
	subsumes_term(Expected, Ex), !.
error(error(Ex, _Ctx), Expected) :-
	format(user_error,
	       '~NWrong exception: ~p (expected ~p)~n', [Ex, Expected]),
	fail.

error_context(error(_, context(Pred, _)), Pred) :- !.
error_context(error(_, context(Module:Pred, _)), Pred) :-
	hidden_module(Module), !.
error_context(Error, _Pred) :-
	format(user_error, 'Wrong error context: ~q~n', [Error]),
	fail.

hidden_module(user) :- !.
hidden_module(system) :- !.
hidden_module(M) :-
	sub_atom(M, 0, _, _, $).
