%	mp.pl
%
%	Provide additional predicates for MP arithmetic in Prolog
%
%	Copyright (c) 1999 Robert A. van Engelen, Florida State University
%	engelen@cs.fsu.edu. All rights reserved.

:- module(mp,
	[ (mp_is)/2
	, mp_compare/3
	, mp_sort/2
	, mp_merge/3
	, mp_setunion/3
	, mp_setint/3
	, mp_setdiff/3
	, mp_addel/3
	, mp_delel/3
	, mp_round/2
	, mp_bernoulli/2
	, mp_tdiv/3
	, mp_fdiv/3
	, mp_cdiv/3
	, mp_trem/3
	, mp_frem/3
	, mp_crem/3
	, mp_num/2
	, mp_den/2
	, mp_re/2
	, mp_im/2
	, mp_conj/2
	, mp_lcm/3
	, mp_min/3
	, mp_max/3
	, mp_sin/2
	, mp_cos/2
	, mp_tan/2
	, mp_asin/2
	, mp_acos/2
	, mp_atan/2
	, mp_atan/3
	, mp_xor/3
	, mp_eqv/3
	, mp_exp/2
	, mp_log/2
	, mp_log10/2
	, mp_power/3
	, mp_atom/1
	, mp_atomic/1
	, mp_number/1
	, mp_integer/1
	, mp_rational/1
	, mp_float/1
	, mp_complex/1
	, mp_compound/1
	]).

load_mp :-
	current_predicate(_, user:mp_term(_,_,_,_)), !.
load_mp :-
	getenv('PLLD', true), !.
load_mp :-
	load_foreign_library(user:foreign(mp), mp_install).

:- initialization
	load_mp.

:- style_check(+dollar).
:- style_check(+string).

:- multifile
	user:portray/1.
:- dynamic
	user:portray/1.

print_n(Z) :- string(Z), !, format('''~w''', [Z]).
print_n(Z) :- write(Z).

print_q(N/D) :- string(N), !, format('''~w''/''~w''', [N, D]).
print_q(Q) :- write(Q).

print_c(complex(R, I)) :- string(R), !, format('complex(''~w'',''~w'')', [R, I]).
print_c(C) :- write(C).

user:portray(X) :- portray(X).

portray($mpz(X))    :- mp_term($mpz(X), 10, 0, Z), !, print_n(Z).
portray($mpf(X))    :- mp_getprec($mpf(X), P), P =< 64, mp_f(_, $mpf(X)), !, mp_term($mpf(X), 10, 0, F), !, write(F).
portray($mpf(X))    :- mp_term($mpf(X), 10, 0, F), !, print_n(F).
portray($mpq(X, Y)) :- mp_term($mpq(X, Y), 10, 0, Q), !, print_q(Q).
portray($mpc(X, Y)) :- mp_term($mpc(X, Y), 10, 0, C), !, print_c(C).

:-		current_predicate(_, user:current_prolog_flag(_,_)),
		op(700, xfx, user:mp_is),
		op(500, yfx, user:eqv)
	;	op(700, xfx, mp_is),
		op(500, yfx, eqv).

%	-N mp_is +X
%	Evaluate expression X and return value in N.
%	This infix predicate mimics is/2.
%	Fails silently on (type) errors in expression X.
%
%	Prolog integers and floats are automatically converted to MP integers.
%	Long integers and floats with long mantissas can be represented by
%	strings (don't forget to set :-style_check(+string). to enter strings
%	in SWI-Prolog). Rationals are created by dividing two integers (e.g.
%	Q mp_is 4/7) and complex numbers can be created by `i' (e.g.
%	C mp_is 2+3*i). The real and imaginary parts of complex numbers are
%	always floats.
%
%	Taylor series expansions for trigonometric and logarithmic functions
%	are only computed when the default precision is higher than 64. When the
%	precision is less, the standard Prolog functions are called for speed.
%
%	For comparing values, use mp_cmp(N1, N2, R) where
%	R is (<) if N1 < N2
%	R is (=) if N1 = N2
%	R is (>) if N1 > N2
%
%	For comparing terms with MP numbers, use mp_compare(N1, N2, R),
%	because e.g. X @< Y may fail due to the internal MP representation.
%
%	Unification of two equal MP numbers should succeed. However, due to
%	numerical roundoff in floats, it is better to use mp_cmp/3.
%
%	Op		Description
%	--		-----------
%	-N		negation
%	N+N		addition
%	N-N		subtraction
%	N*N		multiplication
%	N/N		division (gives rational on integer arguments)
%	Z//Z		integer division (result truncated)
%	Z mod Z		modulo
%	Z rem Z		remainder of integer division
%	N<<Z		left shift
%	N>>Z		right shift
%	N^N		power
%	N**N		power
%	\Z		bitwise complement
%	Z\/Z		bitwise or
%	Z/\Z		bitwise and
%	Z xor Z		bitwise xor
%	Z eqv Z		bitwise eqv
%	abs(N)		abs value (magnitude of complex number)
%	acos(F)		arccos (not implemented in MP yet)
%	asin(F)		arcsin (not implemented in MP yet)
%	atan(F)		arctan (not implemented in MP yet)
%	atan(F, F)	atan(X, Y) = atan(X/Y) (not implemented in MP yet)
%	bin(Z, Z)	binomial
%	bernoulli(Z)	Bernoulli number
%	cdiv(Z, Z)	cdiv(X, Y) = ceil(X/Y)
%	crem(Z, Z)	crem(X, Y) = remainder of ceil(X/Y)
%	ceil(N)		ceiling
%	ceiling(N)	ceiling
%	clrbit(Z, Z)	clrbit(X, Y) clears bit #Y of X
%	complex(F, F)	complex(R, I) = R+I*i
%	conj(C)		complex conjugate
%	cos(F)		cosine
%	den(Q)		denominator of rational
%	exp(N)		e^N
%	fac(Z)		factorial
%	fib(Z)		Fibonacci number
%	float(N)	convert to float
%	floor(N)	floor
%	fdiv(Z, Z)	fdiv(X, Y) = floor(X/Y)
%	frem(Z, Z)	frem(X, Y) = remainder of floor(X/Y)
%	gcd(Z, Z)	GCD
%	im(C)		imaginary part
%	integer(N)	convert to integer
%	jacobi(Z, Z)	Jacobi
%	lcm(Z, Z)	LCM
%	legendre(Z, Z)	Legendre
%	ln(F)		natural logarithm
%	log(F)		natural logarithm
%	log10(F)	10-base logarithm
%	max(N, N)	maximum value (real parts of complex are compared)
%	min(N, N)	minumum value (real parts of complex are compared)
%	num(Q)		numerator of rational
%	prec(N)		get precedence of number (in bits)
%	prec(Z, X)	expression X is evaluated with precision Z (in bits)
%	random(Z)	random number
%	re(X)		real part of complex
%	reldiff(F, F)	relative difference between two floats
%	round(N)	rounding
%	setbit(Z, Z)	setbit(X, Y) sets bit #Y of X
%	sign(N)		sign (real part of complex number)
%	sin(F)		sine
%	sqrt(N)		square root
%	tan(F)		tangent
%	trem(Z, Z)	trem(X, Y) = remainder of X//Y
%			(this is sometimes incorrectly called `mod')
%	trunc(N)	truncate
%	truncate(X)	truncate
%	e		natural number
%	pi		pi (100 digits)
%	i		sqrt(-1)
%
%	where
%
%	Z is integer
%	Q is rational
%	F is float
%	C is complex
%	N is integer, rational, float, or complex

N mp_is \ X           :- !, N1 mp_is X, mp_not(N1, N).
N mp_is - X           :- !, N1 mp_is X, mp_neg(N1, N).
N mp_is X \/  Y       :- !, N1 mp_is X, N2 mp_is Y, mp_or(N1, N2, N).
N mp_is X /\  Y       :- !, N1 mp_is X, N2 mp_is Y, mp_and(N1, N2, N).
N mp_is X xor Y       :- !, N1 mp_is X, N2 mp_is Y, mp_xor(N1, N2, N).
N mp_is X eqv Y       :- !, N1 mp_is X, N2 mp_is Y, mp_eqv(N1, N2, N).
N mp_is X +   Y       :- !, N1 mp_is X, N2 mp_is Y, mp_add(N1, N2, N).
N mp_is X -   Y       :- !, N1 mp_is X, N2 mp_is Y, mp_sub(N1, N2, N).
N mp_is X *   Y       :- !, N1 mp_is X, N2 mp_is Y, mp_mul(N1, N2, N).
N mp_is X /   Y       :- !, N1 mp_is X, N2 mp_is Y, mp_div(N1, N2, N).
N mp_is X //  Y       :- !, N1 mp_is X, N2 mp_is Y, mp_tdiv(N1, N2, N).
N mp_is X rem Y       :- !, N1 mp_is X, N2 mp_is Y, mp_trem(N1, N2, N3), mp_f(N3, N4), mp_div(N4, N2, N).
N mp_is X mod Y       :- !, N1 mp_is X, N2 mp_is Y, mp_mod(N1, N2, N).
N mp_is X <<  Y       :- !, N1 mp_is X, N2 mp_is Y, mp_lsh(N1, N2, N).
N mp_is X >>  Y       :- !, N1 mp_is X, N2 mp_is Y, mp_rsh(N1, N2, N).
N mp_is e ^   X       :- !, N1 mp_is X, mp_exp(N1, N).
N mp_is X ^   Y       :- !, N1 mp_is X, N2 mp_is Y, mp_power(N1, N2, N).
N mp_is X **  Y       :- !, N mp_is X ^ Y.
N mp_is abs(X)        :- !, N1 mp_is X, mp_abs(N1, N).
N mp_is acos(X)       :- !, N1 mp_is X, mp_acos(N1, N).
N mp_is asin(X)       :- !, N1 mp_is X, mp_asin(N1, N).
N mp_is atan(X)       :- !, N1 mp_is X, mp_atan(N1, N).
N mp_is atan(X, Y)    :- !, N1 mp_is X, N2 mp_is Y, mp_atan(N1, N2, N).
N mp_is bin(X, Y)     :- !, N1 mp_is X, N2 mp_is Y, mp_bin(N1, N2, N).
N mp_is bernoulli(X)  :- !, N1 mp_is X, mp_bernoulli(N1, N).
N mp_is cdiv(X, Y)    :- !, N1 mp_is X, N2 mp_is Y, mp_cdiv(N1, N2, N).
N mp_is crem(X, Y)    :- !, N1 mp_is X, N2 mp_is Y, mp_crem(N1, N2, N).
N mp_is ceil(X)       :- !, N1 mp_is X, mp_ceil(N1, N).
N mp_is ceiling(X)    :- !, N1 mp_is X, mp_ceil(N1, N).
N mp_is clrbit(X, Y)  :- !, N1 mp_is X, N2 mp_is Y, mp_clrbit(N1, N2, N).
N mp_is conj(X)       :- !, N1 mp_is X, mp_conj(N1, N).
N mp_is cos(X)        :- !, N1 mp_is X, mp_cos(N1, N).
N mp_is den(X)        :- !, N1 mp_is X, mp_den(N1, N).
N mp_is exp(X)        :- !, N1 mp_is X, mp_exp(N1, N).
N mp_is fac(X)        :- !, N1 mp_is X, mp_fac(N1, N).
N mp_is fib(X)        :- !, N1 mp_is X, mp_fib(N1, N).
N mp_is float(X)      :- !, N1 mp_is X, mp_f(N1, N).
N mp_is floor(X)      :- !, N1 mp_is X, mp_floor(N1, N).
N mp_is fdiv(X, Y)    :- !, N1 mp_is X, N2 mp_is Y, mp_fdiv(N1, N2, N).
N mp_is frem(X, Y)    :- !, N1 mp_is X, N2 mp_is Y, mp_frem(N1, N2, N).
N mp_is gcd(X, Y)     :- !, N1 mp_is X, N2 mp_is Y, mp_gcd(N1, N2, N).
N mp_is im(X)         :- !, N1 mp_is X, mp_im(N1, N).
N mp_is integer(X)    :- !, N1 mp_is X, mp_round(N1, N).
N mp_is jacobi(X, Y)  :- !, N1 mp_is X, N2 mp_is Y, mp_jacobi(N1, N2, N).
N mp_is lcm(X, Y)     :- !, N1 mp_is X, N2 mp_is Y, mp_lcm(N1, N2, N).
N mp_is legendre(X, Y):- !, N1 mp_is X, N2 mp_is Y, mp_legendre(N1, N2, N).
N mp_is ln(X)         :- !, N1 mp_is X, mp_log(N1, N).
N mp_is log(X)        :- !, N1 mp_is X, mp_log(N1, N).
N mp_is log10(X)      :- !, N1 mp_is X, mp_log10(N1, N).
N mp_is max(X, Y)     :- !, N1 mp_is X, N2 mp_is Y, mp_min(N1, N2, N).
N mp_is min(X, Y)     :- !, N1 mp_is X, N2 mp_is Y, mp_min(N1, N2, N).
N mp_is num(X)        :- !, N1 mp_is X, mp_num(N1, N).
N mp_is prec(X)       :- !, N1 mp_is X, mp_getprec(N1, N).
N mp_is prec(X, Y)    :- !, N1 mp_is X, mp_getprec(0, P), mp_setprec(N1), N mp_is Y, mp_setprec(P).
N mp_is random(X)     :- !, N1 mp_is X, mp_z(N2, N1), N is random(N2).
N mp_is re(X)         :- !, N1 mp_is X, mp_re(N1, N).
N mp_is reldiff(X, Y) :- !, N1 mp_is X, N2 mp_is Y, mp_reldiff(N1, N2, N).
N mp_is round(X)      :- !, N1 mp_is X, mp_round(N1, N).
N mp_is setbit(X, Y)  :- !, N1 mp_is X, N2 mp_is Y, mp_setbit(N1, N2, N).
N mp_is sign(X)       :- !, N1 mp_is X, mp_sgn(N1, R), (R == (<) -> N = -1; R == (>) -> N = 1; N = 0).
N mp_is sin(X)        :- !, N1 mp_is X, mp_sin(N1, N).
N mp_is sqrt(X)       :- !, N1 mp_is X, mp_sqrt(N1, N).
N mp_is tan(X)	      :- !, N1 mp_is X, mp_tan(N1, N).
N mp_is trem(X, Y)    :- !, N1 mp_is X, N2 mp_is Y, mp_trem(N1, N2, N).
N mp_is trunc(X)      :- !, N1 mp_is X, mp_z(N1, N).
N mp_is truncate(X)   :- !, N1 mp_is X, mp_z(N1, N).
N mp_is e	      :- !, mp_exp(1, N).
N mp_is pi	      :- !, mp_f("3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068", N).
N mp_is i	      :- !, mp_c(complex(0, 1), N).
N mp_is N.

%	mp_compare(?Order, ?Term1, ?Term2)
%	Like compare/3 gives order of Term1 with respect to Term2. However,
%	variables have the highest order, thus are the most general,
%	e.g. X > f(Y) > f(a) > a > 1 > 1/2.	(because 1/2 is rational)
%	In addition, terms with variables that appear more than once in a term
%	are considered more specific to terms with arbitrary variables at the
%	some position in the term assuming that variables used in Term1 do not
%	appear in Term2.
%	e.g. f(X,Y) > f(Z,Z)
%	This allows for ordering terms from more specific to more general using
%	mp_sort/2.

mp_compare(_, X, Y) :- mp_compare(X, Y).
mp_compare(O, _, _) :- flag('mp:mp_compare', O, O).

mp_compare(X, Y) :-
	mp_compare1(O, X, Y),
	flag('mp:mp_compare', _, O),
	fail.

:- index(mp_compare1(0, 1, 1)).

mp_compare1(O, X, Y) :-
	var(X), !,
	(	var(Y)
	->	Y = '',
		O = (=)
	;	O = (>)
	),
	X = ''.
mp_compare1(O, '', Y) :- !,
	(	var(Y)
	->	Y = '',
		O = (<)
	;	Y == ''
	->	O = (=)
	;	O = (>)
	).
mp_compare1(<, _, '') :- !.
mp_compare1(O, $mpz(X), $mpz(Y)) :-
	mp_cmp($mpz(X), $mpz(Y), O), !.
mp_compare1(O, $mpq(N, D), $mpq(M, E)) :-
	mp_cmp($mpq(N, D), $mpq(M, E), O), !.
mp_compare1(O, $mpf(X), $mpf(Y)) :- 
	mp_cmp($mpf(X), $mpf(Y), O), !.
mp_compare1(O, $mpc(R, I), $mpc(S, J)) :-
	mp_cmp($mpc(R, I), $mpc(S, J), O), !.
mp_compare1(=, [], []) :- !.
mp_compare1(<, [], _) :- !.
mp_compare1(O, [X|Xs], [Y|Ys]) :-
	mp_compare1(R, X, Y), !,
	(	R == (=)
	->	mp_compare1(O, Xs, Ys)
	;	O = R
	).
mp_compare1(O, F, G) :-
	functor(F, N, A),
	functor(G, N, A),
	F =.. [N|Xs],
	G =.. [N|Ys], !,
	mp_compare1(O, Xs, Ys).
mp_compare1(O, X, Y) :-
	mp_cmp(X, Y, O), !.
mp_compare1(O, X, Y) :- !,
	compare(O, X, Y).

%	mp_sort(+List1, ?List2)
%	List2 is ordered List1 (with possible duplicate elements)

mp_sort(L, S) :- predsort(mp_compare, L, S).

%       mp_merge(+List1, +List2, -List3)
%       Merge the ordered sets List1 and List2 into a new ordered  list.
%       Duplicates are not removed and their order is maintained.

mp_merge([], L, L) :- !.
mp_merge(L, [], L) :- !.
mp_merge([H1|T1], [H2|T2], [H|R]) :-
	(	mp_compare(>, H1, H2)
	->	H = H2,
		mp_merge([H1|T1], T2, R)
	;	H = H1,
		mp_merge(T1, [H2|T2], R)
	).

/******************************************************************************\
*
*	Set operations (adapted from the oset library by Jon Jagger)
*
\******************************************************************************/

%	mp_setunion(+Set1, +Set2, -Union).

mp_setunion([], U, U).
mp_setunion([H1|T1], L2, U) :- union2(L2, H1, T1, U).

union2([], H1, T1, [H1|T1]).
union2([H2|T2], H1, T1, U) :- mp_compare(O, H1, H2), union3(O, H1, T1, H2, T2, U).

union3(<, H1, T1,  H2, T2, [H1|U]) :- union2(T1, H2, T2, U).
union3(=, H1, T1, _H2, T2, [H1|U]) :- mp_setunion(T1, T2, U).
union3(>, H1, T1,  H2, T2, [H2|U]) :- union2(T2, H1, T1, U).

%	mp_setint(+Set1, +Set2, -Int)

mp_setint([], _I, []).
mp_setint([H1|T1], L2, I) :- isect2(L2, H1, T1, I).

isect2([], _H1, _T1, []).
isect2([H2|T2], H1, T1, I) :- mp_compare(O, H1, H2), isect3(O, H1, T1, H2, T2, I).

isect3(<, _H1, T1,  H2, T2, I) :- isect2(T1, H2, T2, I).
isect3(=, H1, T1, _H2, T2, [H1|I]) :- mp_setint(T1, T2, I).
isect3(>, H1, T1,  _H2, T2, I) :- isect2(T2, H1, T1, I).


%	mp_setdiff(+Set1, +Set2, -Diff)

mp_setdiff([], _N, []).
mp_setdiff([H1|T1], L2, D) :- diff21(L2, H1, T1, D).

diff21([], H1, T1, [H1|T1]).
diff21([H2|T2], H1, T1, D) :- mp_compare(O, H1, H2), diff3(O, H1, T1, H2, T2, D).

diff12([], _H2, _T2, []).
diff12([H1|T1], H2, T2, D) :- mp_compare(O, H1, H2), diff3(O, H1, T1, H2, T2, D).

diff3(<,  H1, T1,  H2, T2, [H1|D]) :- diff12(T1, H2, T2, D).
diff3(=, _H1, T1, _H2, T2, D) :- mp_setdiff(T1, T2, D).
diff3(>,  H1, T1, _H2, T2, D) :- diff21(T2, H1, T1, D).

%	mp_addel(+Set, +El, -Add)  

mp_addel([], E, [E]). 
mp_addel([H|T], E, A) :- mp_compare(Order, H, E), addel(Order, H, T, E, A).

addel(<, H, T,  E, [H|A]) :- mp_addel(T, E, A).
addel(=, H, T, _E, [H|T]). 
addel(>, H, T,  E, [E,H|T]).

%	mp_delel(+Set, +el, -Del)  

mp_delel([], _E, []).
mp_delel([H|T], E, D) :- compare(Order, H, E), delel(Order, H, T, E, D).

delel(<,  H, T,  E, [H|D]) :- mp_delel(T, E, D).
delel(=, _H, T, _E, T).
delel(>,  H, T, _E, [H|T]).

/******************************************************************************\
*
*	Arithmetic
*
\******************************************************************************/

%	mp_round(+N, ?Z)
%	Z = round(N)

mp_round(N, Z) :-
	mp_sgn(N, <), !,
	mp_sub(1/2, N, N1),
	mp_trunc(N1, Z1),
	mp_neg(Z1, Z).
mp_round(N, Z) :-
	mp_add(1/2, N, N1),
	mp_trunc(N1, Z).

%	mp_tdiv(+Z1, +Z2, ?Z3)
%	Z3 = trunc(Z1/Z2)

mp_tdiv(X, Y, Z) :- mp_tdivrem(X, Y, Z, _).

%	mp_fdiv(+Z1, +Z2, ?Z3)
%	Z3 = floor(Z1/Z2)

mp_fdiv(X, Y, Z) :- mp_fdivrem(X, Y, Z, _).

%	mp_cdiv(+Z1, +Z2, ?Z3)
%	Z3 = ceil(Z1/Z2)

mp_cdiv(X, Y, Z) :- mp_cdivrem(X, Y, Z, _).

%	mp_trem(+Z1, +Z2, ?Z3)
%	Z3 = remainder of trunc(Z1/Z2)

mp_trem(X, Y, Z) :- mp_tdivrem(X, Y, _, Z).

%	mp_frem(+Z1, +Z2, ?Z3)
%	Z3 = remainder of floor(Z1/Z2)

mp_frem(X, Y, Z) :- mp_fdivrem(X, Y, _, Z).

%	mp_crem(+Z1, +Z2, ?Z3)
%	Z3 = remainder of ceil(Z1/Z2)

mp_crem(X, Y, Z) :- mp_cdivrem(X, Y, _, Z).

%	mp_num(+Q, ?Z)
%	Z = numerator of Q

mp_num($mpq(N, _), $mpz(N)).
mp_num(N/_, N).

%	mp_den(+Q, ?Z)
%	Z = denominator of Q

mp_den($mpq(_, D), $mpz(D)).
mp_den(_/D, D).

%	mp_re(+C, ?F)
%	F = Re(C)

mp_re($mpc(R, _), $mpf(R)).
mp_re(complex(R, _), R).

%	mp_im(+C, ?F)
%	F = Im(C)

mp_im($mpc(_, I), $mpf(I)).
mp_im(complex(_, I), I).

%	mp_conj(+C1, ?C2)
%	C2 = complex conjugate of C1

mp_conj($mpc(R, I), $mpc(R, N)) :- mp_neg($mpf(I), N).
mp_conj(complex(R, I), complex(R, N)) :- mp_neg(I, N).

%	mp_lcm(+N1, +N2, ?LCM)
%	LCM = LCM(N1, N2)

mp_lcm(Z1, Z2, LCM) :-
	mp_gcd(Z1, Z2, GCD),
	mp_mul(Z1, Z2, Z3),
	mp_div(Z3, GCD, LCM).

%	mp_min(+N1, +N2, ?Min)
%	Min = min(N1, N2)

mp_min(N1, N2, N1) :- mp_cmp(N1, N2, <), !.
mp_min(_,  N2, N2).

%	mp_max(+N1, +N2, ?Max)
%	Max = max(N1, N2)

mp_max(N1, N2, N1) :- mp_cmp(N1, N2, >), !.
mp_max(_,  N2, N2).

%	mp_bernoulli(+Z, ?B)
%	B = Bernoulli(Z)

mp_bernoulli(Z, B) :- mp_z(N, Z), \+ mp_sgn(N, <), bernoulli(N, B).

bernoulli(0, B) :- mp_z(1, B), !.
bernoulli(1, B) :- mp_q(-1/2, B), !.
bernoulli(2, B) :- mp_q(1/6, B), !.
bernoulli(4, B) :- mp_q(-1/30, B), !.
bernoulli(6, B) :- mp_q(1/42, B), !.
bernoulli(8, B) :- mp_q(-1/30, B), !.
bernoulli(N, B) :- 1 is N mod 2, mp_z(0, B), !.
bernoulli(N, B) :- bernoulli1(N, [B1|_]), mp_fac(N, F), mp_mul(F, B1, B).

bernoulli1(8, [-1/1209600, 0, 1/30240, 0, -1/720, 0, 1/12, -1/2, 1]) :- !.
bernoulli1(N, [0|Bs]) :-
	1 is N mod 2, !,
	succ(K, N),
	bernoulli1(K, Bs).
bernoulli1(N, [B|Bs]) :-
	succ(K, N),
	bernoulli1(K, Bs),
	bernoulli1(Bs, 2, 0, B).

bernoulli1([0|Bs], I, B1, B2) :-
	succ(I, I1), !,
	bernoulli1(Bs, I1, B1, B2).
bernoulli1([B|Bs], I, B1, B3) :-
	mp_fac(I, F),
	mp_div(B, F, BF),
	mp_sub(B1, BF, B2),
	succ(I, I1), !,
	bernoulli1(Bs, I1, B2, B3).
bernoulli1([], _, B, B).

%	mp_sin(+N, ?Sin)
%	Compute sine of N using Taylor series expansion
%	Note: complex numbers are not supported yet

mp_sin(N, Sin) :- mp_getprec(0, 64), mp_f(F, N), !, Sin is sin(F).
mp_sin(N, Sin) :- mp_f(N, Y), trig(N, 1, Y, Sin).

%	mp_cos(+N, ?Cos)
%	Compute cosine of N using Taylor series expansion
%	Note: complex numbers are not supported yet

mp_cos(N, Cos) :- mp_getprec(0, 64), mp_f(F, N), !, Cos is cos(F).
mp_cos(N, Cos) :- mp_f(1, Y), trig(N, 0, Y, Cos).

%	mp_tan(+N, ?Tan)
%	Tan = sin(N)/cos(N)
%	Note: complex numbers are not supported yet

mp_tan(N, Tan) :- mp_getprec(0, 64), mp_f(F, N), !, Tan is tan(F).
mp_tan(N, Tan) :- mp_sin(N, Sin), mp_cos(N, Cos), mp_div(Sin, Cos, Tan).

%	mp_asin(+N, ?ASin)
%	Compute arcsine of N using Taylor series expansion
%	Note: complex numbers are not supported yet

mp_asin(N, ASin) :- mp_f(F, N), ASin is asin(F).

%	mp_acos(+N, ?Aos)
%	Compute arccosine of N using Taylor series expansion
%	Note: complex numbers are not supported yet

mp_acos(N, ACos) :- mp_f(F, N), ACos is acos(F).

%	mp_atan(+N, ?ATan)
%	Compute arctan of N using Taylor series expansion
%	Note: complex numbers are not supported yet

mp_atan(N, ATan) :- mp_f(F, N), ATan is atan(F).

%	mp_atan(+N1, +N2, ?ATan)

mp_atan(N1, N2, ATan) :-
	mp_f(F1, N1),
	mp_f(F2, N2),
	ATan is atan(F1, F2).

%	mp_exp(+N, ?Exp)
%	Exp = e^N

mp_exp(N, Exp) :-
	mp_getprec(0, 64),
	mp_f(F, N),
	F > -700,
	F < 700,
	Exp is exp(F), !.
mp_exp(N, Exp) :-
	mp_complex(N), !,
	mp_re(N, R),
	mp_im(N, I),
	mp_exp(R, E),
	mp_sin(I, S),
	mp_cos(I, C),
	mp_mul(E, S, ES),
	mp_mul(E, C, EC),
	mp_c(complex(ES, EC), Exp).
mp_exp(N, Exp) :-
	mp_f(N, N1),
	mp_add(N1, 1, N2),
	exp(N1, 1, N1, N2, Exp).

%	mp_log(+N, ?Log)
%	Log = ln(N)
%	Note: complex numbers are not supported yet

mp_log(N, Log) :-
	mp_getprec(0, 64),
	mp_f(F, N), !,
	F > 0,
	Log is log(F).
mp_log(N, Log) :-
	mp_sgn(N, >),
	mp_cmp(N, 1, R),
	(	R == (>)
	->	mp_f(N, N1),
		mp_getprec(N1, P),
		Eps is (4/P)+1,
		log_shift(N1, 0, Eps, N2, S),
		mp_sub(N2, 1, N3),
		mp_neg(N3, N4),
		log(N4, 1, N3, N3, Log1),
		mp_lsh(Log1, S, Log)
	;	R == (=)
	->	Log = 0.0
	;	mp_div(1, N, N1),
		mp_log(N1, Log1),
		mp_neg(Log1, Log)
	).

%	mp_log10(+N, ?Log)
%	Log = log10(N)
%	Note: complex numbers are not supported yet

mp_log10(N, Log) :- mp_getprec(0, 64), mp_f(F, N), !, F > 0, Log is log10(F).
mp_log10(N, Log) :- mp_log(N, LnN), mp_log(10, Ln10), mp_div(LnN, Ln10, Log).

%	mp_power(+N1, +N2, ?Pow)
%	Pow = N1^N2

mp_power(N1, N2, Pow) :-
	mp_pow(N1, N2, Pow), !.
mp_power(N1, N2, Pow) :-
	mp_integer(N2),
	(	mp_sgn(N2, <)
	->	mp_neg(N2, N3),
		pow(N1, N3, [], P),
		mp_inv(P, Pow)
	;	pow(N1, N2, [], Pow)
	).
mp_power(N1, N2, Pow) :-
	mp_log(N1, LnN1),
	mp_mul(LnN1, N2, P),
	mp_exp(P, Pow).

%	mp_xor(+Z1, +Z2, ?Xor)
%	Xor = Z1 xor Z2

mp_xor(Z1, Z2, Xor) :- mp_or(Z1, Z2, Or), mp_and(Z1, Z2, And), mp_sub(Or, And, Xor).

%	mp_eqv(+Z1, +Z2, ?Eqv)
%	Eqv = Z1 eqv Z2

mp_eqv(Z1, Z2, Eqv) :- mp_xor(Z1, Z2, Xor), mp_not(Xor, Eqv).

%	trig(+N, +I, +Term, -Result)
%	Computes trig functions using Taylor series expansion

trig(N, I, Term, Result) :-
	mp_mul(N, N, N2),
	mp_neg(N2, NegN2),
	trig(NegN2, I, Term, Term, Result).

trig(NegN2, I, Term, Sum, Result) :-
	mp_mul(Term, NegN2, Term1),
	mp_add(I, 1, I1),
	mp_div(Term1, I1, Term2),
	mp_add(I, 2, I2),
	mp_div(Term2, I2, Term3),
	mp_add(Sum, Term3, Sum1),
	(	mp_cmp(Sum, Sum1, =)
	->	Result = Sum
	;	trig(NegN2, I2, Term3, Sum1, Result)
	).

%	exp(+N, +I, +Term, +Sum, -Exp)
%	Computes exp function using Taylor series expansion

exp(N, I, Term, Sum, Exp) :-
	mp_add(I, 1, I1),
	mp_mul(Term, N, Term1),
	mp_div(Term1, I1, Term2),
	mp_add(Sum, Term2, Sum1),
	(	mp_cmp(Sum, Sum1, =)
	->	Exp = Sum
	;	exp(N, I1, Term2, Sum1, Exp)
	).

%	log(+N, +I, +Term, +Sum, -Log)
%	Computes log function using Taylor series expansion

log(N, I, Term, Sum, Log) :-
	mp_add(I, 1, I1),
	mp_mul(Term, N, Term1),
	mp_div(Term1, I1, Term2),
	mp_add(Sum, Term2, Sum1),
	(	mp_cmp(Sum, Sum1, =)
	->	Log = Sum
	;	log(N, I1, Term1, Sum1, Log)
	).

log_shift(N, S, Eps, N2, S2) :-
	mp_cmp(N, Eps, >), !,
	mp_sqrt(N, N1),
	S1 is S+1, !,
	log_shift(N1, S1, Eps, N2, S2).
log_shift(N, S, _, N, S).

%	pow(+N, +I, +Ps, -Pow)
%	Computes power function using Indian power algorithm

pow(N, I, Ps, Pow) :-
	mp_cmp(I, 1, R),
	(	R == (<)
	->	pow_prod(Ps, 1, Pow)
	;	R == (=)
	->	pow_prod(Ps, N, Pow)
	), !.
pow(N, I, Ps, Pow) :-
	mp_mul(N, N, N2),
	mp_rsh(I, 1, I1),
	mp_and(I, 1, R1),
	pow_rem(N, R1, Ps, Ps1), !,
	pow(N2, I1, Ps1, Pow).

pow_prod([P|Ps], Pow1, Pow3) :- mp_mul(P, Pow1, Pow2), !, pow_prod(Ps, Pow2, Pow3).
pow_prod([], Pow, Pow).

pow_rem(_, R, Ps,     Ps) :- mp_cmp(R, 0, =), !.
pow_rem(N, _, Ps, [N|Ps]).

%	Term inspection predicates

mp_atom(A) :- atom(A).

mp_atomic(A) :- atomic(A), !.
mp_atomic(N) :- mp_number(N).

mp_number($)             :- !, fail.
mp_number($mpz(_))       :- !.
mp_number($mpf(_))       :- !.
mp_number($mpq(_, _))    :- !.
mp_number($mpc(_, _))    :- !.
mp_number(N/D)           :- mp_integer(N), mp_integer(D), !.
mp_number(complex(R, I)) :- mp_number(R), mp_number(I), !.
mp_number(N)             :- number(N).

mp_integer($)       :- !, fail.
mp_integer($mpz(_)) :- !.
mp_integer(N)       :- integer(N), !.

mp_rational($)          :- !, fail.
mp_rational($mpq(_, _)) :- !.
mp_rational(N/D)        :- mp_integer(N), mp_integer(D).

mp_float($)       :- !, fail.
mp_float($mpf(_)) :- !.
mp_float(N)       :- float(N), !.

mp_complex($)             :- !, fail.
mp_complex($mpc(_, _))    :- !.
mp_complex(complex(R, I)) :- mp_number(R), mp_number(I), !.

mp_compound(X) :- \+ mp_atomic(X).
