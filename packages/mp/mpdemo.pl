/*	mp.pl

	Demonstrate MP library

	Copyright: Robert A. van Engelen, Florida State University, 1999.
*/

:- module(mp_demo,
	 [ mp_demo/0
	 ]).

load_mp :-
	current_predicate(_, user:mp_term(_,_,_,_)), !.
load_mp :-
	use_module(library(mp)).

:- load_mp.

:- style_check(+string).

key :- format('Press RETURN ...'), get0(_), nl.

mp_demo :-
	format('~nHello MP user craving for Biggie Numbers!~n~n'),
	format('Here are some examples of MP~n~n'),
	key,
	X1 mp_is fac(10),
	format('10! = ~p~n', [X1]),
	key,
	X2 mp_is fac(100),
	format('100! = ~p~n', [X2]),
	key,
	X3 mp_is fac(1000),
	format('1000! = ~p~n', [X3]),
	key,
	X4 mp_is prec(10000, sin(1)),
	format('sin(1) = ~p~nUsing 10000 bits of precision~n', [X4]),
	key,
	X5 mp_is 7/6 * 36/91,
	format('7/6 * 36/91 = ~p~n', [X5]),
	key,
	X6 mp_is 2^1000,
	format('2^1000 = ~p~n', [X6]),
	key,
	X7 mp_is (3/7)^17,
	format('(3/7)^17 = ~p~n', [X7]),
	key,
	X8 mp_is "12345678901234567890123456789" /\ "98765432109876543210987654321",
	format('12345678901234567890123456789 & 98765432109876543210987654321 = ~p~n', [X8]),
	key,
	X9 mp_is 8.12 << 2,
	format('8.12 << 2 = ~p~n', [X9]),
	key,
	X10 mp_is (1+2*i)*pi,
	format('(1+2*i)*pi = ~p~n', [X10]),
	key,
	format('Bye!~n~n').

