/*  $Id$

    Part of SWI-Prolog

    Author:        R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): Universidade do Porto, University of Amsterdam,
		   VU University Amsterdam.
*/

/*************************************************************************
*									 *
*	 YAP Prolog							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		random.yap						 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	Random operations					 *
*									 *
*************************************************************************/

% original code from RA O'Keefe.

%   This is algorithm AS 183 from Applied Statistics.  I also have a C
%   version.  It is really very good.  It is straightforward to make a
%   version which yields 15-bit random integers using only integer
%   arithmetic.

:- module(random,
	  [ random/1,			% -float
	    random/3,			% +Low, +High, -Random
	    randseq/3,			% +Size, +Max, -Set
	    randset/3,			% +Size, +Max, -List
	    getrand/1,			% -State
	    setrand/1,			% +State
	    random_permutation/2	% +List, -Permutation
	  ]).
:- use_module(library(pairs)).
:- use_module(library(error)).

/** <module> Random numbers

This library is derived from the DEC10   library random. Later, the core
random generator was moved to C. The current version uses the SWI-Prolog
arithmetic functions to realise this library.  These functions are based
on the GMP library.

@copyright	DEC10 version: Public domain, YAP: Artistic
@author		R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
@see		Built-in function random/1: A is random(10)
*/



		 /*******************************
		 *	   C PRIMITIVES		*
		 *******************************/

%%	random(-R:float) is det.
%
%	Binds R to a new random number in [0.0,1.0).
%
%	@see setrand/1, getrand/1 maye be used to fetch/set the state.
%	@see In SWI-Prolog, random/1 is implemented by the function
%	     random_float/0.

random(R) :-
	R is random_float.

%%	setrand(+State) is det.
%%	getrand(-State) is det.
%
%	Query/set the state of the random   generator.  This is intended
%	for  restarting  the  generator  at  a  known  state  only.  The
%	predicate  setrand/1  accepts  an  opaque    term   returned  by
%	getrand/1. This term may be  asserted,   written  and  read. The
%	application may not make other assumptions about this term.
%
%	For compatibility reasons with older   versions of this library,
%	setrand/1 also accepts a term rand(A,B,C), where  A, B and C are
%	integers in the range 1..30,000. This   argument is used to seed
%	the random generator.  Deprecated.
%
%	@see	set_random/1 and random_property/1 provide the SWI-Prolog
%		native implementation.
%	@error	existence_error(random_state, _) is raised if the
%		underlying infrastructure cannot fetch the random state.
%		This is currently the case if SWI-Prolog is not compiled
%		with the GMP library.

setrand(rand(A,B,C)) :- !,
	Seed is A<<30+B<<15+C,
	set_random(seed(Seed)).
setrand(State) :-
	set_random(state(State)).

:- if(current_predicate(random_property/1)).
getrand(State) :-
	random_property(state(State)).
:- else.
getrand(State) :-
	existence_error(random_state, State).
:- endif.


		 /*******************************
		 *	       PROLOG		*
		 *******************************/

%%	random(+L:int, +U:int, -R:int) is det.
%%	random(+L:float, +U:float, -R:float) is det.
%
%	Binds R to a random  number  in  [L,U).   If  L  and  U are both
%	integers, R is an integer, Otherwise, R  is a float. Note that U
%	will *never* be generated.
%
%	@bug	The state is only 48-bits.  This is insufficient for
%		generating uniformely distributed integers in a very
%		large domain.

random(L, U, R) :-
	integer(L), integer(U), !,
	R is L+random(U-L).
random(L, U, R) :-
	number(L), number(U), !,
	R is L+((U-L)*random_float).
random(L, U, _) :-
	must_be(number, L),
	must_be(number, U).

%%	randset(+K:int, +N:int, -S:list(int)) is det.
%
%	S is a sorted list of K integers in the range 1..N.
%
%	@see randseq/3.


randset(K, N, S) :-
	must_be(nonneg, K),
	K =< N,
	randset(K, N, [], S).


randset(0, _, S, S) :- !.
randset(K, N, Si, So) :-
	random(X),
	X * N < K, !,
	J is K-1,
	M is N-1,
	randset(J, M, [N|Si], So).
randset(K, N, Si, So) :-
	M is N-1,
	randset(K, M, Si, So).


%%	randseq(+K:int, +N:int, -S:list(int)) is det.
%
%	S is a list of K integers in the range 1..N. The order is
%	random.
%
%	@see randset/3.


randseq(K, N, S) :-
	randseq(K, N, L, []),
	keysort(L, R),
	pairs_values(R, S).

randseq(0, _, S, S) :- !.
randseq(K, N, [Y-N|Si], So) :-
	random(X),
	X * N < K, !,
	random(Y),
	J is K-1,
	M is N-1,
	randseq(J, M, Si, So).
randseq(K, N, Si, So) :-
	M is N-1,
	randseq(K, M, Si, So).

%%	random_permutation(+List, -Permutation) is det.
%
%	Permutation is a random permutation of List. This is intended to
%	process the elements of List in random order.

random_permutation(List, RandomPermutation) :-
        key_random(List, Keyed),
        keysort(Keyed, Sorted),
        pairs_values(Sorted, RandomPermutation).

key_random([], []).
key_random([H|T0], [K-H|T]) :-
        random(K),
        key_random(T0, T).
