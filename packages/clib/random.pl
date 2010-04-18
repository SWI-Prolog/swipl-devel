/*  $Id$

    Part of SWI-Prolog

    Author:        R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): Universidade do Porto, University of Amsterdam
*/

/*************************************************************************
*									 *
*	 YAP Prolog 							 *
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
	    setrand/1			% +State
	  ]).
:- use_module(library(pairs)).

:- use_foreign_library(foreign(random)).

/** <module> Random numbers

Random number generator developed as  part   of  the  DEC10 library. The
algorithm is based  on  AS  183   from  Applied  Statistics.  Originally
implemented ib Prolog by Richard O'Keeke.   The SWI-Prolog versions is a
translation of a C-version for YAP based on the orginal source.

@copyright	DEC10 version: Public domain, YAP: Artistic
@author 	R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
@see		Built-in function random/1: A is random(10)
*/



		 /*******************************
		 *	   C PRIMITIVES		*
		 *******************************/

%%	random(-R:float) is det.
%
%	Binds R to a new random number in [0.0,1.0).
%
%	@see setrand/1, getrand/1.

%%	setrand(+State:rand(A,B,C)) is det.
%%	getrand(+State:rand(A,B,C)) is det.
%
%	Query/set the state of the  random  library.   A,  B  and  C are
%	integers in the range 1..30,000. The initial state is predefined
%	and can be extracted using getrand/1:
%
%	    ==
%	    ?- getrand(X).
%	    X = rand(27314, 9213, 17773).
%	    ==
%
%	@see random/1.


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
	random(X),
	R is L+floor((U-L)*X).
random(L, U, R) :-
	number(L), number(U), !,
	random(X),
	R is L+((U-L)*X).

%%	randset(+K:int, +N:int, -S:list(int)) is det.
%
%	S is a sorted list of K integers in the range 1..N.
%
%	@see randseq/3.


randset(K, N, S) :-
	K >= 0,
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




