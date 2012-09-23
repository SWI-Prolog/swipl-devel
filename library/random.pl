/*  Part of SWI-Prolog

    Author:        R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): Universidade do Porto, University of Amsterdam,
		   VU University Amsterdam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.

    Alternatively, this program may be distributed under the Perl
    Artistic License, version 2.0.
*/

:- module(random,
	  [ random/1,			% -Float (0,1)
	    random_between/3,		% +Low, +High, -Random

	    getrand/1,			% -State
	    setrand/1,			% +State

	    maybe/0,
	    maybe/1,			% +P
	    maybe/2,			% +K, +N

	    random_perm2/4,		% A,B, X,Y

	    random_member/2,		% -Element, +List
	    random_select/3,		% ?Element, +List, -Rest

	    randseq/3,			% +Size, +Max, -Set
	    randset/3,			% +Size, +Max, -List
	    random_permutation/2,	% ?List, ?Permutation

					% deprecated interface
	    random/3			% +Low, +High, -Random
	  ]).
:- use_module(library(pairs)).
:- use_module(library(error)).
:- use_module(library(lists)).

/** <module> Random numbers

This library is derived from the DEC10   library random. Later, the core
random generator was moved to C. The current version uses the SWI-Prolog
arithmetic functions to realise this library.  These functions are based
on the GMP library.

@author		R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
@see		Built-in function random/1: A is random(10)
*/

check_gmp :-
	current_arithmetic_function(random_float), !.
check_gmp :-
	print_message(warning, random(no_gmp)).

:- initialization check_gmp.


		 /*******************************
		 *	   PRIMITIVES		*
		 *******************************/

%%	random(-R:float) is det.
%
%	Binds R to a new random float in the _open_ interval (0.0,1.0).
%
%	@see setrand/1, getrand/1 may be used to fetch/set the state.
%	@see In SWI-Prolog, random/1 is implemented by the function
%	     random_float/0.

random(R) :-
	R is random_float.

%%	random_between(+L:int, +U:int, -R:int) is semidet.
%
%	Binds R to a random integer in [L,U] (i.e., including both L and
%	U).  Fails silently if U<L.

random_between(L, U, R) :-
	integer(L), integer(U), !,
	U >= L,
	R is L+random((U+1)-L).
random_between(L, U, _) :-
	must_be(integer, L),
	must_be(integer, U).


%%	random(+L:int, +U:int, -R:int) is det.
%%	random(+L:float, +U:float, -R:float) is det.
%
%	Generate a random integer or float in a   range.  If L and U are
%	both integers, R is a random integer   in the half open interval
%	[L,U). If L and U are both  floats,   R  is  a float in the open
%	interval (L,U).
%
%	@deprecated Please use random/1 for   generating  a random float
%	and random_between/3 for generating a  random integer. Note that
%	the  random_between/3  includes  the  upper  bound,  while  this
%	predicate excludes the upper bound.

random(L, U, R) :-
	integer(L), integer(U), !,
	R is L+random(U-L).
random(L, U, R) :-
	number(L), number(U), !,
	R is L+((U-L)*random_float).
random(L, U, _) :-
	must_be(number, L),
	must_be(number, U).


		 /*******************************
		 *	       STATE		*
		 *******************************/

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
		 *	      MAYBE		*
		 *******************************/

%%	maybe is semidet.
%
%	Succeed/fail with equal probability (variant of maybe/1).

maybe :-
	random(2) =:= 0.

%%	maybe(+P) is semidet.
%
%	Succeed with probability P, fail with probability 1-P

maybe(P) :-
	must_be(between(0.0,1.0), P),
	random_float < P.

%%	maybe(+K, +N) is semidet.
%
%	Succeed with probability K/N (variant of maybe/1)

maybe(K, N) :-
	integer(K), integer(N),
	between(0, N, K), !,
	random(N) < K.
maybe(K, N) :-
	must_be(nonneg, K),
	must_be(nonneg, N),
	domain_error(not_less_than_zero,N-K).


		 /*******************************
		 *	    PERMUTATION		*
		 *******************************/

%%	random_perm2(?A, ?B, ?X, ?Y) is semidet.
%
%	Does X=A,Y=B or X=B,Y=A with equal probability.

random_perm2(A,B, X,Y) :-
	(   maybe
	->  X = A, Y = B
	;   X = B, Y = A
	).


		 /*******************************
		 *    SET AND LIST OPERATIONS	*
		 *******************************/

%%	random_member(-X, +List:list) is semidet.
%
%	X is a random member of   List.  Equivalent to random_between(1,
%	|List|), followed by nth1/3. Fails of List is the empty list.
%
%	@compat	Quintus and SICStus libraries.

random_member(X, List) :-
	List \== [],
	length(List, Len),
	N is random(Len),
	nth0(N, List, X).

%%	random_select(-X, +List, -Rest) is semidet.
%%	random_select(+X, -List, +Rest) is det.
%
%	Randomly select or insert an element.   Either List or Rest must
%	be a list.  Fails if List is the empty list.
%
%	@compat	Quintus and SICStus libraries.

random_select(X, List, Rest) :-
	(   '$skip_list'(Len, List, Tail),
	    Tail == []
	->  true
	;   '$skip_list'(RLen, Rest, Tail),
	    Tail == []
	->  Len is RLen+1
	), !,
	Len > 0,
	N is random(Len),
	nth0(N, List, X, Rest).
random_select(_, List, Rest) :-
	partial_list(List), partial_list(Rest),
	instantiation_error(List+Rest).
random_select(_, List, Rest) :-
	must_be(list, List),
	must_be(list, Rest).

%%	randset(+K:int, +N:int, -S:list(int)) is det.
%
%	S is a sorted list of  K   unique  random  integers in the range
%	1..N. Implemented by enumerating 1..N   and  deciding whether or
%	not the number should be part of the set.  For example:
%
%	  ==
%	  ?- randset(5, 5, S).
%	  S = [1, 2, 3, 4, 5].		(always)
%	  ?- randset(5, 20, S).
%	  S = [2, 7, 10, 19, 20].
%	  ==
%
%	@see randseq/3.
%	@bug Slow if N is large and K is small.

randset(K, N, S) :-
	must_be(nonneg, K),
	K =< N,
	randset(K, N, [], S).

randset(0, _, S, S) :- !.
randset(K, N, Si, So) :-
	random(N) < K, !,
	J is K-1,
	M is N-1,
	randset(J, M, [N|Si], So).
randset(K, N, Si, So) :-
	M is N-1,
	randset(K, M, Si, So).


%%	randseq(+K:int, +N:int, -List:list(int)) is det.
%
%	S is a list of K unique random   integers in the range 1..N. The
%	order is random. Works as if defined by the following code.
%
%	  ==
%	  randseq(K, N, List) :-
%		randset(K, N, Set),
%		random_permutation(Set, List).
%	  ==
%
%	@see randset/3.


randseq(K, N, S) :-
	randseq(K, N, L, []),
	keysort(L, R),
	pairs_values(R, S).

randseq(0, _, S, S) :- !.
randseq(K, N, [Y-N|Si], So) :-
	random(N) < K, !,
	random(Y),
	J is K-1,
	M is N-1,
	randseq(J, M, Si, So).
randseq(K, N, Si, So) :-
	M is N-1,
	randseq(K, M, Si, So).

%%	random_permutation(+List, -Permutation) is det.
%%	random_permutation(-List, +Permutation) is det.
%
%	Permutation is a random permutation of List. This is intended to
%	process the elements of List in   random order. The predicate is
%	symmetric.
%
%	@error instantiation_error, type_error(list, _).

random_permutation(List1, List2) :-
	is_list(List1), !,
	random_permutation_(List1, List2).
random_permutation(List1, List2) :-
	is_list(List2), !,
	random_permutation_(List2, List1).
random_permutation(List1, List2) :-
	partial_list(List1), partial_list(List2), !,
	instantiation_error(List1+List2).
random_permutation(List1, List2) :-
	must_be(list, List1),
	must_be(list, List2).

random_permutation_(List, RandomPermutation) :-
        key_random(List, Keyed),
        keysort(Keyed, Sorted),
        pairs_values(Sorted, RandomPermutation).

key_random([], []).
key_random([H|T0], [K-H|T]) :-
        random(K),
        key_random(T0, T).

%%	partial_list(@Term) is semidet.
%
%	True if Term is a partial list.

partial_list(List) :-
	'$skip_list'(_, List, Tail),
	var(Tail).

:- multifile
	prolog:message//1.

prolog:message(random(no_gmp)) -->
	[ 'This version of SWI-Prolog is not compiled with GMP support.'-[], nl,
	  'Floating point random operations are not supported.'-[]
	].
