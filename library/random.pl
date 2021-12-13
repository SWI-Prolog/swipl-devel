/*  Part of SWI-Prolog

    Author:        R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, Universidade do Porto, University of Amsterdam,
                              VU University Amsterdam.
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

:- module(random,
          [ random/1,                   % -Float (0,1)
            random_between/3,           % +Low, +High, -Random

            getrand/1,                  % -State
            setrand/1,                  % +State

            maybe/0,
            maybe/1,                    % +P
            maybe/2,                    % +K, +N

            random_perm2/4,             % A,B, X,Y

            random_member/2,            % -Element, +List
            random_select/3,            % ?Element, +List, -Rest
            random_subseq/3,            % ?List, ?Subseq, ?Complement

            randseq/3,                  % +Size, +Max, -Set
            randset/3,                  % +Size, +Max, -List
            random_permutation/2,       % ?List, ?Permutation
            random_numlist/4,           % +P, +L, +U, -List

                                        % deprecated interface
            random/3                    % +Low, +High, -Random
          ]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(error),
	    [must_be/2,domain_error/2,instantiation_error/1]).
:- autoload(library(lists),[nth0/3,nth0/4,append/3]).
:- autoload(library(pairs),[pairs_values/2]).


/** <module> Random numbers

This library is derived from the DEC10   library random. Later, the core
random generator was moved to C. The current version uses the SWI-Prolog
arithmetic functions to realise this library.  These functions are based
on the GMP library.

@author         R.A. O'Keefe, V.S. Costa, L. Damas, Jan Wielemaker
@see            Built-in function random/1: A is random(10)
*/

check_gmp :-
    current_arithmetic_function(random_float),
    !.
check_gmp :-
    print_message(warning, random(no_gmp)).

:- initialization check_gmp.


                 /*******************************
                 *         PRIMITIVES           *
                 *******************************/

%!  random(-R:float) is det.
%
%   Binds R to a new random float in the _open_ interval (0.0,1.0).
%
%   @see setrand/1, getrand/1 may be used to fetch/set the state.
%   @see In SWI-Prolog, random/1 is implemented by the function
%        random_float/0.

random(R) :-
    R is random_float.

%!  random_between(+L:int, +U:int, -R:int) is semidet.
%
%   Binds R to a random integer in [L,U] (i.e., including both L and
%   U).  Fails silently if U<L.

random_between(L, U, R) :-
    integer(L), integer(U),
    !,
    U >= L,
    R is L+random((U+1)-L).
random_between(L, U, _) :-
    must_be(integer, L),
    must_be(integer, U).


%!  random(+L:int, +U:int, -R:int) is det.
%!  random(+L:float, +U:float, -R:float) is det.
%
%   Generate a random integer or float in a   range.  If L and U are
%   both integers, R is a random integer   in the half open interval
%   [L,U). If L and U are both  floats,   R  is  a float in the open
%   interval (L,U).
%
%   @deprecated Please use random/1 for   generating  a random float
%   and random_between/3 for generating a  random integer. Note that
%   random_between/3  includes  the  upper  bound,  while  this
%   predicate excludes it.

random(L, U, R) :-
    integer(L), integer(U),
    !,
    R is L+random(U-L).
random(L, U, R) :-
    number(L), number(U),
    !,
    R is L+((U-L)*random_float).
random(L, U, _) :-
    must_be(number, L),
    must_be(number, U).


                 /*******************************
                 *             STATE            *
                 *******************************/

%!  setrand(+State) is det.
%!  getrand(-State) is det.
%
%   Query/set the state of the random   generator.  This is intended
%   for  restarting  the  generator  at  a  known  state  only.  The
%   predicate  setrand/1  accepts  an  opaque    term   returned  by
%   getrand/1. This term may be  asserted,   written  and  read. The
%   application may not make other assumptions about this term.
%
%   For compatibility reasons with older   versions of this library,
%   setrand/1 also accepts a term rand(A,B,C), where  A, B and C are
%   integers in the range 1..30,000. This   argument is used to seed
%   the random generator.  Deprecated.
%
%   @see    set_random/1 and random_property/1 provide the SWI-Prolog
%           native implementation.
%   @error  existence_error(random_state, _) is raised if the
%           underlying infrastructure cannot fetch the random state.
%           This is currently the case if SWI-Prolog is not compiled
%           with the GMP library.

setrand(rand(A,B,C)) :-
    !,
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
                 *            MAYBE             *
                 *******************************/

%!  maybe is semidet.
%
%   Succeed/fail with equal probability (variant of maybe/1).

maybe :-
    random(2) =:= 0.

%!  maybe(+P) is semidet.
%
%   Succeed with probability P, fail with probability 1-P

maybe(P) :-
    must_be(between(0.0,1.0), P),
    random_float < P.

%!  maybe(+K, +N) is semidet.
%
%   Succeed with probability K/N (variant of maybe/1)

maybe(K, N) :-
    integer(K), integer(N),
    between(0, N, K),
    !,
    random(N) < K.
maybe(K, N) :-
    must_be(nonneg, K),
    must_be(nonneg, N),
    domain_error(not_less_than_zero,N-K).


                 /*******************************
                 *          PERMUTATION         *
                 *******************************/

%!  random_perm2(?A, ?B, ?X, ?Y) is semidet.
%
%   Does X=A,Y=B or X=B,Y=A with equal probability.

random_perm2(A,B, X,Y) :-
    (   maybe
    ->  X = A, Y = B
    ;   X = B, Y = A
    ).


                 /*******************************
                 *    SET AND LIST OPERATIONS   *
                 *******************************/

%!  random_member(-X, +List:list) is semidet.
%
%   X is a random member of   List.  Equivalent to random_between(1,
%   |List|), followed by nth1/3. Fails of List is the empty list.
%
%   @compat Quintus and SICStus libraries.

random_member(X, List) :-
    must_be(list, List),
    length(List, Len),
    Len > 0,
    N is random(Len),
    nth0(N, List, X).

%!  random_select(-X, +List, -Rest) is semidet.
%!  random_select(+X, -List, +Rest) is det.
%
%   Randomly select or insert an element.   Either List or Rest must
%   be a list.  Fails if List is the empty list.
%
%   @compat Quintus and SICStus libraries.

random_select(X, List, Rest) :-
    (   '$skip_list'(Len, List, Tail),
        Tail == []
    ->  true
    ;   '$skip_list'(RLen, Rest, Tail),
        Tail == []
    ->  Len is RLen+1
    ),
    !,
    Len > 0,
    N is random(Len),
    nth0(N, List, X, Rest).
random_select(_, List, Rest) :-
    partial_list(List), partial_list(Rest),
    instantiation_error(List+Rest).
random_select(_, List, Rest) :-
    must_be(list, List),
    must_be(list, Rest).

%!  random_subseq(+List, -Subseq, -Complement) is det.
%!  random_subseq(-List, +Subseq, +Complement) is semidet.
%
%   Selects a random subsequence Subseq of List, with Complement
%   containing all elements of List that were not selected.
%   Each element of List is included with equal probability in either
%   Subseq or Complement.
%
%   random_subseq/3 may also be called with Subseq and Complement bound
%   and List unbound, which will recreate List by randomly interleaving
%   Subseq and Complement. This mode may fail randomly, matching SICStus
%   behavior. The failure probability corresponds to the probability of
%   the "forward" mode selecting a Subseq/Complement combination with
%   different lengths.
%
%   @compat SICStus 4

random_subseq([], [], []).
random_subseq([Head|Tail], Subseq, Complement) :-
    (   maybe
    ->  Subseq = [Head|SubTail],
        Complement = CompTail
    ;   Subseq = SubTail,
        Complement = [Head|CompTail]
    ),
    random_subseq(Tail, SubTail, CompTail).

%!  randset(+K:int, +N:int, -S:list(int)) is det.
%
%   S is a sorted list of K unique   random  integers in the range 1..N.
%   The implementation uses different techniques  depending on the ratio
%   K/N. For small K/N it generates a   set of K random numbers, removes
%   the duplicates and adds more numbers until |S| is K. For a large K/N
%   it enumerates 1..N and decides  randomly   to  include the number or
%   not. For example:
%
%     ==
%     ?- randset(5, 5, S).
%     S = [1, 2, 3, 4, 5].          (always)
%     ?- randset(5, 20, S).
%     S = [2, 7, 10, 19, 20].
%     ==
%
%   @see randseq/3.

randset(K, N, S) :-
    must_be(nonneg, K),
    K =< N,
    (   K < N//7
    ->  randsetn(K, N, [], S)
    ;   randset(K, N, [], S)
    ).

randset(0, _, S, S) :- !.
randset(K, N, Si, So) :-
    random(N) < K,
    !,
    J is K-1,
    M is N-1,
    randset(J, M, [N|Si], So).
randset(K, N, Si, So) :-
    M is N-1,
    randset(K, M, Si, So).

randsetn(K, N, Sofar, S) :-
    length(Sofar, Len),
    (   Len =:= K
    ->  S = Sofar
    ;   Needed is K-Len,
        length(New, Needed),
        maplist(srand(N), New),
        (   Sofar == []
        ->  sort(New, Sorted)
        ;   append(New, Sofar, Sofar2),
            sort(Sofar2, Sorted)
        ),
        randsetn(K, N, Sorted, S)
    ).

srand(N, E) :-
    E is random(N)+1.

%!  randseq(+K:int, +N:int, -List:list(int)) is det.
%
%   S is a list of K unique random   integers in the range 1..N. The
%   order is random. Defined as
%
%     ```
%     randseq(K, N, List) :-
%           randset(K, N, Set),
%           random_permutation(Set, List).
%     ```
%
%   @see randset/3.

randseq(K, N, Seq) :-
    randset(K, N, Set),
    random_permutation_(Set, Seq).

%!  random_permutation(+List, -Permutation) is det.
%!  random_permutation(-List, +Permutation) is det.
%
%   Permutation is a random permutation of List. This is intended to
%   process the elements of List in   random order. The predicate is
%   symmetric.
%
%   @error instantiation_error, type_error(list, _).

random_permutation(List1, List2) :-
    is_list(List1),
    !,
    random_permutation_(List1, List2).
random_permutation(List1, List2) :-
    is_list(List2),
    !,
    random_permutation_(List2, List1).
random_permutation(List1, List2) :-
    partial_list(List1), partial_list(List2),
    !,
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

%!  random_numlist(+P, +L, +U, -List) is det.
%
%   Unify List with an ascending list of integers between L and U
%   (inclusive). Each integer in the range L..U is included with
%   probability P.
%
%   @compat SICStus 4

random_numlist(P, L, U, List) :-
    must_be(between(0.0, 1.0), P),
    must_be(integer, L),
    must_be(integer, U),
    random_numlist_(P, L, U, List).
random_numlist_(_P, L, U, List) :-
    L > U,
    !,
    List = [].
random_numlist_(P, L, U, List) :-
    (   maybe(P)
    ->  List = [L|Tail]
    ;   List = Tail
    ),
    L1 is L + 1,
    random_numlist_(P, L1, U, Tail).

%!  partial_list(@Term) is semidet.
%
%   True if Term is a partial list.

partial_list(List) :-
    '$skip_list'(_, List, Tail),
    var(Tail).

:- multifile
    prolog:message//1.

prolog:message(random(no_gmp)) -->
    [ 'This version of SWI-Prolog is not compiled with GMP support.'-[], nl,
      'Floating point random operations are not supported.'-[]
    ].
