/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

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
*/

%% Migrated from Ciao to SWI-Prolog

:- module(hiordlib, [map/3, map/4, map/5, map/6, minimum/3, split/4],
	    [assertions, basicmodes, dcg, % nativeprops, fsyntax,
	     hiord, unittestdecls]).

% :- fun_eval(arith(true)).

% BUG: foldl already implemented in apply.pl, but with other arrange
% of arguments.

:- reexport(library(apply), [foldl/4]).

:- doc(title, "Higher-order predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").
:- doc(author, "Edison Mera").

:- doc(module, "This library implements a few basic higher-order
   predicates. These add functionality to the basic 
   higher-order functionality of Ciao. Examples of the latter are:


   Using pred(1):

@begin{verbatim}
  list(L, functor(_,2))
  list(L, >(0))
@end{verbatim}

   Using pred(2):

").

:- pred map(LList, Op, RList) # "Examples of use:
@begin{verbatim}
  map([1,3,2], arg(f(a,b,c,d)), [a,c,b]) or
  map([1,3,2], nth([a,b,c,d]), [a,c,b])
  map([\"D\",\"C\"], append(\".\"), [\"D.\",\"C.\"])
@end{verbatim}
".

:- load_test_module(library(lists), [nth/3, append/3]).

:- test map(A, B, C) : (A = [1, 3, 2], B = arg(f(a, b, c, d)))
	=> (C = [a, c, b]) + (not_fails, is_det).

:- test map(A, B, C) : (A = [1, 3, 2], B = nth([a, b, c, d]))
	=> (C = [a, c, b]) + (not_fails, is_det).

:- test map(A, B, C) : (A = ["D", "C"], B = append("."))
	=> (C = ["D.", "C."]) + (not_fails, is_det).

:- meta_predicate map(_, pred(2), _).

% map([],     _) := [].
% map([X|Xs], P) := [~P(X) |~map(Xs, P)].

map(L, P, R) :- maplist(P, L, R).

:- pred map(LList, Op, RList, Tail) # "DCG version of map.".

:- test map(A, B, C, D) :
	(
	    A = [1, 3, 2],
	    B = (''(L, [E|T], T) :- arg(L, f(a, b, c, d), E)),
	    D = [x, y])
	=> (C = [a, c, b, x, y]) + (not_fails, is_det).

:- meta_predicate map(?, pred(3), ?, ?).

map([],     _) --> [].
map([X|Xs], P) --> call(P, X), map(Xs, P).

:- meta_predicate map(?, ?, pred(4), ?, ?).

map([],     [], _) --> [].
map([X|Xs], [Y|Ys], P) --> call(P, X, Y), map(Xs, Ys, P).

:- meta_predicate map(?, ?, ?, pred(5), ?, ?).

map([],     [],     [],     _) --> [].
map([X|Xs], [Y|Ys], [Z|Zs], P) --> call(P, X, Y, Z), map(Xs, Ys, Zs, P).

:- meta_predicate minimum(_, pred(2), _).

:- pred minimum(?List, +SmallerThan, ?Minimum) : list * callable *
	term # "@var{Minimum} is the smaller in the nonempty list
	@var{List} according to the relation @var{SmallerThan}:
	@pred{SmallerThan(X, Y)} succeeds iff X is smaller than Y.".

minimum([X|Xs], Pred, Min) :- minimum_carry(Xs, Pred, X, Min).
minimum_carry([],     _Pred, M,        M).
minimum_carry([X|Xs], Pred,  MinSoFar, Min) :-
	(
	    call(Pred, MinSoFar, X) ->
	    minimum_carry(Xs, Pred, MinSoFar, Min)
	;
	    minimum_carry(Xs, Pred, X, Min)
	).

:- pred split(+List, +Condition, ?Left, ?Right) : ( list * callable *
	    term * term ) => (list * callable * list * list) # "Divides
	@var{List} in two list, where @var{Left} contains the elements
	for which the call to @var{Condition} succeeds, and @var{Right} the
	remaining elements.".

:-test split(A, B, C, D)
	: (A=[1, 2, 3, 4, 5, 6], B= '>'(4))
	=> (C=[5, 6], D=[1, 2, 3, 4])
	+ not_fails.

:- meta_predicate split(?, pred(1), ?, ?).

split([],    _,         [],    []).
split([E|R], Condition, Left0, Right0) :-
	(
	    call(Condition, E) ->
	    Left0 = [E|Left],
	    Right0 = Right
	;
	    Left0 = Left,
	    Right0 = [E|Right]
	),
	split(R, Condition, Left, Right).
