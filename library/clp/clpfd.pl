/*  $Id$

    Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, Markus Triska

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Thanks to Tom Schrijvers for his "bounds.pl", the first finite
   domain constraint solver included with SWI-Prolog. I've learned a
   lot from it and could even use some of the code for this solver.
   The propagation queue idea is taken from "prop.pl", a prototype
   solver also written by Tom. Highlights of the present solver:

   Symbolic constants for infinities
   ---------------------------------

   ?- X in 0..5 \/ 10..sup, Y #= -X, Z #= X + Y.
   %@ X = _G109{0..5 \/ 10..sup},
   %@ Y = _G114{inf..-10 \/ -5..0},
   %@ Z = _G120{inf..sup}

   No artificial limits (using GMP)
   ---------------------------------

   ?- N is 2^66, X #\= N.
   %@ X = _G1676{inf..73786976294838206463 \/ 73786976294838206465..sup}

   Often stronger propagation
   ---------------------------------

   ?- Y #= abs(X), Y #\= 3, Z * Z #= 4, A in 0..10, A mod 2 #= 0.
   %@ Y = _G1448{0..2 \/ 4..sup},
   %@ X = _G1446{inf..-4 \/ -2..2 \/ 4..sup},
   %@ Z = _G1454{-2 \/ 2},
   %@ A = _G1463{0 \/ 2 \/ 4 \/ 6 \/ 8 \/ 10}

   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Many things can be improved; if you want to help, feel free to
   e-mail me. A good starting point is taking a propagation algorithm
   from the literature and adding it.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(clpfd, [
                  op(760, yfx, (#<==>)),
                  op(750, xfy, (#==>)),
                  op(750, yfx, (#<==)),
                  op(740, yfx, (#\/)),
                  op(730, yfx, (#\)),
                  op(720, yfx, (#/\)),
                  op(710,  fy, (#\)),
                  op(700, xfx, (#>)),
                  op(700, xfx, (#<)),
                  op(700, xfx, (#>=)),
                  op(700, xfx, (#=<)),
                  op(700, xfx, (#=)),
                  op(700, xfx, (#\=)),
                  op(700, xfx, in),
                  op(700, xfx, ins),
                  op(450, xfx, (..)), % should bind more tightly than \/
                  (#>)/2,
                  (#<)/2,
                  (#>=)/2,
                  (#=<)/2,
                  (#=)/2,
                  (#\=)/2,
                  (#<==>)/2,
                  (#==>)/2,
                  (#<==)/2,
                  (#\/)/2,
                  (#/\)/2,
                  in/2,
                  ins/2,
                  all_different/1,
                  all_distinct/1,
                  sum/3,
                  tuples_in/2,
                  labeling/2,
                  label/1,
                  indomain/1,
                  lex_chain/1
                 ]).


:- use_module(library(error)).

:- op(700, xfx, cis).
:- op(700, xfx, cis1).
:- op(700, xfx, cis_geq).
:- op(700, xfx, cis_gt).
:- op(700, xfx, cis_leq).
:- op(700, xfx, cis_lt).

/** <module> Constraint Logic Programming over Finite Domains

Constraint programming is a declarative formalism that lets you
describe conditions a solution should satisfy. This library provides
CLP(FD), Constraint Logic Programming over Finite Domains. It can be
used to model and solve various combinatorial problems from diverse
areas such as planning, scheduling, and graph colouring.

As an example, consider the cryptoarithmetic puzzle SEND + MORE =
MONEY, where different letters denote distinct integers between 0 and
9. It can be modeled in CLP(FD) as follows:

==
:- use_module(library(clpfd)).

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
        Vars = [S,E,N,D,M,O,R,Y], Vars ins 0..9,
        all_different(Vars),
                  S*1000 + E*100 + N*10 + D +
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
        M #> 0, S #> 0,
        label(Vars).

?- puzzle(P).
P = ([9, 5, 6, 7]+[1, 0, 8, 5]=[1, 0, 6, 5, 2])
==

Most predicates of this library are _constraints_: They generalise
arithmetic evaluation of integer expressions in that propagation can
proceed in all directions. This library also provides _enumeration
predicates_, which let you systematically search for solutions on
variables whose domains have become finite.

A finite domain _expression_ is one of:

    * an integer
    * a variable
    * -Expr
    * Expr + Expr
    * Expr * Expr
    * Expr - Expr.
    * min(Expr,Expr)
    * max(Expr,Expr)
    * Expr mod Expr
    * abs(Expr)
    * Expr / Expr
      (integer division)

The most important finite domain _constraints_ are:

    * Expr #>= Expr
    * Expr #=< Expr
    * Expr #=  Expr
    * Expr #\= Expr
    * Expr #> Expr
    * Expr #< Expr

The constraints #=/2, #\=/2, #</2, #>/2, #=</2, #>=/2 can be
_reified_, which means reflecting their truth values into Boolean
variables. Let P and Q denote conjunctions ((#/\)/2) or disjunctions
((#\/)/2) of reifiable constraints or Boolean variables, then:

    * P #<==> Q
    True iff P and Q are equivalent.
    * P #==> Q
    True iff P implies Q.
    * P #<== Q
    True iff Q implies P.

If SWI Prolog is compiled with support for arbitrary precision
integers (using GMP), there is no limit on the size of domains.

@author Markus Triska
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A bound is either:

   n(N):    integer N
   inf:     infimum of Z (= negative infinity)
   sup:     supremum of Z (= positive infinity)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_bound(n(N)) :- integer(N).
is_bound(inf).
is_bound(sup).

defaulty_to_bound(D, P) :- ( integer(D) -> P = n(D) ; P = D ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Compactified is/2 and predicates for several arithmetic expressions
   with infinities, tailored for the modes needed by this solver.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% cis_gt only works for terms of depth 0 on both sides
cis_gt(n(N), B) :- cis_gt_numeric(B, N).
cis_gt(sup, B0) :-B0 \== sup.

cis_gt_numeric(n(B), A) :- A > B.
cis_gt_numeric(inf, _).

cis_geq(A, B) :-
        (   cis_gt(A, B) -> true
        ;   A == B -> true
        ).

cis_geq_zero(sup).
cis_geq_zero(n(N)) :- N >= 0.

cis_lt(A, B)  :- cis_gt(B, A).

cis_leq(A, B) :- cis_geq(B, A).

cis_min(inf, _, inf).
cis_min(sup, B, B).
cis_min(n(N), B, Min) :- cis_min_(B, N, Min).

cis_min_(inf, _, inf).
cis_min_(sup, N, n(N)).
cis_min_(n(B), A, n(M)) :- M is min(A,B).

cis_max(sup, _, sup).
cis_max(inf, B, B).
cis_max(n(N), B, Max) :- cis_max_(B, N, Max).

cis_max_(inf, N, n(N)).
cis_max_(sup, _, sup).
cis_max_(n(B), A, n(M)) :- M is max(A,B).

cis_plus(inf, _, inf).
cis_plus(sup, _, sup).
cis_plus(n(A), B, Plus) :- cis_plus_(B, A, Plus).

cis_plus_(sup, _, sup).
cis_plus_(inf, _, inf).
cis_plus_(n(B), A, n(S)) :- S is A + B.

cis_minus(inf, _, inf).
cis_minus(sup, _, sup).
cis_minus(n(A), B, M) :- cis_minus_(B, A, M).

cis_minus_(inf, _, sup).
cis_minus_(sup, _, inf).
cis_minus_(n(B), A, n(M)) :- M is A - B.

cis_uminus(inf, sup).
cis_uminus(sup, inf).
cis_uminus(n(A), n(B)) :- B is -A.

cis_abs(inf, sup).
cis_abs(sup, sup).
cis_abs(n(A), n(B)) :- B is abs(A).

cis_times(inf, B, P) :-
        (   B cis_lt n(0) -> P = sup
        ;   B cis_gt n(0) -> P = inf
        ;   P = n(0)
        ).
cis_times(sup, B, P) :-
        (   B cis_gt n(0) -> P = sup
        ;   B cis_lt n(0) -> P = inf
        ;   B == n(0) -> P = n(0)
        ).
cis_times(n(N), B, P) :- cis_times_(B, N, P).

cis_times_(inf, A, P) :- cis_times(inf, n(A), P).
cis_times_(sup, A, P) :- cis_times(sup, n(A), P).
cis_times_(n(B), A, n(P)) :- P is A * B.

% compactified is/2 for expressions of interest
A cis B :- cis_(B, A).

cis_(n(N), n(N)).
cis_(inf, inf).
cis_(sup, sup).
cis_(A0+B0, E)       :- cis_(A0, A), cis_(B0, B), cis_plus(A, B, E).
cis_(abs(A0), E)     :- cis_(A0, A), cis_abs(A, E).
cis_(min(A0,B0), E)  :- cis_(A0, A), cis_(B0, B), cis_min(A, B, E).
cis_(max(A0,B0), E)  :- cis_(A0, A), cis_(B0, B), cis_max(A, B, E).
cis_(A0-B0, E)       :- cis_(A0, A), cis_(B0, B), cis_minus(A, B, E).
cis_(-A0, E)         :- cis_(A0, A), cis_uminus(A, E).
cis_(A0*B0, E)       :- cis_(A0, A), cis_(B0, B), cis_times(A, B, E).
cis_(floor(A0), E)   :- cis_(A0, A), cis_floor(A, E).
cis_(ceiling(A0), E) :- cis_(A0, A), cis_ceiling(A, E).
cis_(div(A0,B0), E)  :- cis_(A0, A), cis_(B0, B), cis_div(A, B, E).
cis_(A0//B0, E)      :- cis_(A0, A), cis_(B0, B), cis_slash(A, B, E).

% special case for the frequent case of depth 1 expressions

A cis1 B :- cis1_(B, A).

cis1_(n(N), n(N)).
cis1_(inf, inf).
cis1_(sup, sup).
cis1_(A+B, E)        :- cis_plus(A, B, E).
cis1_(abs(A), E)     :- cis_abs(A, E).
cis1_(min(A,B), E)   :- cis_min(A, B, E).
cis1_(max(A,B), E)   :- cis_max(A, B, E).
cis1_(A-B, E)        :- cis_minus(A, B, E).
cis1_(-A, E)         :- cis_uminus(A, E).
cis1_(A*B, E)        :- cis_times(A, B, E).
cis1_(floor(A), E)   :- cis_floor(A, E).
cis1_(ceiling(A), E) :- cis_ceiling(A, E).
cis1_(div(A,B), E)   :- cis_div(A, B, E).
cis1_(A//B, E)       :- cis_slash(A, B, E).

cis_floor(sup, sup).
cis_floor(inf, inf).
cis_floor(n(A), n(B)) :- B is floor(A).

cis_ceiling(sup, sup).
cis_ceiling(inf, inf).
cis_ceiling(n(A), n(B)) :- B is ceiling(A).

cis_div(sup, Y, Z) :- ( cis_geq_zero(Y) -> Z = sup ; Z = inf ).
cis_div(inf, Y, Z) :- ( cis_geq_zero(Y) -> Z = inf ; Z = sup ).
cis_div(n(X), Y, Z) :- cis_div_(Y, X, Z).

cis_div_(sup, _, n(0)).
cis_div_(inf, _, n(0)).
cis_div_(n(Y), X, Z) :-
        (   Y =:= 0 -> (  X >= 0 -> Z = sup ; Z = inf )
        ;   Z0 is X / Y, Z = n(Z0)
        ).

cis_slash(sup, _, sup).
cis_slash(inf, _, inf).
cis_slash(n(N), B, S) :- cis_slash_(B, N, S).

cis_slash_(sup, _, 0).
cis_slash_(inf, _, 0).
cis_slash_(n(B), A, n(S)) :- S is A // B.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A domain is a finite set of disjoint intervals. Internally, domains
   are represented as trees. Each node is one of:

   empty: empty domain.

   split(N, Left, Right)
      - split on integer N, with Left and Right domains whose elements are
        all less than and greater than N, respectively. The domain is the
        union of Left and Right, i.e., N is a hole.

   from_to(From, To)
      - interval (From-1, To+1); From and To are bounds

   Desiderata: rebalance domains; singleton intervals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Type definition and inspection of domains.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

check_domain(D, Line) :-
        (   ground(D), is_domain(D) -> true
        ;   format("~w: invalid domain ~w\n", [Line,D]),
            fail
        ).

is_domain(empty).
is_domain(from_to(From,To)) :-
        is_bound(From), is_bound(To),
        From cis_leq To.
is_domain(split(S, Left, Right)) :-
        integer(S),
        is_domain(Left), is_domain(Right),
        all_less_than(Left, S),
        all_greater_than(Right, S).

all_less_than(empty, _).
all_less_than(from_to(From,To), S) :-
        From cis_lt n(S), To cis_lt n(S).
all_less_than(split(S0,Left,Right), S) :-
        S0 < S,
        all_less_than(Left, S),
        all_less_than(Right, S).

all_greater_than(empty, _).
all_greater_than(from_to(From,To), S) :-
        From cis_gt n(S), To cis_gt n(S).
all_greater_than(split(S0,Left,Right), S) :-
        S0 > S,
        all_greater_than(Left, S),
        all_greater_than(Right, S).

default_domain(from_to(inf,sup)).

domain_infimum(empty, _) :- format("infimum of empty domain"), fail.
domain_infimum(from_to(I, _), I).
domain_infimum(split(_, Left, _), I) :- domain_infimum(Left, I).

domain_supremum(empty, _) :- format("supremum of empty domain"), fail.
domain_supremum(from_to(_, S), S).
domain_supremum(split(_, _, Right), S) :- domain_supremum(Right, S).

domain_num_elements(empty, n(0)).
domain_num_elements(from_to(From,To), Num) :- Num cis To - From + n(1).
domain_num_elements(split(_, Left, Right), Num) :-
        domain_num_elements(Left, NL),
        domain_num_elements(Right, NR),
        Num cis1 NL + NR.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Test whether domain contains a given integer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_contains(from_to(From,To), I) :- From cis_leq n(I), n(I) cis_leq To.
domain_contains(split(S, Left, Right), I) :-
        (   I < S -> domain_contains(Left, I)
        ;   I > S -> domain_contains(Right, I)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Test whether a domain contains another domain.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_subdomain(Dom, Sub) :- domain_subdomain(Dom, Dom, Sub).

domain_subdomain(from_to(_,_), Dom, Sub) :-
        domain_subdomain_fromto(Sub, Dom).
domain_subdomain(split(_, _, _), Dom, Sub) :-
        domain_subdomain_split(Sub, Dom, Sub).

domain_subdomain_split(empty, _, _).
domain_subdomain_split(from_to(From,To), split(S,Left0,Right0), Sub) :-
        (   To cis_lt n(S) -> domain_subdomain(Left0, Left0, Sub)
        ;   From cis_gt n(S) -> domain_subdomain(Right0, Right0, Sub)
        ).
domain_subdomain_split(split(_,Left,Right), Dom, _) :-
        domain_subdomain(Dom, Dom, Left),
        domain_subdomain(Dom, Dom, Right).

domain_subdomain_fromto(empty, _).
domain_subdomain_fromto(from_to(From,To), from_to(From0,To0)) :-
        From0 cis_leq From, To0 cis_geq To.
domain_subdomain_fromto(split(_,Left,Right), Dom) :-
        domain_subdomain_fromto(Left, Dom),
        domain_subdomain_fromto(Right, Dom).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Remove an integer from a domain. The domain is traversed until an
   interval is reached from which the element can be removed, or until
   it is clear that no such interval exists.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_remove(empty, _, empty).
domain_remove(from_to(L0, U0), X, D) :- domain_remove_(L0, U0, X, D).
domain_remove(split(S, Left0, Right0), X, D) :-
        (   X =:= S -> D = split(S, Left0, Right0)
        ;   X < S ->
            domain_remove(Left0, X, Left1),
            (   Left1 == empty -> D = Right0
            ;   D = split(S, Left1, Right0)
            )
        ;   domain_remove(Right0, X, Right1),
            (   Right1 == empty -> D = Left0
            ;   D = split(S, Left0, Right1)
            )
        ).

%?- domain_remove(from_to(n(0),n(5)), 3, D).

domain_remove_(inf, U0, X, D) :-
        (   U0 == n(X) -> U1 cis1 U0 - n(1), D = from_to(inf, U1)
        ;   U0 cis_lt n(X) -> D = from_to(inf,U0)
        ;   L1 cis1 n(X) + n(1), U1 cis1 n(X) - n(1),
            D = split(X, from_to(inf, U1), from_to(L1,U0))
        ).
domain_remove_(n(N), U0, X, D) :- domain_remove_upper(U0, n(N), X, D).

domain_remove_upper(sup, L0, X, D) :-
        (   L0 == n(X) -> L1 cis1 n(X) + n(1), D = from_to(L1,sup)
        ;   L0 cis_gt n(X) -> D = from_to(L0,sup)
        ;   L1 cis1 n(X) + n(1), U1 cis1 n(X) - n(1),
            D = split(X, from_to(L0,U1), from_to(L1,sup))
        ).
domain_remove_upper(n(U00), L0, X, D) :-
        U0 = n(U00),
        (   L0 == U0, n(X) == L0 -> D = empty
        ;   L0 == n(X) -> L1 cis1 n(X) + n(1), D = from_to(L1, U0)
        ;   U0 == n(X) -> U1 cis1 U0 - n(1), D = from_to(L0, U1)
        ;   L0 cis_leq n(X), n(X) cis_leq U0 ->
            U1 cis1 n(X) - n(1), L1 cis1 n(X) + n(1),
            D = split(X, from_to(L0, U1), from_to(L1, U0))
        ;   D = from_to(L0,U0)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Remove all elements greater than / less than some constant.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_remove_greater_than(empty, _, empty).
domain_remove_greater_than(from_to(From0,To0), G, D) :-
        (   From0 cis_gt n(G) -> D = empty
        ;   To cis1 min(To0,n(G)), D = from_to(From0,To)
        ).
domain_remove_greater_than(split(S,Left0,Right0), G, D) :-
        (   S =< G ->
            domain_remove_greater_than(Right0, G, Right),
            (   Right == empty -> D = Left0
            ;   D = split(S, Left0, Right)
            )
        ;   domain_remove_greater_than(Left0, G, D)
        ).

domain_remove_smaller_than(empty, _, empty).
domain_remove_smaller_than(from_to(From0,To0), V, D) :-
        (   To0 cis_lt n(V) -> D = empty
        ;   From cis1 max(From0,n(V)), D = from_to(From,To0)
        ).
domain_remove_smaller_than(split(S,Left0,Right0), V, D) :-
        (   S >= V ->
            domain_remove_smaller_than(Left0, V, Left),
            (   Left == empty -> D = Right0
            ;   D = split(S, Left, Right0)
            )
        ;   domain_remove_smaller_than(Right0, V, D)
        ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Remove a whole domain from another domain. (Set difference.)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_subtract(Dom0, Sub, Dom) :- domain_subtract(Dom0, Dom0, Sub, Dom).

domain_subtract(empty, _, _, empty).
domain_subtract(from_to(From0,To0), Dom, Sub, D) :-
        (   Sub == empty -> D = Dom
        ;   Sub = from_to(From,To) ->
            (   From == To -> From = n(X), domain_remove(Dom, X, D)
            ;   From cis_gt To0 -> D = Dom
            ;   To cis_lt From0 -> D = Dom
            ;   From cis_leq From0 ->
                (   To cis_geq To0 -> D = empty
                ;   From1 cis1 To + n(1),
                    D = from_to(From1, To0)
                )
            ;   To1 cis1 From - n(1),
                (   To cis_lt To0 ->
                    From = n(S),
                    From2 cis1 To + n(1),
                    D = split(S,from_to(From0,To1),from_to(From2,To0))
                ;   D = from_to(From0,To1)
                )
            )
        ;   Sub = split(S, Left, Right) ->
            (   n(S) cis_gt To0 -> domain_subtract(Dom, Dom, Left, D)
            ;   n(S) cis_lt From0 -> domain_subtract(Dom, Dom, Right, D)
            ;   domain_subtract(Dom, Dom, Left, D1),
                domain_subtract(D1, D1, Right, D)
            )
        ).
domain_subtract(split(S, Left0, Right0), _, Sub, D) :-
        domain_subtract(Left0, Left0, Sub, Left),
        domain_subtract(Right0, Right0, Sub, Right),
        (   Left == empty -> D = Right
        ;   Right == empty -> D = Left
        ;   D = split(S, Left, Right)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Convert domain to a list of disjoint intervals From-To.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_intervals(D, Is) :- domain_intervals(D, Is, []).

domain_intervals(split(_, Left, Right)) -->
        domain_intervals(Left), domain_intervals(Right).
domain_intervals(empty)                 --> [].
domain_intervals(from_to(From,To))      --> [From-To].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   To compute the intersection of two domains D1 and D2, we
   (arbitrarily) choose D1 as the reference domain. For each interval
   of D1, we compute how far and to which values D2 lets us extend it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domains_intersection(D1, D2, Intersection) :-
        domains_intersection_(D1, D2, Intersection),
        Intersection \== empty.

domains_intersection_(empty, _, empty).
domains_intersection_(from_to(L0,U0), D2, Dom) :-
        narrow(D2, L0, U0, Dom).
domains_intersection_(split(S,Left0,Right0), D2, Dom) :-
        domains_intersection_(Left0, D2, Left1),
        domains_intersection_(Right0, D2, Right1),
        (   Left1 == empty, Right1 == empty -> Dom = empty
        ;   Left1 == empty -> Dom = Right1
        ;   Right1 == empty -> Dom = Left1
        ;   Dom = split(S, Left1, Right1)
        ).

narrow(empty, _, _, empty).
narrow(from_to(L0,U0), From0, To0, Dom) :-
        From1 cis1 max(From0,L0), To1 cis1 min(To0,U0),
        (   From1 cis_gt To1 -> Dom = empty
        ;   Dom = from_to(From1,To1)
        ).
narrow(split(S, Left0, Right0), From0, To0, Dom) :-
        (   To0 cis_lt n(S) -> narrow(Left0, From0, To0, Dom)
        ;   From0 cis_gt n(S) -> narrow(Right0, From0, To0, Dom)
        ;   narrow(Left0, From0, To0, Left1),
            narrow(Right0, From0, To0, Right1),
            (   Left1 == empty -> Dom = Right1
            ;   Right1 == empty -> Dom = Left1
            ;   Dom = split(S, Left1, Right1)
            )
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Union of 2 domains.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domains_union(D1, D2, Union) :-
        domain_intervals(D1, Is1),
        domain_intervals(D2, Is2),
        append(Is1, Is2, IsU0),
        merge_intervals(IsU0, IsU1),
        intervals_to_domain(IsU1, Union).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Shift a domain by some offset.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_shift(empty, _, empty).
domain_shift(from_to(From0,To0), O, from_to(From,To)) :-
        From cis1 From0 + n(O), To cis1 To0 + n(O).
domain_shift(split(S0, Left0, Right0), O, split(S, Left, Right)) :-
        S is S0 + O,
        domain_shift(Left0, O, Left),
        domain_shift(Right0, O, Right).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The new domain contains all values of the old domain,
   multiplied by a constant multiplier.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_expand(D0, M, D) :-
        (   M < 0 -> domain_negate(D0, D1), M1 is abs(M)
        ;   D1 = D0, M1 = M
        ),
        domain_expand_(D1, M1, D).

domain_expand_(empty, _, empty).
domain_expand_(from_to(From0, To0), M, D) :-
        (   M =:= 1 -> D = from_to(From0, To0)
        ;   From0 == inf -> To cis1 To0*n(M), D = from_to(inf, To)
        ;   To0 == sup -> From cis1 From0*n(M), D = from_to(From, sup)
        ;   % domain is bounded
            To1 cis1 To0 + n(1),
            First cis1 From0*n(M),
            D0 = from_to(First,First),
            From1 cis1 From0 + n(1),
            From1 = n(NFrom1), To1 = n(NTo1),
            all_multiples(NFrom1, NTo1, M, D0, D)
        ).
domain_expand_(split(S0, Left0, Right0), M, split(S, Left, Right)) :-
        S is M*S0,
        domain_expand_(Left0, M, Left),
        domain_expand_(Right0, M, Right).

all_multiples(N, N, _, D, D) :- !.
all_multiples(N0, N, M, D0, D) :-
        Mult is N0*M,
        S is Mult -  1,
        NMult = n(Mult),
        D1 = split(S, D0, from_to(NMult,NMult)),
        N1 is N0 + 1,
        all_multiples(N1, N, M, D1, D).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   similar to domain_expand/3, tailored for division: an interval
   [From,To] is extended to [From*M, ((To+1)*M - 1)], i.e., to all
   values that integer-divided by M yield a value from interval.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_expand_more(D0, M, D) :-
        %format("expanding ~w by ~w\n", [D0,M]),
        (   M < 0 -> domain_negate(D0, D1), M1 is abs(M)
        ;   D1 = D0, M1 = M
        ),
        domain_expand_more_(D1, M1, D).
        %format("yield: ~w\n", [D]).

domain_expand_more_(empty, _, empty).
domain_expand_more_(from_to(From0, To0), M, from_to(From,To)) :-
        (   From0 cis_lt n(0) ->
            From cis (From0-n(1))*n(M) + n(1)
        ;   From cis From0*n(M)
        ),
        To cis (To0+n(1))*n(M) - n(1).
domain_expand_more_(split(S0, Left0, Right0), M, D) :-
        S is M*S0,
        domain_expand_more_(Left0, M, Left),
        domain_expand_more_(Right0, M, Right),
        domain_supremum(Left, LeftSup),
        domain_infimum(Right, RightInf),
        (   LeftSup cis_lt n(S), n(S) cis_lt RightInf ->
            D = split(S, Left, Right)
        ;   domain_infimum(Left, Inf),
            domain_supremum(Right, Sup),
            D = from_to(Inf, Sup)
        ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Scale a domain down by a constant multiplier. Assuming (//)/2.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_contract(D0, M, D) :-
        %format("contracting ~w by ~w\n", [D0,M]),
        (   M < 0 -> domain_negate(D0, D1), M1 is abs(M)
        ;   D1 = D0, M1 = M
        ),
        domain_contract_(D1, M1, D).

domain_contract_(empty, _, empty).
domain_contract_(from_to(From0, To0), M, from_to(From,To)) :-
        From cis1 From0 // n(M), To cis1 To0 // n(M).
domain_contract_(split(S0,Left0,Right0), M, D) :-
        S is S0 // M,
        %  Scaled down domains do not necessarily retain any holes of
        %  the original domain.
        domain_contract_(Left0, M, Left),
        domain_contract_(Right0, M, Right),
        domain_supremum(Left, LeftSup),
        domain_infimum(Right, RightInf),
        (   LeftSup cis_lt n(S), n(S) cis_lt RightInf ->
            D = split(S, Left, Right)
        ;   domain_infimum(Left0, Inf),
            % TODO: this is not necessarily an interval
            domain_supremum(Right0, Sup),
            min_divide(Inf, Sup, n(M), n(M), From0),
            max_divide(Inf, Sup, n(M), n(M), To0),
            domain_infimum(Left, LeftInf),
            domain_supremum(Right, RightSup),
            From cis max(LeftInf, ceiling(From0)),
            To cis min(RightSup, floor(To0)),
            D = from_to(From, To)
        ).
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Similar to domain_contract, tailored for division, i.e.,
   {21,23} contracted by 4 is 5. It contracts "less".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_contract_less(D0, M, D) :-
        (   M < 0 -> domain_negate(D0, D1), M1 is abs(M)
        ;   D1 = D0, M1 = M
        ),
        (   domain_infimum(D0, n(_)), domain_supremum(D0, n(_)) ->
            % bounded domain
            domain_intervals(D0, Is),
            intervals_contract_less(Is, M, Cs, []),
            list_to_domain(Cs, D)
        ;   domain_contract_less_(D0, M, D)
        ).

intervals_contract_less([], _)               --> [].
intervals_contract_less([n(A0)-n(B0)|Is], M) -->
        { A is A0 // M, B is B0 // M, numlist(A, B, Ns) },
        dlist(Ns),
        intervals_contract_less(Is, M).

domain_contract_less_(empty, _, empty).
domain_contract_less_(from_to(From0, To0), M, from_to(From,To)) :-
        From cis1 From0 // n(M), To cis1 To0 // n(M).
domain_contract_less_(split(S0,Left0,Right0), M, D) :-
        S is S0 // M,
        %  Scaled down domains do not necessarily retain any holes of
        %  the original domain.
        domain_contract_less_(Left0, M, Left),
        domain_contract_less_(Right0, M, Right),
        domain_supremum(Left, LeftSup),
        domain_infimum(Right, RightInf),
        (   LeftSup cis_lt n(S), n(S) cis_lt RightInf ->
            D = split(S, Left, Right)
        ;   domain_infimum(Left0, Inf),
            % TODO: this is not necessarily an interval
            domain_supremum(Right0, Sup),
            min_divide(Inf, Sup, n(M), n(M), From0),
            max_divide(Inf, Sup, n(M), n(M), To0),
            domain_infimum(Left, LeftInf),
            domain_supremum(Right, RightSup),
            From cis max(LeftInf, floor(From0)),
            To cis min(RightSup, ceiling(To0)),
            D = from_to(From, To)
            %format("got: ~w\n", [D])
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Negate the domain. Left and Right sub-domains and bounds switch sides.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_negate(empty, empty).
domain_negate(from_to(From0, To0), from_to(To,From)) :-
        From cis1 -From0, To cis1 -To0.
domain_negate(split(S0, Left0, Right0), split(S, Left, Right)) :-
        S is -S0,
        domain_negate(Left0, Right),
        domain_negate(Right0, Left).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Construct a domain from a list of integers. Try to balance it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

list_to_disjoint_intervals([], []).
list_to_disjoint_intervals([N|Ns], Is) :-
        list_to_disjoint_intervals(Ns, N, N, Is).

list_to_disjoint_intervals([], M, N, [n(M)-n(N)]).
list_to_disjoint_intervals([B|Bs], M, N, Is) :-
        (   B =:= N + 1 ->
            list_to_disjoint_intervals(Bs, M, B, Is)
        ;   Is = [n(M)-n(N)|Rest],
            list_to_disjoint_intervals(Bs, B, B, Rest)
        ).

list_to_domain(List0, D) :-
        (   List0 == [] -> D = empty
        ;   sort(List0, List),
            list_to_disjoint_intervals(List, Is),
            intervals_to_domain(Is, D)
        ).

intervals_to_domain([], empty) :- !.
intervals_to_domain([M-N], from_to(M,N)) :- !.
intervals_to_domain(Is, D) :-
        length(Is, L),
        FL is floor(L / 2),
        length(Front, FL),
        append(Front, Tail, Is),
        Tail = [n(Start)-_|_],
        Hole is Start - 1,
        intervals_to_domain(Front, Left),
        intervals_to_domain(Tail, Right),
        D = split(Hole, Left, Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% indomain(?Var)
%
% Bind 'Var' to all feasible values of its domain on backtracking. The
% domain of 'Var' must be finite.

indomain(Var) :-
        (   var(Var) ->
            finite_domain(Var),
            indomain_(up, Var)
        ;   must_be(integer, Var)
        ).

indomain_(Order, Var) :-
        (   var(Var) ->
            get(Var, Dom, _),
            order_dom_next(Order, Dom, Next),
            (   Var = Next
            ;   Var #\= Next,
                indomain_(Order, Var)
            )
        ;   must_be(integer, Var)
        ).

order_dom_next(up, Dom, Next)   :- domain_infimum(Dom, n(Next)).
order_dom_next(down, Dom, Next) :- domain_supremum(Dom, n(Next)).


%% label(+Vars)
%
% Equivalent to labeling([], Vars).

label(Vs) :- labeling([], Vs).

%% labeling(+Options, +Vars)
%
% Labeling means systematically trying out values for the finite
% domain variables 'Vars' until all of them are ground. The domain of
% each variable in 'Vars' must be finite. 'Options' is a list of
% options that let you exhibit some control over the search process.
% Two sets of options exist: One for variable selection strategy, and
% one for optimisation. The variable selection strategy lets you
% specify which variable should be labeled next and is one of:
%
%   * leftmost
% Label the variables in the order they occur in 'Vars'. This is the
% default.
%
%   * ff
% "First fail". Label the leftmost variable with smallest domain next,
% in order to detect infeasibility early. This is often a good
% strategy.
%
%   * min
% Label the leftmost variable whose lower bound is the lowest next.
%
%   * max
% Label the leftmost variable whose upper bound is the highest next.
%
% The second set of options lets you influence the order of solutions:
%
%   * min(Expr)
%   * max(Expr)
%
% This first generates all solutions such that the arithmetic
% expression 'Expr' assumes the smallest/highest possible value it can
% obtain (with respect to existing constraints).
%
% If more than one option of a category is specified, the one
% occurring rightmost in the option list takes precedence over all
% others of that category. Labeling is always complete and always
% terminates.
%

labeling(Options, Vars) :-
        (   var(Options) -> instantiation_error(Options)
        ;   true
        ),
        is_acyclic_list(Options),
        is_acyclic_list(Vars),
        maplist(finite_domain, Vars),
        label(Options, leftmost, up, none, Vars).

finite_domain(Var) :-
        (   var(Var) ->
            get(Var, Dom, _),
            (   domain_infimum(Dom, n(_)),
                domain_supremum(Dom, n(_)) -> true
            ;   instantiation_error(Var)
            )
        ;   integer(Var) -> true
        ;   must_be(integer, Var)
        ).


label([Option|Options], Selection, Order, Optimisation, Vars) :-
        (   var(Option)-> instantiation_error(Option)
        ;   selection(Option) ->
            label(Options, Option, Order, Optimisation, Vars)
        ;   order(Option) ->
            label(Options, Selection, Option, Optimisation, Vars)
        ;   optimization(Option) ->
            label(Options, Selection, Order, Option, Vars)
        ;   domain_error(labeling_option, Option)
        ).
label([], Selection, Order, Optimisation, Vars) :-
        ( Optimisation == none ->
            label(Vars, Selection, Order)
        ;   optimize(Vars, Selection, Order, Optimisation)
        ).


label([], _, _) :- !.
label(Vars, Selection, Order) :-
        select_var(Selection, Vars, Var, RVars),
        indomain_(Order, Var),
        label(RVars, Selection, Order).

selection(ff).
selection(ffc).
selection(min).
selection(max).
selection(leftmost).

order(up).
order(down).

optimization(min(_)).
optimization(max(_)).

select_var(leftmost, [Var|Vars], Var, Vars).
select_var(min, [V|Vs], Var, RVars) :-
        find_min(Vs, V, Var),
        delete_eq([V|Vs], Var, RVars).
select_var(max, [V|Vs], Var, RVars) :-
        find_max(Vs, V, Var),
        delete_eq([V|Vs], Var, RVars).
select_var(ff, [V|Vs], Var, RVars) :-
        find_ff(Vs, V, Var),
        delete_eq([V|Vs], Var, RVars).
select_var(ffc, Vars0, Var, Vars) :-
        find_ffc(Vars0, Var),
        delete_eq(Vars0, Var, Vars).

find_min([], Var, Var).
find_min([V|Vs], CM, Min) :-
        (   min_lt(V, CM) ->
            find_min(Vs, V, Min)
        ;   find_min(Vs, CM, Min)
        ).

find_max([], Var, Var).
find_max([V|Vs], CM, Max) :-
        (   max_gt(V, CM) ->
            find_max(Vs, V, Max)
        ;   find_max(Vs, CM, Max)
        ).

find_ff([], Var, Var).
find_ff([V|Vs], CM, FF) :-
        (   ff_lt(V, CM) ->
            find_ff(Vs, V, FF)
        ;   find_ff(Vs, CM, FF)
        ).

find_ffc(Vars0, Var) :-
        find_ff(Vars0, _, SD),
        (   var(SD) ->
            find_ffc(Vars0, SD, Var)
        ;   Var = SD
        ).

find_ffc([], Var, Var).
find_ffc([V|Vs], Prev, FF) :-
        (   ffc_lt(V, Prev) ->
            find_ffc(Vs, V, FF)
        ;   find_ffc(Vs, Prev, FF)
        ).

ff_lt(X, Y) :-
        (   var(X) ->
            get(X, DX, _),
            domain_num_elements(DX, NX)
        ;   NX = n(1)
        ),
        (   var(Y) ->
            get(Y, DY, _),
            domain_num_elements(DY, NY)
        ;   NY = n(1)
        ),
        NX cis_lt NY.

ffc_lt(X, Y) :-
        (   var(X) ->
            get(X, XD, XPs),
            domain_num_elements(XD, NXD),
            length(XPs, NXPs)
        ;   NXD = n(0), NXPs = n(0)
        ),
        (   var(Y) ->
            get(Y, YD, YPs),
            domain_num_elements(YD, NYD),
            length(YPs, NYPs)
        ;   NYD = n(0), NYPs = n(0)
        ),
        NXD == NYD,
        NXPs > NYPs.

min_lt(X,Y) :- bounds(X,LX,_), bounds(Y,LY,_), LX cis_lt LY.

max_gt(X,Y) :- bounds(X,_,UX), bounds(Y,_,UY), UX cis_gt UY.

bounds(X, L, U) :-
        (   var(X) ->
            get(X, Dom, _),
            domain_infimum(Dom, L),
            domain_supremum(Dom, U)
        ;   L = n(X), U = L
        ).

delete_eq([],_,[]).
delete_eq([X|Xs],Y,List) :-
        (   X == Y -> List = Xs
        ;   List = [X|Tail],
            delete_eq(Xs,Y,Tail)
        ).

optimize(Vars, Selection, Order, Opt) :-
        copy_term(Vars-Opt, Vars1-Opt1),
        once(label(Vars1, Selection, Order)),
        functor(Opt1, Direction, _),
        maplist(arg(1), [Opt,Opt1], [Expr,Expr1]),
        optimize(Direction, Selection, Order, Vars, Expr1, Expr).

optimize(Direction, Selection, Order, Vars, Expr0, Expr) :-
        Val0 is Expr0,
        copy_term(Vars-Expr, Vars1-Expr1),
        (   Direction == min -> Tighten = (Expr1 #< Val0)
        ;   Tighten = (Expr1 #> Val0) % max
        ),
        (   Tighten, label(Vars1, Selection, Order) ->
            optimize(Direction, Selection, Order, Vars, Expr1, Expr)
        ;
              (   Expr #= Expr0, label(Vars, Selection, Order)
              ;   Expr #\= Expr0, label(Vars, Selection, Order)
              )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% all_different(+Vars)
%
% Constrain 'Vars' to be pairwise distinct.

all_different([]).
all_different([X|Xs]) :- different(Xs, X), all_different(Xs).

different([], _).
different([Y|Ys], X) :- neq(X, Y), different(Ys, X).

%all_different(Ls) :- all_distinct(Ls).

%% sum(+Vars, +Op, +Expr)
%
% Constrain the sum of all integers or variables in 'Vars' to the
% relation 'Op' (for example: #=<) with respect to Expr.

sum(Ls, Op, Value) :- sum(Ls, 0, Op, Value).

sum([], Sum, Op, Value) :- call(Op, Sum, Value).
sum([X|Xs], Acc, Op, Value) :-
        NAcc #= Acc + X,
        sum(Xs, NAcc, Op, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex_chain([]).
lex_chain([Ls|Lss]) :-
        lex_chain_lag(Lss, Ls),
        lex_chain(Lss).

lex_chain_lag([], _).
lex_chain_lag([Ls|Lss], Ls0) :-
        lex_le(Ls0, Ls),
        lex_chain_lag(Lss, Ls).

lex_le([], []).
lex_le([V1|V1s], [V2|V2s]) :-
        V1 #=< V2,
        (   integer(V1) ->
            (   integer(V2) ->
                (   V1 =:= V2 -> lex_le(V1s, V2s) ;  true )
            ;   freeze(V2, lex_le([V1|V1s], [V2|V2s]))
            )
        ;   freeze(V1, lex_le([V1|V1s], [V2|V2s]))
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(nb_setval('$clpfd_tuples_counter', 0)).

tuples_in(Tuples, Relation) :-
        ground(Relation),
        tuples_domain(Tuples, Relation),
        do_queue.

tuples_domain([], _).
tuples_domain([Tuple|Tuples], Relation) :-
        relation_unifiable(Relation, Tuple, Us, 0, _),
        (   ground(Tuple) -> memberchk(Tuple, Relation)
        ;   tuple_domain(Tuple, Us),
            tuple_freeze(Tuple, Us)
        ),
        tuples_domain(Tuples, Relation).

tuple_domain([], _).
tuple_domain([T|Ts], Relation0) :-
        take_firsts(Relation0, Firsts, Relation1),
        ( var(T) ->
            (   Firsts = [Unique] -> T = Unique
            ;   list_to_domain(Firsts, FDom),
                get(T, TDom, TPs),
                domains_intersection(FDom, TDom, TDom1),
                put(T, TDom1, TPs)
            )
        ;   true
        ),
        tuple_domain(Ts, Relation1).

take_firsts([], [], []).
take_firsts([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        take_firsts(Rest, Fs, Oss).

tuple_freeze(Tuple, Relation) :-
        nb_getval('$clpfd_tuples_counter', Count0),
        Count is Count0 + 1,
        nb_setval('$clpfd_tuples_counter', Count),
        atom_concat('$clpfd_rel_', Count0, RID),
        nb_setval(RID, Relation),
        Prop = propagator(rel_tuple(RID,Tuple), mutable(passive)),
        tuple_freeze(Tuple, Tuple, Prop).

tuple_freeze([],  _, _).
tuple_freeze([T|Ts], Tuple, Prop) :-
        ( var(T) ->
            %Prop = propagator(rel_tuple(RID,Tuple), mutable(passive)),
            init_propagator(T, Prop),
            trigger_prop(Prop)
        ;   true
        ),
        tuple_freeze(Ts, Tuple, Prop).

relation_unifiable([], _, [], Changed, Changed).
relation_unifiable([R|Rs], Tuple, Us, Changed0, Changed) :-
        (   all_in_domain(R, Tuple) ->
            Us = [R|Rest],
            relation_unifiable(Rs, Tuple, Rest, Changed0, Changed)
        ;   relation_unifiable(Rs, Tuple, Us, 1, Changed)
        ).

all_in_domain([], []).
all_in_domain([A|As], [T|Ts]) :-
        ( var(T) ->
            get(T, Dom, _),
            domain_contains(Dom, A)
        ;   T =:= A
        ),
        all_in_domain(As, Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Constraint propagation proceeds as follows: Each CLP(FD) variable
   has an attribute that stores its associated domain and contains a
   list of all associated constraints. Whenever the domain of a
   variable is changed, all constraints it participates in are
   triggered: They are stored in a global structure that contains a
   list of triggered constraints. do_queue/0 works off all triggered
   constraints, possible triggering new ones, until fixpoint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% % LIFO queue
% make_queue :- nb_setval('$propagator_queue',[]).

% push_queue(E) :-
%         b_getval('$propagator_queue',Q),
%         b_setval('$propagator_queue',[E|Q]).
% pop_queue(E) :-
%         b_getval('$propagator_queue',[E|Q]),
%         b_setval('$propagator_queue',Q).


% FIFO queue
make_queue :- nb_setval('$clpfd_queue', Q-Q).
push_queue(E) :-
        b_getval('$clpfd_queue', H-[E|T]), b_setval('$clpfd_queue', H-T).
pop_queue(E) :-
        b_getval('$clpfd_queue', H-T),
        nonvar(H), H = [E|NH], b_setval('$clpfd_queue', NH-T).

fetch_constraint_(C) :-
        pop_queue(ps(C0,State)),
        (   State == dead -> fetch_constraint_(C)
        ;   C = C0
        ).

:- make_queue.

parse_clpfd(Expr, Result) :-
        (   var(Expr) ->
            get(Expr, Dom, Es),
            put(Expr, Dom, Es), % constrain Expr to integers
            Result = Expr
        ;   integer(Expr) -> Result = Expr
        ;   Expr = (L + R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            myplus(RL, RR, Result)
        ;   Expr = (L * R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            mytimes(RL, RR, Result)
        ;   Expr = (L - R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            mytimes(-1, RR, RRR),
            myplus(RL, RRR, Result)
        ;   Expr = (- E) ->
            parse_clpfd(E, RE),
            mytimes(-1, RE, Result)
        ;   Expr = max(L, R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            mymax(RL, RR, Result)
        ;   Expr = min(L,R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            mymin(RL, RR, Result)
        ;   Expr = L mod R ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            RR #\= 0,
            mymod(RL, RR, Result)
        ;   Expr = abs(E) ->
            parse_clpfd(E, RE),
            myabs(RE, Result),
            Result #>= 0
        ;   Expr = (L / R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR), RR #\= 0,
            mydiv(RL, RR, Result)
        ;   type_error(integer, Expr)
        ).

trigger_twice(Prop) :-
        % two invocations necessary for fixpoint when posting initially
        trigger_prop(Prop), do_queue,
        trigger_prop(Prop), do_queue.

neq(A, B) :-
        Prop = propagator(pneq(A, B), mutable(passive)),
        init_propagator(A, Prop), init_propagator(B, Prop),
        trigger_twice(Prop).


geq(A, B) :-
        Prop = propagator(pgeq(A,B), mutable(passive)),
        init_propagator(A, Prop), init_propagator(B, Prop),
        trigger_twice(Prop).

leq(A, B) :- geq(B, A).

myplus(X, Y, Z) :-
        Prop = propagator(pplus(X,Y,Z),mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_twice(Prop).

mytimes(X,Y,Z) :-
        Prop = propagator(ptimes(X,Y,Z),mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_twice(Prop).

mydiv(X,Y,Z) :-
        Prop = propagator(pdiv(X,Y,Z), mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_twice(Prop).

myabs(X, Y) :-
        Prop = propagator(pabs(X,Y), mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        trigger_prop(Prop), trigger_twice(Prop).

mymod(X, M, K) :-
        Prop = propagator(pmod(X,M,K), mutable(passive)),
        init_propagator(X, Prop), init_propagator(M, Prop),
        init_propagator(K, Prop), trigger_twice(Prop).

mymax(X, Y, Z) :-
        Prop = propagator(pmax(X,Y,Z), mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_twice(Prop).

mymin(X, Y, Z) :-
        Prop = propagator(pmin(X,Y,Z), mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_twice(Prop).


X #>= Y :- parse_clpfd(X,RX), parse_clpfd(Y,RY), geq(RX,RY).
X #=< Y :- parse_clpfd(X,RX), parse_clpfd(Y,RY), leq(RX,RY).
X #= Y  :- parse_clpfd(X,RX), parse_clpfd(Y,RX).
X #\= Y :- parse_clpfd(X, RX), parse_clpfd(Y, RY), neq(RX, RY).
X #> Y  :- Z #= Y + 1, X #>= Z.
X #< Y  :- Y #> X.

L #<==> R  :- reify(L, B), reify(R, B), do_queue.
L #==> R   :- reify(L, BL), reify(R, BR), myimpl(BL, BR), do_queue.
L #<== R   :- reify(L, BL), reify(R, BR), myimpl(BL, BR), do_queue.
L #/\ R    :- reify(L, 1), reify(R, 1), do_queue.
L #\/ R    :- reify(L, BL), reify(R, BR), myor(BL, BR, 1), do_queue.

myor(X, Y, Z) :-
        Prop = propagator(por(X,Y,Z), mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop),
        trigger_prop(Prop).

myimpl(X, Y) :-
        Prop = propagator(pimpl(X,Y), mutable(passive)),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        trigger_prop(Prop).


reify(Expr, B) :-
        B in 0..1,
        (   var(Expr) ->
            B = Expr
        ;   integer(Expr) -> B = Expr
        ;   Expr = (L #>= R) ->
            parse_clpfd(L, LR), parse_clpfd(R, RR),
            Prop = propagator(reified_geq(LR,RR,B), mutable(passive)),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop),
            trigger_prop(Prop)
        ;   Expr = (L #> R)  -> reify(L #>= (R+1), B)
        ;   Expr = (L #=< R) -> reify(R #>= L, B)
        ;   Expr = (L #< R)  -> reify(R #>= (L+1), B)
        ;   Expr = (L #= R)  ->
            parse_clpfd(L, LR), parse_clpfd(R, RR),
            Prop = propagator(reified_eq(LR,RR,B), mutable(passive)),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop),
            trigger_prop(Prop)
        ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A drep is a user-accessible and visible domain representation. N,
   N..M, and D1 \/ D2 are dreps, if D1 and D2 are dreps.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_drep(V)      :- var(V), !, instantiation_error(V).
is_drep(N)      :- integer(N), !.
is_drep(N..M)   :- !, drep_bound(N), drep_bound(M).
is_drep(D1\/D2) :- !, is_drep(D1), is_drep(D2).

drep_bound(V)   :- var(V), !, instantiation_error(V).
drep_bound(sup) :- !. % should infinities be accessible?
drep_bound(inf) :- !.
drep_bound(I)   :- integer(I), !.

drep_to_intervals(I)        --> { integer(I) }, !, [n(I)-n(I)].
drep_to_intervals(N..M)     -->
        (   { defaulty_to_bound(N, N1), defaulty_to_bound(M, M1),
              N1 cis_leq M1} -> [N1-M1]
        ;   []
        ).
drep_to_intervals(D1 \/ D2) -->
        drep_to_intervals(D1), drep_to_intervals(D2).

drep_to_domain(DR, D) :-
        drep_to_intervals(DR, Is0, []),
        merge_intervals(Is0, Is1),
        intervals_to_domain(Is1, D).

merge_intervals(Is0, Is) :-
        keysort(Is0, Is1),
        merge_overlapping(Is1, Is).

merge_overlapping([], []).
merge_overlapping([A-B0|ABs0], [A-B|ABs]) :-
        merge_remaining(ABs0, B0, B, Rest),
        merge_overlapping(Rest, ABs).

merge_remaining([], B, B, []).
merge_remaining([N-M|NMs], B0, B, Rest) :-
        Next cis B0 + n(1),
        (   N cis_gt Next -> B = B0, Rest = [N-M|NMs]
        ;   B1 cis max(B0,M),
            merge_remaining(NMs, B1, B, Rest)
        ).

cyclic_list(L0) :- cyclic_term(L0), cyclic_list(L0, []).

cyclic_list([_|Tail], Seen) :-
        \+ var(Tail),
        (   memberchk(Tail, Seen) -> true ; cyclic_list(Tail, [Tail|Seen]) ).

is_acyclic_list(Ls) :-
        (   cyclic_list(Ls) ->
            domain_error(acyclic_list, Ls)
        ;   is_list(Ls) -> true
        ;   type_error(list, Ls)
        ).

%% +Var in +Domain
%
%  Constrain 'Var' to elements of 'Domain'. 'Domain' is one of:
%
%         * Lower..Upper
%           All integers I such that Lower =< I =< Upper. The atoms
%           "inf" and "sup" denote negative and positive infinity,
%           respectively.
%         * Domain1 \/ Domain2
%           The union of Domain1 and Domain2.

V in D :-
        fd_variable(V),
        (   is_drep(D) -> true
        ;   must_be(integer, D)
        ),
        drep_to_domain(D, Dom),
        domain(V, Dom).

fd_variable(V) :-
        (   var(V) -> true
        ;   integer(V) -> true
        ;   type_error(integer, V)
        ).

%% ?Vars ins +Domain
%
%  Constrain the variables or integers in 'Vars' to 'Domain'.

Vs ins D :-
        (   var(Vs) -> true
        ;   is_acyclic_list(Vs),
            maplist(fd_variable, Vs)
        ),
        (   is_drep(D) -> true
        ;   must_be(integer, D)
        ),
        drep_to_domain(D, Dom),
        domains(Vs, Dom).

domain(V, Dom) :-
        (   var(V) ->
            get(V, Dom0, VPs),
            domains_intersection(Dom, Dom0, Dom1),
            %format("intersected\n: ~w\n ~w\n==> ~w\n\n", [Dom,Dom0,Dom1]),
            put(V, Dom1, VPs),
            do_queue
        ;   domain_contains(Dom, V)
        ).

domains([], _).
domains([V|Vs], D) :- domain(V, D), domains(Vs, D).


get(X, Dom, Ps) :-
        (   get_attr(X, clpfd, Attr) -> Attr = clpfd(Dom, Ps)
        ;   var(X) -> default_domain(Dom), Ps = []
        ).

get(X, Dom, Inf, Sup, Ps) :-
        get(X, Dom, Ps),
        domain_infimum(Dom, Inf),
        domain_supremum(Dom, Sup).

put(X, Dom, Ps) :-
        Dom \== empty,
        (   Dom = from_to(F, F) -> F = n(X)
        ;   (   get_attr(X, clpfd, Attr) ->
                put_attr(X, clpfd, clpfd(Dom, Ps)),
                Attr = clpfd(OldDom, _OldPs),
                %format("putting dom: ~w\n", [Dom]),
                (   OldDom == Dom -> true
                ;   domain_intervals(Dom, Is),
                    domain_intervals(OldDom, Is) -> true
                ;   trigger_props(Ps)
                )
            ;   var(X) -> %format('\t~w in ~w .. ~w\n',[X,L,U]),
                put_attr(X, clpfd, clpfd(Dom, Ps))
            ;   true
            )
        ).

trigger_props([]).
trigger_props([P|Ps]) :- trigger_prop(P), trigger_props(Ps).

trigger_prop(Propagator) :-
        arg(2, Propagator, MState),
        MState = mutable(State),
        (   State == dead -> true
        ;   State == queued -> true
        ;   % passive
            % format("triggering: ~w\n", [Propagator]),
            setarg(1, MState, queued),
            push_queue(ps(Propagator,MState))
        ).

kill(MState) :- setarg(1, MState, dead).

activate_propagator(propagator(P,MState)) :-
        MState = mutable(State),
        (   State == dead -> true
        ;   %format("running: ~w\n", [P]),
            setarg(1, MState, passive),
            run_propagator(P, MState)
        ).

do_queue :-
        (   fetch_constraint_(C) ->
            activate_propagator(C),
            %C = propagator(Prop,_),
            %functor(Prop, FP, _),
            %format("\n\ngot: ~w\n\n", [FP]),
            do_queue
        ;   true
        ).

init_propagator(Var, Prop) :-
        (   var(Var) -> get(Var, Dom, Ps), put(Var, Dom, [Prop|Ps])
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(pdistinct(Left,Right,X), _MState) :-
        (   ground(X) -> exclude_fire(Left, Right, X)
        ;   %outof_reducer(Left, Right, X),
            %(   var(X) -> kill_if_isolated(Left, Right, X, MState)
            %;   true
            %)
            true
        ).

run_propagator(check_distinct(Left,Right,X), _) :-
        \+ list_contains(Left, X),
        \+ list_contains(Right, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(pneq(A, B), MState) :-
        (   nonvar(A) ->
            (   nonvar(B) -> A =\= B, kill(MState)
            ;   get(B, BD0, BExp0),
                domain_remove(BD0, A, BD1),
                kill(MState),
                put(B, BD1, BExp0)
            )
        ;   nonvar(B) -> run_propagator(pneq(B, A), MState)
        ;   get(A, _, AI, AS, _), get(B, _, BI, BS, _),
            (   AS cis_lt BI -> kill(MState)
            ;   AI cis_gt BS -> kill(MState)
            ;   true
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(pgeq(A,B), MState) :-
        (   A == B -> true
        ;   nonvar(A) ->
            (   nonvar(B) -> kill(MState), A >= B
            ;   get(B, BD, BPs),
                domain_remove_greater_than(BD, A, BD1),
                kill(MState),
                put(B, BD1, BPs)
            )
        ;   nonvar(B) ->
            get(A, AD, APs),
            domain_remove_smaller_than(AD, B, AD1),
            kill(MState),
            put(A, AD1, APs)
        ;   get(A, AD, AL, AU, APs),
            get(B, _, BL, BU, _),
            AU cis_geq BL,
            (   AL cis_gt BU -> kill(MState)
            ;   AU == BL -> A = B
            ;   NAL cis1 max(AL,BL),
                domains_intersection(from_to(NAL,AU), AD, NAD),
                put(A, NAD, APs),
                (   get(B, BD2, BL2, BU2, BPs2) ->
                    NBU cis1 min(BU2, AU),
                    domains_intersection(from_to(BL2,NBU), BD2, NBD),
                    put(B, NBD, BPs2)
                ;   true
                )
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(rel_tuple(RID, Tuple), MState) :-
        b_getval(RID, Relation),
        (   ground(Tuple) -> kill(MState), memberchk(Tuple, Relation)
        ;   relation_unifiable(Relation, Tuple, Us, 0, Changed),
            Us = [_|_],
            (   Us = [Single] -> kill(MState), Single = Tuple
            ;   Changed =:= 0 -> true
            ;   b_setval(RID, Us), tuple_domain(Tuple, Us)
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X + Y = Z
run_propagator(pplus(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is X + Y
            ;   nonvar(Z) -> kill(MState), Y is Z - X
            ;   get(Z, ZD, ZPs),
                get(Y, YD, YPs),
                domain_shift(YD, X, Shifted_YD),
                domains_intersection(Shifted_YD, ZD, ZD1),
                put(Z, ZD1, ZPs),
                (   var(Y) ->
                    O is -X,
                    domain_shift(ZD1, O, YD1),
                    put(Y, YD1, YPs)
                ;   true
                )
            )
        ;   nonvar(Y) -> run_propagator(pplus(Y,X,Z), MState)
        ;   nonvar(Z) ->
            get(X, XD, XPs),
            get(Y, YD, YPs),
            domain_negate(XD, XDN),
            domain_shift(XDN, Z, YD1),
            domains_intersection(YD, YD1, YD2),
            domain_negate(YD2, YD2N),
            domain_shift(YD2N, Z, XD1),
            domains_intersection(XD, XD1, XD2),
            put(X, XD2, XPs),
            put(Y, YD2, YPs)
        ;   get(X, XD, XL, XU, XPs), get(Y, YD, YL, YU, YPs),
            get(Z, ZD, ZL, ZU, ZPs),
            NXL cis max(XL, ZL-YU),
            NXU cis min(XU, ZU-YL),
            (   NXL == XL, NXU == XU -> true
            ;   domains_intersection(XD, from_to(NXL, NXU), NXD),
                put(X, NXD, XPs)
            ),
            (   get(Y, YD2, YL2, YU2, YPs2) ->
                NYL cis max(YL2, ZL-NXU),
                NYU cis min(YU2, ZU-NXL),
                (   NYL == YL2, NYU == YU2 -> true
                ;   domains_intersection(YD2, from_to(NYL, NYU), NYD),
                    put(Y, NYD, YPs2)
                )
            ;   NYL = Y, NYU = Y
            ),
            (   get(Z, ZD2, ZL2, ZU2, ZPs2) ->
                NZL cis max(ZL2,NXL+NYL),
                NZU cis min(ZU2,NXU+NYU),
                (   NZL == ZL2, NZU == ZU2 -> true
                ;   domains_intersection(ZD2, from_to(NZL,NZU), NZD),
                    put(Z, NZD, ZPs2)
                )
            ;   true
            )

%             (   XI \== inf, XS \== sup,
%                 YI \== inf, YS \== sup ->
%                 To1 is To + 1,
%                 Low is min(XI,YI), Up is max(XS,YS),
%                 pplus_remove_impossibles(From, To1, Low, Up, XD, YD, ZD1, ZD2)
%             ;   ZD2 = ZD1
%             ),
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(ptimes(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is X * Y
            ;   X =:= 0 ->
                kill(MState),
                get(Y, YD, YPs),
                put(Y, YD, YPs), % constrain Y to integers
                Z = 0
            ;   nonvar(Z) -> kill(MState), 0 =:= Z mod X, Y is Z // X
            ;   get(Y, YD, _),
                get(Z, ZD, ZPs),
                domain_expand(YD, X, Scaled_YD),
                domains_intersection(ZD, Scaled_YD, ZD1),
                put(Z, ZD1, ZPs),
                (   get(Y, YDom2, YPs2) ->
                    domain_contract(ZD1, X, Contract),
                    domains_intersection(Contract, YDom2, NYDom),
                    put(Y, NYDom, YPs2)
                ;   kill(MState), Z is X * Y
                )
            )
        ;   nonvar(Y) -> mytimes(Y,X,Z)
        ;   nonvar(Z) ->
            (   X == Y ->
                Z >= 0,
                PRoot is floor(sqrt(Z)),
                PRoot**2 =:= Z, NRoot is -PRoot,
                get(X, TXD, TXPs), % temporary variables for this section
                (   PRoot =:= 0 -> TXD1 = from_to(n(0),n(0))
                ;   TXD1 = split(0, from_to(n(NRoot),n(NRoot)),
                                 from_to(n(PRoot),n(PRoot)))
                ),
                domains_intersection(TXD, TXD1, TXD2),
                put(X, TXD2, TXPs)
            ;   true
            ),
            (   get(X, XD, XL, XU, XPs) ->
                get(Y, YD, YL, YU, _),
                min_divide(Z,Z,YL,YU,TNXL),
                max_divide(Z,Z,YL,YU,TNXU),
                NXL cis max(XL,ceiling(TNXL)),
                NXU cis min(XU,floor(TNXU)),
                (   NXL == XL, NXU == XU -> true
                ;   domains_intersection(from_to(NXL,NXU), XD, XD1),
                    put(X, XD1, XPs)
                ),
                (   get(Y, YD2, YL2,YU2,YExp2) ->
                    min_divide(Z,Z,NXL,NXU,NYLT),
                    max_divide(Z,Z,NXL,NXU,NYUT),
                    NYL cis max(YL2,ceiling(NYLT)),
                    NYU cis min(YU2,floor(NYUT)),
                    (   NYL == YL2, NYU == YU2 -> true
                    ;   domains_intersection(from_to(NYL,NYU), YD2, YD3),
                        put(Y, YD3, YExp2)
                    )
                ;   (   Y \== 0 -> 0 =:= Z mod Y, kill(MState), X is Z // Y
                    ;   kill(MState), Z = 0
                    )
                )
            ;   true
            )
        ;   get(X,XD,XL,XU,XExp), get(Y,YD,YL,YU,_), get(Z,ZD,ZL,ZU,_),
            min_divide(ZL,ZU,YL,YU,TXL),
            NXL cis max(XL,ceiling(TXL)),
            max_divide(ZL,ZU,YL,YU,TXU),
            NXU cis min(XU,floor(TXU)),
            domains_intersection(from_to(NXL,NXU), XD, XD1),
            put(X, XD1, XExp),
            (   get(Y,YD2,YL2,YU2,YExp2) ->
                min_divide(ZL,ZU,XL,XU,TYL),
                NYL cis max(YL2,ceiling(TYL)),
                max_divide(ZL,ZU,XL,XU,TYU),
                NYU cis min(YU2,floor(TYU)),
                (   NYL == YL2, NYU == YU2 -> true
                ;   domains_intersection(from_to(NYL,NYU), YD2, YD3),
                    put(Y, YD3, YExp2)
                )
            ;   NYL = Y, NYU = Y
            ),
            (   get(Z, ZD2, ZL2, ZU2, ZExp2) ->
                min_times(NXL,NXU,NYL,NYU,TZL),
                NZL cis1 max(ZL2,TZL),
                max_times(NXL,NXU,NYL,NYU,TZU),
                NZU cis1 min(ZU2,TZU),
                (   NZL == ZL2, NZU == ZU2 -> true
                ;   domains_intersection(from_to(NZL,NZU), ZD2, ZD3),
                    put(Z, ZD3, ZExp2)
                )
            ;   true
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X / Y = Z

run_propagator(pdiv(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is X // Y
            ;   get(Y, YD, YL, YU, YPs),
                (   nonvar(Z) -> true
                    % TODO: cover this
                ;   get(Z, ZD, ZL, ZU, ZPs),
                    (   YL cis_leq n(0), YU cis_geq n(0) ->
                        NZL cis max(-abs(n(X)), ZL),
                        NZU cis min(abs(n(X)), ZU)
                    ;   X >= 0, YL cis_gt n(0) ->
                        NZL cis max(X//YU, ZL),
                        NZU cis min(X//YL, ZU)
                    ;   % TODO: cover more cases
                        NZL = ZL, NZU = ZU
                    ),
                    (   NZL = ZL, NZU = ZU -> true
                    ;   domains_intersection(from_to(NZL,NZU), ZD, NZD),
                        put(Z, NZD, ZPs)
                    )
                )
            )
        ;   nonvar(Y) ->
            get(X, XD, XL, XU, XPs),
            (   nonvar(Z) ->
                (   Z >= 0, Y >= 0 ->
                    NXL cis max(n(Z)*n(Y), XL),
                    NXU cis min((n(Z)+n(1))*n(Y)-n(1), XU)
                ;   % TODO: cover more cases
                    NXL = XL, NXU = XU
                ),
                (   NXL == XL, NXU == XU -> true
                ;   domains_intersection(from_to(NXL,NXU), XD, NXD),
                    put(X, NXD, XPs)
                )
            ;   get(Z, ZD, ZPs),
                domain_contract_less(XD, Y, Contracted),
                domains_intersection(Contracted, ZD, NZD),
                put(Z, NZD, ZPs),
                (   get(X, XD2, XPs2) ->
                    domain_expand_more(NZD, Y, Expanded),
                    domains_intersection(Expanded, XD2, NXD2),
                    put(X, NXD2, XPs2)
                ;   true
                )
            )
        ;   nonvar(Z) ->
            get(X, XD, XL, XU, XPs),
            get(Y, YD, YL, YU, YPs),
            (   YL cis_geq n(0), XL cis_geq n(0) ->
                NXL cis max(YL*Z, XL),
                NXU cis min(YU*(Z+n(1))-n(1), XU)
            ;   %TODO: cover more cases
                NXL = XL, NXU = XU
            ),
            (   NXL == XL, NXU == XU -> true
            ;   domains_intersection(from_to(NXL,NXU), XD, NXD),
                put(X, NXD, XPs)
            )
        ;   true
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Y = abs(X)

run_propagator(pabs(X,Y), MState) :-
        (   nonvar(X) -> kill(MState), Y is abs(X)
        ;   nonvar(Y) ->
            Y >= 0,
            (   Y =:= 0 -> X = 0
            ;   get(X, XD, XPs),
                YN is -Y,
                domains_intersection(split(0, from_to(n(YN),n(YN)),
                                           from_to(n(Y),n(Y))), XD, XD1),
                put(X, XD1, XPs)
            )
        ;   get(X, XD, XPs),
            get(Y, YD, _),
            domain_negate(YD, YDNegative),
            domains_union(YD, YDNegative, XD1),
            domains_intersection(XD, XD1, XD2),
            put(X, XD2, XPs),
            (   get(Y, YD1, YPs1) ->
                domain_negate(XD2, XD2Neg),
                domains_union(XD2, XD2Neg, YD2),
                domain_remove_smaller_than(YD2, 0, YD3),
                domains_intersection(YD1, YD3, YD4),
                put(Y, YD4, YPs1)
            ;   true
            )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% K =_ X   (mod M)

run_propagator(pmod(X,M,K), MState) :-
        (   nonvar(X) ->
            (   nonvar(M) -> kill(MState), K is X mod M
            ;   true
            )
        ;   nonvar(M) ->
            (   M =:= 1 -> X = 0
            ;   get(K, KD, KPs) ->
                MP is abs(M) - 1,
                MN is -MP + 1,
                get(K, KD, KPs),
                domains_intersection(from_to(n(MN), n(MP)), KD, KD1),
                put(K, KD1, KPs)
            ;   get(X, XD, XPs),
                (   domain_supremum(XD, n(_)), domain_infimum(XD, n(_)) ->
                    % bounded domain
                    findall(E, (domain_to_list(XD, XLs),
                                   member(E, XLs), E mod M =:= K), Es),
                    list_to_domain(Es, XD1),
                    domains_intersection(XD, XD1, XD2),
                    put(X, XD2, XPs)
                ;   true
                )
            )
        ;   true % TODO: propagate more
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z = max(X,Y)
% Currently rather preliminary.

run_propagator(pmax(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is max(X,Y)
            ;   nonvar(Z) ->
                (   Z == X -> X #>= Y
                ;   Z > X -> Z = Y
                ;   fail % Z < X
                )
            ;   get(Y, YD, YInf, YSup, _),
                (   YInf cis_gt n(X) -> Z = Y
                ;   YSup cis_lt n(X) -> Z = X
                ;   true % TODO
                )
            )
        ;   nonvar(Y) -> run_propagator(pmax(Y,X,Z), MState)
        ;   nonvar(Z) -> true % TODO
        ;   get(X, _, XInf, _, _),
            get(Y, YD, YInf, YSup, _),
            %get(Z, ZD, _),
            (   YInf cis_gt YSup -> Z = Y
            ;   YSup cis_lt XInf -> Z = X
            ;   true % TODO
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z = min(X,Y)
% Currently rather preliminary.

run_propagator(pmin(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is min(X,Y)
            ;   nonvar(Z) ->
                (   Z == X -> X #=< Y
                ;   Z < X -> Z = Y
                ;   fail % Z > X
                )
            ;   get(Y, YD, YInf, YSup, _),
                (   YSup cis_lt n(X) -> Z = Y
                ;   YInf cis_gt n(X) -> Z = X
                ;   true % TODO
                )
            )
        ;   nonvar(Y) -> run_propagator(pmin(Y,X,Z), MState)
        ;   nonvar(Z) -> true % TODO
        ;   get(X, _, _, XSup, _),
            get(Y, YD, YInf, YSup, _),
            %get(Z, ZD, _),
            (   YSup cis_lt YInf -> Z = Y
            ;   YInf cis_gt XSup -> Z = X
            ;   true % TODO
            )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% reified constraints

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_geq(X,Y,B), MState) :-
        (   var(B) ->
            (   nonvar(X) ->
                (   nonvar(Y) ->
                    kill(MState),
                    (   X >= Y -> B = 1 ; B = 0 )
                ;   get(Y, _, YL, YU, _),
                    (   n(X) cis_geq YU -> B = 1
                    ;   n(X) cis_lt YL -> B = 0
                    ;   true
                    )
                )
            ;   nonvar(Y) ->
                get(X, _, XL, XU, _),
                (   XL cis_geq n(Y) -> B = 1
                ;   XU cis_lt n(Y) -> B = 0
                ;   B in 0..1
                )
            ;   get(X, _, XL, XU, _),
                get(Y, _, YL, YU, _),
                (   XL cis_geq YU -> B = 1
                ;   XU cis_lt YL -> B = 0
                ;   true
                )
            )
        ;   B =:= 1 -> kill(MState), X #>= Y
        ;   B == 0 -> kill(MState), X #< Y
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_eq(X,Y,B), MState) :-
        (   var(B) ->
            (   nonvar(X) ->
                (   nonvar(Y) ->
                    kill(MState),
                    (   X =:= Y -> B = 1 ; B = 0)
                ;   get(Y, _, YL, YU, _),
                    (   YL cis_gt n(X) -> B = 0
                    ;   YU cis_lt n(X) -> B = 0
                    ;   true
                    )
                )
            ;   nonvar(Y) -> run_propagator(reified_eq(Y,X,B), MState)
            ;   X == Y -> B = 1
            ;   get(X, _, XL, XU, _),
                get(Y, _, YL, YU, _),
                (   XL cis_gt YU -> B = 0
                ;   YL cis_gt XU -> B = 0
                ;   true
                )
            )
        ;   B =:= 1 -> X = Y
        ;   B =:= 0 -> X #\= Y
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(pimpl(X, Y), MState) :-
        (   nonvar(X) ->
            (   X =:= 1 -> kill(MState), Y = 1
            ;   true
            )
        ;   nonvar(Y) ->
            (   Y =:= 0 -> kill(MState), X = 0
            ;   true
            )
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(por(X, Y, Z), MState) :-
        (   nonvar(X) ->
            (   X =:= 0 -> Y in 0..1, Y = Z
            ;   X =:= 1 -> Y in 0..1, Z = 1
            )
        ;   nonvar(Y) -> run_propagator(por(Y,X,Z), MState)
        ;   [X,Y,Z] ins 0..1
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_times(L1,U1,L2,U2,Max) :-
	Max cis max(max(L1*L2,L1*U2),max(U1*L2,U1*U2)).
min_times(L1,U1,L2,U2,Min) :-
	Min cis min(min(L1*L2,L1*U2),min(U1*L2,U1*U2)).

max_divide(L1,U1,L2,U2,Max) :-
	(   L2 cis_leq n(0) , U2 cis_geq n(0) -> Max = sup
	;   Max cis max(max(div(L1,L2),div(L1,U2)),max(div(U1,L2),div(U1,U2)))
	).
min_divide(L1,U1,L2,U2,Min) :-
	(   L2 cis_leq n(0) , U2 cis_geq n(0) -> Min = inf
	;   Min cis min(min(div(L1,L2),div(L1,U2)),min(div(U1,L2),div(U1,U2)))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Weak arc consistent all_distinct/1 constraint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%all_distinct(Ls) :- all_different(Ls).
all_distinct(Ls) :-
        MState = mutable(passive),
        all_distinct(Ls, [], MState),
        do_queue.

all_distinct([], _, _).
all_distinct([X|Right], Left, MState) :-
        \+ list_contains(Right, X),
        (   var(X) ->
            Prop = propagator(pdistinct(Left,Right,X), mutable(passive)),
            init_propagator(X, Prop),
            trigger_prop(Prop)
%             Prop2 = propagator(check_distinct(Left,Right,X), mutable(passive)),
%             init_propagator(X, Prop2),
%             trigger_prop(Prop2)
        ;   exclude_fire(Left, Right, X)
        ),
        outof_reducer(Left, Right, X),
        all_distinct(Right, [X|Left], MState).

exclude_fire(Left, Right, E) :-
        exclude_list(Left, E),
        exclude_list(Right, E).

exclude_list([], _).
exclude_list([V|Vs], Val) :-
        (   var(V) ->
            get(V, VD, VPs),
            domain_remove(VD, Val, VD1),
            put(V, VD1, VPs)
        ;   V =\= Val
        ),
        exclude_list(Vs, Val).

list_contains([X|Xs], Y) :-
        (   X == Y -> true
        ;   list_contains(Xs, Y)
        ).

kill_if_isolated(Left, Right, X, MState) :-
        append(Left, Right, Others),
        get(X, XDom, _),
        (   all_empty_intersection(Others, XDom) -> kill(MState)
        ;   true
        ).

all_empty_intersection([], _).
all_empty_intersection([V|Vs], XDom) :-
        (   var(V) ->
            get(V, VDom, _),
            domains_intersection_(VDom, XDom, empty),
            all_empty_intersection(Vs, XDom)
        ;   all_empty_intersection(Vs, XDom)
        ).

outof_reducer(Left, Right, Var) :-
        (   var(Var) ->
            append(Left, Right, Others),
            get(Var, Dom, _),
            domain_num_elements(Dom, N),
            num_subsets(Others, Dom, 0, Num, NonSubs),
            (   n(Num) cis_geq N -> fail
            ;   n(Num) cis1 N - n(1) ->
                reduce_from_others(NonSubs, Dom)
            ;   true
            )
        ;   %\+ list_contains(Right, Var),
            %\+ list_contains(Left, Var)
            true
        ).

reduce_from_others([], _).
reduce_from_others([X|Xs], Dom) :-
        (   var(X) ->
            get(X, XDom, XPs),
            domain_subtract(XDom, Dom, NXDom),
            put(X, NXDom, XPs)
        ;   true
        ),
        reduce_from_others(Xs, Dom).

num_subsets([], _Dom, Num, Num, []).
num_subsets([S|Ss], Dom, Num0, Num, NonSubs) :-
        (   var(S) ->
            get(S, SDom, _),
            (   domain_subdomain(Dom, SDom) ->
                Num1 is Num0 + 1,
                num_subsets(Ss, Dom, Num1, Num, NonSubs)
            ;   NonSubs = [S|Rest],
                num_subsets(Ss, Dom, Num0, Num, Rest)
            )
        ;   num_subsets(Ss, Dom, Num0, Num, NonSubs)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Hooks
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attr_unify_hook(clpfd(Dom,Ps), Other) :-
        (   nonvar(Other) ->
            (   integer(Other) -> true
            ;   type_error(integer, Other)
            ),
            domain_contains(Dom, Other),
            trigger_props(Ps),
            do_queue
        ;   get(Other, OD, OPs),
            domains_intersection(Dom, OD, Dom1),
            append(Ps, OPs, Ps1),
            put(Other, Dom1, Ps1),
            do_queue
        ).


bound_portray(inf, inf).
bound_portray(sup, sup).
bound_portray(n(N), N).

attr_portray_hook(clpfd(Dom,_Ps), _) :-
        domain_intervals(Dom, Is),
        print_intervals(Is),
        %write(Ps),
        true.

print_intervals([]).
print_intervals([A0-B0|Is]) :-
        bound_portray(A0, A),
        bound_portray(B0, B),
        (   A == B -> write(A)
        ;   format("~w..~w", [A,B])
        ;   true
        ),
        (   Is == [] -> true
        ;   format(" \\/ "),
            print_intervals(Is)
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
domain_to_list(Domain, List) :- domain_to_list(Domain, List, []).

domain_to_list(split(_, Left, Right)) -->
        domain_to_list(Left), domain_to_list(Right).
domain_to_list(empty)                 --> [].
domain_to_list(from_to(n(F),n(T)))    --> { numlist(F, T, Ns) }, dlist(Ns).

dlist([])     --> [].
dlist([L|Ls]) --> [L], dlist(Ls).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Testing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- test_intersection([1,2,3,4,5], [1,5], I).

%?- test_intersection([1,2,3,4,5], [], I).

test_intersection(List1, List2, Is) :-
        list_to_domain(List1, D1),
        list_to_domain(List2, D2),
        domains_intersection(D1, D2, I),
        domain_to_list(I, Is).

test_subdomain(L1, L2) :-
        list_to_domain(L1, D1),
        list_to_domain(L2, D2),
        domain_subdomain(D1, D2).
