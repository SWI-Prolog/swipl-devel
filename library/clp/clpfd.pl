/*  $Id$

    Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, 2008 Markus Triska

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

   ?- Z #= X + Y.
   %@ Y in inf..sup,
   %@ X+Y#=Z,
   %@ X in inf..sup,
   %@ Z in inf..sup.

   No artificial limits (using GMP)
   ---------------------------------

   ?- N is 2**66, X #\= N.
   %@ N = 73786976294838206464,
   %@ X in inf..73786976294838206463\/73786976294838206465..sup.

   Often stronger propagation
   ---------------------------------

   ?- Y #= abs(X), Y #\= 3, Z * Z #= 4.
   %@ X in inf.. -4\/ -2..2\/4..sup,
   %@ Y#=abs(X),
   %@ Y in 0..2\/4..sup,
   %@ Z in -2\/2.

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
                  (#\)/1,
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
                  lex_chain/1,
                  serialized/2,
                  fd_var/1,
                  fd_inf/2,
                  fd_sup/2,
                  fd_size/2,
                  fd_dom/2
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
used to model and solve various combinatorial problems such as
planning, scheduling and allocation tasks.

Most predicates of this library are finite domain _constraints_, which
are relations over integers. They generalise arithmetic evaluation of
integer expressions in that propagation can proceed in all directions.
This library also provides _enumeration_ _predicates_, which let you
systematically search for solutions on variables whose domains have
become finite. A finite domain _expression_ is one of:

    | an integer         | Given value                   |
    | a variable         | Unknown value                 |
    | -Expr              | Unary minus                   |
    | Expr + Expr        | Addition                      |
    | Expr * Expr        | Multiplication                |
    | Expr - Expr        | Subtraction                   |
    | min(Expr,Expr)     | Minimum of two expressions    |
    | max(Expr,Expr)     | Maximum of two expressions    |
    | Expr mod Expr      | Remainder of integer division |
    | abs(Expr)          | Absolute value                |
    | Expr / Expr        | Integer division              |

The most important finite domain constraints are:

    | Expr1 #>= Expr2  | Expr1 is larger than or equal to Expr2  |
    | Expr1 #=< Expr2  | Expr1 is smaller than or equal to Expr2 |
    | Expr1 #=  Expr2  | Expr1 equals Expr2 |
    | Expr1 #\= Expr2  | Expr1 is not equal to Expr2 |
    | Expr1 #> Expr2   | Expr1 is strictly larger than Expr2 |
    | Expr1 #< Expr2   | Expr1 is strictly smaller than Expr2 |

The constraints #=/2, #\=/2, #</2, #>/2, #=</2, and #>=/2 can be
_reified_, which means reflecting their truth values into Boolean
values represented by the integers 0 and 1. Let P and Q denote
reifiable constraints or Boolean variables, then:

    | #\ Q      | True iff Q is false             |
    | P #\/ Q   | True iff either P or Q          |
    | P #/\ Q   | True iff both P and Q           |
    | P #<==> Q | True iff P and Q are equivalent |
    | P #==> Q  | True iff P implies Q            |
    | P #<== Q  | True iff Q implies P            |

If a variable occurs at the place of a constraint that is being
reified, it is implicitly constrained to the Boolean values 0 and 1.
Therefore, the following queries all fail: ?- #\ 2. ?- #\ #\ 2. etc.

As an example of a constraint satisfaction problem, consider the
cryptoarithmetic puzzle SEND + MORE = MONEY, where different letters
denote distinct integers between 0 and 9. It can be modeled in CLP(FD)
as follows:

==
:- use_module(library(clpfd)).

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
        Vars = [S,E,N,D,M,O,R,Y],
        Vars ins 0..9,
        all_different(Vars),
                  S*1000 + E*100 + N*10 + D +
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
        M #> 0, S #> 0.
==

Sample query and its result:

==
?- puzzle(As+Bs=Cs).
As = [9, _G10178, _G10181, _G10184],
Bs = [1, 0, _G10199, _G10178],
Cs = [1, 0, _G10181, _G10178, _G10223],
_G10223 in 2..8,
1000*9+91*_G10178+ -90*_G10181+_G10184+ -9000*1+ -900*0+10*_G10199+ -1*_G10223#=0,
all_different([_G10178, _G10181, _G10184, _G10199, _G10223, 0, 1, 9]),
_G10199 in 2..8,
_G10184 in 2..8,
_G10181 in 5..8,
_G10178 in 4..7.
==

Here, the constraint solver could deduce more stringent bounds for
many variables. Labeling can be used to search for solutions:

==
?- puzzle(As+Bs=Cs), label(As), label(Bs).
As = [9, 5, 6, 7],
Bs = [1, 0, 8, 5],
Cs = [1, 0, 6, 5, 2]
==

This library also provides _reflection_ predicates (like fd_dom/2,
fd_size/2 etc.) with which you can inspect a variable's current
domain. Use call_residue_vars/2 and copy_term/3 to inspect residual
goals and the constraints in which a variable is involved.

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
cis_gt(sup, B0) :- B0 \== sup.

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

cis_times_(inf, A, P)     :- cis_times(inf, n(A), P).
cis_times_(sup, A, P)     :- cis_times(sup, n(A), P).
cis_times_(n(B), A, n(P)) :- P is A * B.

% compactified is/2 for expressions of interest
A cis B :- cis_(B, A).

cis_(n(N), n(N)).
cis_(inf, inf).
cis_(sup, sup).
cis_(sign(A0), S)    :- cis_(A0, A), cis_sign(A, S).
cis_(A0+B0, E)       :- cis_(A0, A), cis_(B0, B), cis_plus(A, B, E).
cis_(abs(A0), E)     :- cis_(A0, A), cis_abs(A, E).
cis_(min(A0,B0), E)  :- cis_(A0, A), cis_(B0, B), cis_min(A, B, E).
cis_(max(A0,B0), E)  :- cis_(A0, A), cis_(B0, B), cis_max(A, B, E).
cis_(A0-B0, E)       :- cis_(A0, A), cis_(B0, B), cis_minus(A, B, E).
cis_(-A0, E)         :- cis_(A0, A), cis_uminus(A, E).
cis_(A0*B0, E)       :- cis_(A0, A), cis_(B0, B), cis_times(A, B, E).
cis_(div(A0,B0), E)  :- cis_(A0, A), cis_(B0, B), cis_div(A, B, E).
cis_(A0//B0, E)      :- cis_(A0, A), cis_(B0, B), cis_slash(A, B, E).

% special case for the frequent case of depth 1 expressions

A cis1 B :- cis1_(B, A).

cis1_(n(N), n(N)).
cis1_(inf, inf).
cis1_(sup, sup).
cis1_(sign(A), S)    :- cis_sign(A, S).
cis1_(A+B, E)        :- cis_plus(A, B, E).
cis1_(abs(A), E)     :- cis_abs(A, E).
cis1_(min(A,B), E)   :- cis_min(A, B, E).
cis1_(max(A,B), E)   :- cis_max(A, B, E).
cis1_(A-B, E)        :- cis_minus(A, B, E).
cis1_(-A, E)         :- cis_uminus(A, E).
cis1_(A*B, E)        :- cis_times(A, B, E).
cis1_(div(A,B), E)   :- cis_div(A, B, E).
cis1_(A//B, E)       :- cis_slash(A, B, E).

cis_sign(sup, n(1)).
cis_sign(inf, n(0)).
cis_sign(n(N), n(S)) :-
        (   N < 0 -> S = -1
        ;   N > 0 -> S = 1
        ;   S = 0
        ).

cis_div(sup, Y, Z)  :- ( cis_geq_zero(Y) -> Z = sup ; Z = inf ).
cis_div(inf, Y, Z)  :- ( cis_geq_zero(Y) -> Z = inf ; Z = sup ).
cis_div(n(X), Y, Z) :- cis_div_(Y, X, Z).

cis_div_(sup, _, n(0)).
cis_div_(inf, _, n(0)).
cis_div_(n(Y), X, Z) :-
        (   Y =:= 0 -> (  X >= 0 -> Z = sup ; Z = inf )
        ;   Z0 is X // Y, Z = n(Z0)
        ).

cis_slash(sup, _, sup).
cis_slash(inf, _, inf).
cis_slash(n(N), B, S) :- cis_slash_(B, N, S).

cis_slash_(sup, _, n(0)).
cis_slash_(inf, _, n(0)).
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

check_domain(D) :-
        (   var(D) -> instantiation_error(D)
        ;   is_domain(D) -> true
        ;   domain_error(clpfd_domain, D)
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

domain_infimum(from_to(I, _), I).
domain_infimum(split(_, Left, _), I) :- domain_infimum(Left, I).

domain_supremum(from_to(_, S), S).
domain_supremum(split(_, _, Right), S) :- domain_supremum(Right, S).

domain_num_elements(empty, n(0)).
domain_num_elements(from_to(From,To), Num) :- Num cis To - From + n(1).
domain_num_elements(split(_, Left, Right), Num) :-
        domain_num_elements(Left, NL),
        domain_num_elements(Right, NR),
        Num cis1 NL + NR.

domain_direction_element(from_to(n(From), n(To)), Dir, E) :-
        (   Dir == up -> between(From, To, E)
        ;   between(From, To, E0),
            E is To - (E0 - From)
        ).
domain_direction_element(split(_, D1, D2), Dir, E) :-
        (   Dir == up ->
            (   domain_direction_element(D1, Dir, E)
            ;   domain_direction_element(D2, Dir, E)
            )
        ;   (   domain_direction_element(D2, Dir, E)
            ;   domain_direction_element(D1, Dir, E)
            )
        ).

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
        (   U0 == n(X) -> U1 is X - 1, D = from_to(inf, n(U1))
        ;   U0 cis_lt n(X) -> D = from_to(inf,U0)
        ;   L1 is X + 1, U1 is X - 1,
            D = split(X, from_to(inf, n(U1)), from_to(n(L1),U0))
        ).
domain_remove_(n(N), U0, X, D) :- domain_remove_upper(U0, n(N), X, D).

domain_remove_upper(sup, L0, X, D) :-
        (   L0 == n(X) -> L1 is X + 1, D = from_to(n(L1),sup)
        ;   L0 cis_gt n(X) -> D = from_to(L0,sup)
        ;   L1 is X + 1, U1 is X - 1,
            D = split(X, from_to(L0,n(U1)), from_to(n(L1),sup))
        ).
domain_remove_upper(n(U00), L0, X, D) :-
        U0 = n(U00),
        (   L0 == U0, n(X) == L0 -> D = empty
        ;   L0 == n(X) -> L1 is X + 1, D = from_to(n(L1), U0)
        ;   U0 == n(X) -> U1 cis1 U0 - n(1), D = from_to(L0, U1)
        ;   L0 cis_leq n(X), n(X) cis_leq U0 ->
            U1 is X - 1, L1 is X + 1,
            D = split(X, from_to(L0, n(U1)), from_to(n(L1), U0))
        ;   D = from_to(L0,U0)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Remove all elements greater than / less than a constant.
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

domain_intervals(D, Is) :- phrase(domain_intervals(D), Is).

domain_intervals(split(_, Left, Right)) -->
        domain_intervals(Left), domain_intervals(Right).
domain_intervals(empty)                 --> [].
domain_intervals(from_to(From,To))      --> [From-To].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   To compute the intersection of two domains D1 and D2, we choose D1
   as the reference domain. For each interval of D1, we compute how
   far and to which values D2 lets us extend it.
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
        (   Left1 == empty -> Dom = Right1
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
   Shift the domain by an offset.
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
        (   M < 0 ->
            domain_negate(D0, D1),
            M1 is abs(M),
            domain_expand_(D1, M1, D)
        ;   M =:= 1 -> D = D0
        ;   domain_expand_(D0, M, D)
        ).

domain_expand_(empty, _, empty).
domain_expand_(from_to(From0, To0), M, from_to(From,To)) :-
        From cis1 From0*n(M),
        To cis1 To0*n(M).
domain_expand_(split(S0, Left0, Right0), M, split(S, Left, Right)) :-
        S is M*S0,
        domain_expand_(Left0, M, Left),
        domain_expand_(Right0, M, Right).

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
        ;   From cis1 From0*n(M)
        ),
        (   To0 cis_lt n(0) ->
            To cis1 To0*n(M)
        ;   To cis (To0+n(1))*n(M) - n(1)
        ).
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
        (   cis_geq_zero(From0) ->
            From cis (From0 + n(M) - n(1)) // n(M)
        ;   From cis1 From0 // n(M)
        ),
        (   cis_geq_zero(To0) ->
            To cis1 To0 // n(M)
        ;   To cis (To0 - n(M) + n(1)) // n(M)
        ).
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
            From cis1 max(LeftInf, From0),
            To cis1 min(RightSup, To0),
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
        (   fail, domain_infimum(D1, n(_)), domain_supremum(D1, n(_)) ->
            % bounded domain - currently disabled
            domain_intervals(D1, Is),
            phrase(intervals_contract_less(Is, M1), Cs),
            list_to_domain(Cs, D)
        ;   domain_contract_less_(D1, M1, D)
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
            min_divide_less(Inf, Sup, n(M), n(M), From0),
            max_divide_less(Inf, Sup, n(M), n(M), To0),
            domain_infimum(Left, LeftInf),
            domain_supremum(Right, RightSup),
            From cis1 max(LeftInf, From0),
            To cis1 min(RightSup, To0),
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
        FL is L // 2,
        length(Front, FL),
        append(Front, Tail, Is),
        Tail = [n(Start)-_|_],
        Hole is Start - 1,
        intervals_to_domain(Front, Left),
        intervals_to_domain(Tail, Right),
        D = split(Hole, Left, Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ?Var in +Domain
%
%  Constrain Var to elements of Domain. Domain is one of:
%
%         * Lower..Upper
%           All integers _I_ such that _Lower_ =< _I_ =< _Upper_. The atoms
%           *inf* and *sup* denote negative and positive infinity,
%           respectively.
%         * Domain1 \/ Domain2
%           The union of Domain1 and Domain2.

V in D :-
        fd_variable(V),
        (   is_drep(D) -> true
        ;   domain_error(clpfd_domain, D)
        ),
        drep_to_domain(D, Dom),
        domain(V, Dom).

fd_variable(V) :-
        (   var(V) -> true
        ;   integer(V) -> true
        ;   type_error(integer, V)
        ).

%% +Vars ins +Domain
%
%  Constrain the variables in the list Vars to elements of Domain.

Vs ins D :-
        must_be(list, Vs),
        maplist(fd_variable, Vs),
        (   is_drep(D) -> true
        ;   domain_error(clpfd_domain, D)
        ),
        drep_to_domain(D, Dom),
        domains(Vs, Dom).

%% indomain(?Var)
%
% Bind Var to all feasible values of its domain on backtracking. The
% domain of Var must be finite.

indomain(Var) :- label([Var]).

order_dom_next(up, Dom, Next)   :- domain_infimum(Dom, n(Next)).
order_dom_next(down, Dom, Next) :- domain_supremum(Dom, n(Next)).


%% label(+Vars)
%
% Equivalent to labeling([], Vars).

label(Vs) :- labeling([], Vs).

%% labeling(+Options, +Vars)
%
% Labeling means systematically trying out values for the finite
% domain variables Vars until all of them are ground. The domain of
% each variable in Vars must be finite. Options is a list of options
% that let you exhibit some control over the search process. Several
% categories of options exist:
%
% The variable selection strategy lets you specify which variable of
% Vars should be labeled next and is one of:
%
%   * leftmost
%   Label the variables in the order they occur in Vars. This is the
%   default.
%
%   * ff
%   _|First fail|_. Label the leftmost variable with smallest domain next,
%   in order to detect infeasibility early. This is often a good
%   strategy.
%
%   * ffc
%   Of the variables with smallest domains, the leftmost one
%   participating in most constraints is labeled next.
%
%   * min
%   Label the leftmost variable whose lower bound is the lowest next.
%
%   * max
%   Label the leftmost variable whose upper bound is the highest next.
%
% The value order is one of:
%
%   * up
%   Try the elements of the chosen variable's domain in ascending order.
%   This is the default.
%
%   * down
%   Try the domain elements in descending order.
%
% The branching strategy is one of:
%
%   * step
%   For each variable X, a choice is made between X = V and X #\= V,
%   where V is determined by the value ordering options. This is the
%   default.
%
%   * enum
%   For each variable X, a choice is made between X = V_1, X = V_2
%   etc., for all values V_i of the domain of X. The order is
%   determined by the value ordering options.
%
%   * bisect
%   For each variable X, a choice is made between X #=< M and X #> M,
%   where M is the midpoint of the domain of X.
%
% The order of solutions can be influenced with:
%
%   * min(Expr)
%   * max(Expr)
%
% This generates solutions in ascending/descending order with respect
% to the evaluation of the arithmetic expression Expr. Labeling Vars
% must make Expr ground. To obtain the incomplete behaviour that other
% systems exhibit with "maximize(Expr)" and "minimize(Expr)", use
% once/1, e.g.:
%
% ==
% once(labeling([max(Expr)], Vars))
% ==
%
% If more than one option of a category is specified, the one
% occurring rightmost in the option list takes precedence over all
% others of that category. Labeling is always complete, always
% terminates, and yields no redundant solutions.
%

labeling(Options, Vars) :-
        must_be(list, Options),
        must_be(list, Vars),
        maplist(finite_domain, Vars),
        label(Options, leftmost, up, step, none, upto_ground, Vars).

finite_domain(Var) :-
        (   fd_get(Var, Dom, _) ->
            (   domain_infimum(Dom, n(_)), domain_supremum(Dom, n(_)) -> true
            ;   instantiation_error(Var)
            )
        ;   integer(Var) -> true
        ;   must_be(integer, Var)
        ).


label([O|Os], Selection, Order, Choice, Optimisation, Consistency, Vars) :-
        (   var(O)-> instantiation_error(O)
        ;   selection(O) ->
            label(Os, O, Order, Choice, Optimisation, Consistency, Vars)
        ;   order(O) ->
            label(Os, Selection, O, Choice, Optimisation, Consistency, Vars)
        ;   choice(O) ->
            label(Os, Selection, Order, O, Optimisation, Consistency, Vars)
        ;   optimisation(O) ->
            label(Os, Selection, Order, Choice, O, Consistency, Vars)
        ;   consistency(O, O1) ->
            label(Os, Selection, Order, Choice, Optimisation, O1, Vars)
        ;   domain_error(labeling_option, O)
        ).
label([], Selection, Order, Choice, Optimisation, Consistency, Vars) :-
        ( Optimisation == none ->
            label(Vars, Selection, Order, Choice, Consistency)
        ;   optimise(Vars, [Selection,Order,Choice], Optimisation)
        ).

all_dead([]).
all_dead([propagator(_, mutable(dead, _))|Ps]) :- all_dead(Ps).

label([], _, _, _, Consistency) :- !,
        (   Consistency = upto_in(I0,I) -> I0 = I
        ;   true
        ).
label(Vars, Selection, Order, Choice, Consistency) :-
        select_var(Selection, Vars, Var, RVars),
        (   var(Var) ->
            (   Consistency = upto_in(I0,I),
                fd_get(Var, _, Ps),
                all_dead(Ps) ->
                fd_size(Var, Size),
                I1 is I0*Size,
                label(RVars, Selection, Order, Choice, upto_in(I1,I))
            ;   choice_order_variable(Choice, Order, Var, RVars, Selection, Consistency)
            )
        ;   must_be(integer, Var),
            label(RVars, Selection, Order, Choice, Consistency)
        ).

choice_order_variable(step, Order, Var, Vars, Selection, Consistency) :-
        fd_get(Var, Dom, _),
        order_dom_next(Order, Dom, Next),
        (   Var = Next,
            label(Vars, Selection, Order, step, Consistency)
        ;   neq_num(Var, Next),
            label([Var|Vars], Selection, Order, step, Consistency)
        ).
choice_order_variable(enum, Order, Var, Vars, Selection, Consistency) :-
        fd_get(Var, Dom0, _),
        domain_direction_element(Dom0, Order, Var),
        label(Vars, Selection, Order, enum, Consistency).
choice_order_variable(bisect, Order, Var, Vars, Selection, Consistency) :-
        fd_get(Var, Dom, _),
        domain_infimum(Dom, n(I)),
        domain_supremum(Dom, n(S)),
        Mid0 is (I + S) // 2,
        (   Mid0 =:= S -> Mid is Mid0 - 1 ; Mid = Mid0 ),
        (   Var #=< Mid,
            label([Var|Vars], Selection, Order, bisect, Consistency)
        ;   Var #> Mid,
            label([Var|Vars], Selection, Order, bisect, Consistency)
        ).

selection(ff).
selection(ffc).
selection(min).
selection(max).
selection(leftmost).

choice(step).
choice(enum).
choice(bisect).

order(up).
order(down).

consistency(upto_in(I), upto_in(1, I)).
consistency(upto_ground, upto_ground).

optimisation(min(_)).
optimisation(max(_)).

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
        (   fd_get(X, DX, _) ->
            domain_num_elements(DX, NX)
        ;   NX = n(1)
        ),
        (   fd_get(Y, DY, _) ->
            domain_num_elements(DY, NY)
        ;   NY = n(1)
        ),
        NX cis_lt NY.

ffc_lt(X, Y) :-
        (   fd_get(X, XD, XPs) ->
            domain_num_elements(XD, NXD),
            length(XPs, NXPs)
        ;   NXD = n(0), NXPs = n(0)
        ),
        (   fd_get(Y, YD, YPs) ->
            domain_num_elements(YD, NYD),
            length(YPs, NYPs)
        ;   NYD = n(0), NYPs = n(0)
        ),
        NXD == NYD,
        NXPs > NYPs.

min_lt(X,Y) :- bounds(X,LX,_), bounds(Y,LY,_), LX cis_lt LY.

max_gt(X,Y) :- bounds(X,_,UX), bounds(Y,_,UY), UX cis_gt UY.

bounds(X, L, U) :-
        (   fd_get(X, Dom, _) ->
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Optimisation uses destructive assignment to save the computed
   extremum over backtracking. Failure is used to get rid of copies of
   attributed variables that are created in intermediate steps. At
   least that's the intention - it currently doesn't work in SWI:

   %?- X in 0..3, call_residue_vars(labeling([min(X)], [X]), Vs).
   %@ X = 0,
   %@ Vs = [_G6174, _G6177],
   %@ _G6174 in 0..3

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

optimise(Vars, Options, What) :-
        Extremum = extremum(none),
        (   store_extremum(Vars, Options, What, Extremum)
        ;   Extremum = extremum(n(Val)),
            arg(1, What, Expr),
            (   Expr #= Val,
                labeling(Options, Vars)
            ;   Expr #\= Val,
                optimise(Vars, Options, What)
            )
        ).

store_extremum(Vars, Options, What, Extremum) :-
        duplicate_term(Vars-What, Vars1-What1),
        once(labeling(Options, Vars1)),
        functor(What, Direction, _),
        maplist(arg(1), [What,What1], [Expr,Expr1]),
        optimise(Direction, Options, Vars, Expr1, Expr, Extremum).

optimise(Direction, Options, Vars, Expr0, Expr, Extremum) :-
        Val0 is Expr0,
        nb_setarg(1, Extremum, n(Val0)),
        duplicate_term(Vars-Expr, Vars1-Expr1),
        tighten(Direction, Expr1, Val0),
        once(labeling(Options, Vars1)),
        optimise(Direction, Options, Vars, Expr1, Expr, Extremum).

tighten(min, E, V) :- E #< V.
tighten(max, E, V) :- E #> V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% all_different(+Vars)
%
% Constrain Vars to be pairwise distinct.

all_different(Ls) :-
        must_be(list, Ls),
        State = mutable(shared, _),
        all_different(Ls, [], State),
        do_queue.

all_different([], _, _).
all_different([X|Right], Left, State) :-
        (   var(X) ->
            make_propagator(pdifferent(Left,Right,X,State), Prop),
            init_propagator(X, Prop),
            trigger_prop(Prop)
        ;   exclude_fire(Left, Right, X)
        ),
        all_different(Right, [X|Left], State).

%% sum(+Vars, +Op, +Expr)
%
% Constrain the sum of a list.  The sum/3 constraint demands that
% "sumlist(Vars) Op Expr" hold, e.g.:
%
% ==
% sum(List, #=<, 100)
% ==

scalar_supported(#=).
scalar_supported(#\=).

sum(Ls, Op, Value) :-
        must_be(list, Ls),
        maplist(fd_variable, Ls),
        must_be(callable, Op),
        (   scalar_supported(Op),
            vars_plusterm(Ls, 0, Left),
            left_right_linsum_const(Left, Value, Cs, Vs, Const) ->
            scalar_product(Cs, Vs, Op, Const)
        ;   sum(Ls, 0, Op, Value)
        ).

vars_plusterm([], T, T).
vars_plusterm([V|Vs], T0, T) :- vars_plusterm(Vs, T0+V, T).

scalar_product(Cs, Vs, Op, C) :-
        make_propagator(scalar_product(Cs,Vs,Op,C), Prop),
        vs_propagator(Vs, Prop),
        trigger_prop(Prop),
        do_queue.

vs_propagator([], _).
vs_propagator([V|Vs], Prop) :-
        init_propagator(V, Prop),
        vs_propagator(Vs, Prop).

sum([], Sum, Op, Value) :- call(Op, Sum, Value).
sum([X|Xs], Acc, Op, Value) :-
        NAcc #= Acc + X,
        sum(Xs, NAcc, Op, Value).

coeffs_variables_const([], [], [], [], I, I).
coeffs_variables_const([C|Cs], [V|Vs], Cs1, Vs1, I0, I) :-
        (   var(V) ->
            Cs1 = [C|CRest], Vs1 = [V|VRest], I1 = I0
        ;   I1 is I0 + C*V,
            Cs1 = CRest, Vs1 = VRest
        ),
        coeffs_variables_const(Cs, Vs, CRest, VRest, I1, I).

sum_finite_domains([], [], [], [], Inf, Sup, Inf, Sup).
sum_finite_domains([C|Cs], [V|Vs], Infs, Sups, Inf0, Sup0, Inf, Sup) :-
        fd_get(V, _, Inf1, Sup1, _),
        (   Inf1 = n(NInf) ->
            (   C < 0 ->
                Sup2 is Sup0 + C*NInf
            ;   Inf2 is Inf0 + C*NInf
            ),
            Sups = Sups1,
            Infs = Infs1
        ;   (   C < 0 ->
                Sup2 = Sup0,
                Sups = [C*V|Sups1],
                Infs = Infs1
            ;   Inf2 = Inf0,
                Infs = [C*V|Infs1],
                Sups = Sups1
            )
        ),
        (   Sup1 = n(NSup) ->
            (   C < 0 ->
                Inf2 is Inf0 + C*NSup
            ;   Sup2 is Sup0 + C*NSup
            ),
            Sups1 = Sups2,
            Infs1 = Infs2
        ;   (   C < 0 ->
                Inf2 = Inf0,
                Infs1 = [C*V|Infs2],
                Sups1 = Sups2
            ;   Sup2 = Sup0,
                Sups1 = [C*V|Sups2],
                Infs1 = Infs2
            )
        ),
        sum_finite_domains(Cs, Vs, Infs2, Sups2, Inf2, Sup2, Inf, Sup).

remove_dist_upper_lower([], _, _, _).
remove_dist_upper_lower([C|Cs], [V|Vs], D1, D2) :-
        (   fd_get(V, VD, VPs) ->
            (   C < 0 ->
                domain_supremum(VD, n(Sup)),
                L is Sup + D1//C,
                domain_remove_smaller_than(VD, L, VD1),
                domain_infimum(VD1, n(Inf)),
                G is Inf - D2//C,
                domain_remove_greater_than(VD1, G, VD2)
            ;   domain_infimum(VD, n(Inf)),
                G is Inf + D1//C,
                domain_remove_greater_than(VD, G, VD1),
                domain_supremum(VD1, n(Sup)),
                L is Sup - D2//C,
                domain_remove_smaller_than(VD1, L, VD2)
            ),
            fd_put(V, VD2, VPs)
        ;   true
        ),
        remove_dist_upper_lower(Cs, Vs, D1, D2).

remove_dist_upper([], _).
remove_dist_upper([C*V|CVs], D) :-
        (   fd_get(V, VD, VPs) ->
            (   C < 0 ->
                (   domain_supremum(VD, n(Sup)) ->
                    L is Sup + D//C,
                    domain_remove_smaller_than(VD, L, VD1)
                ;   VD1 = VD
                )
            ;   (   domain_infimum(VD, n(Inf)) ->
                    G is Inf + D//C,
                    domain_remove_greater_than(VD, G, VD1)
                ;   VD1 = VD
                )
            ),
            fd_put(V, VD1, VPs)
        ;   true
        ),
        remove_dist_upper(CVs, D).

remove_dist_lower([], _).
remove_dist_lower([C*V|CVs], D) :-
        (   fd_get(V, VD, VPs) ->
            (   C < 0 ->
                (   domain_infimum(VD, n(Inf)) ->
                    G is Inf - D//C,
                    domain_remove_greater_than(VD, G, VD1)
                ;   VD1 = VD
                )
            ;   (   domain_supremum(VD, n(Sup)) ->
                    L is Sup - D//C,
                    domain_remove_smaller_than(VD, L, VD1)
                ;   VD1 = VD
                )
            ),
            fd_put(V, VD1, VPs)
        ;   true
        ),
        remove_dist_lower(CVs, D).

remove_upper([], _).
remove_upper([C*X|CXs], Max) :-
        (   fd_get(X, XD, XPs) ->
            D is Max//C,
            (   C < 0 ->
                domain_remove_smaller_than(XD, D, XD1)
            ;   domain_remove_greater_than(XD, D, XD1)
            ),
            fd_put(X, XD1, XPs)
        ;   true
        ),
        remove_upper(CXs, Max).

remove_lower([], _).
remove_lower([C*X|CXs], Min) :-
        (   fd_get(X, XD, XPs) ->
            D is -Min//C,
            (   C < 0 ->
                domain_remove_greater_than(XD, D, XD1)
            ;   domain_remove_smaller_than(XD, D, XD1)
            ),
            fd_put(X, XD1, XPs)
        ;   true
        ),
        remove_lower(CXs, Min).

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

fetch_propagator(Propagator) :-
        pop_queue(Prop),
        arg(2, Prop, MState),
        arg(1, MState, State),
        (   State == dead -> fetch_propagator(Propagator)
        ;   Propagator = Prop
        ).

:- thread_initialization((make_queue,
                          nb_setval('$clpfd_queue_status', enabled))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing a CLP(FD) expression has two important side-effects: First,
   it constrains the variables occurring in the expression to
   integers. Second, it constrains some of them even more: For
   example, in X/Y and X mod Y, Y is constrained to be #\= 0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

constrain_to_integer(Var) :-
        fd_get(Var, D, Ps),
        fd_put(Var, D, Ps).

parse_clpfd(Expr, Result) :-
        (   cyclic_term(Expr) -> domain_error(clpfd_expression, Expr)
        ;   var(Expr) ->
            constrain_to_integer(Expr),
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
        ;   Expr = (L ^ R) ->
            parse_clpfd(L, RL), parse_clpfd(R, RR),
            myexp(RL, RR, Result)
        ;   domain_error(clpfd_expression, Expr)
        ).

trigger_once(Prop) :- trigger_prop(Prop), do_queue.

neq(A, B) :-
        make_propagator(pneq(A, B), Prop),
        init_propagator(A, Prop), init_propagator(B, Prop),
        trigger_once(Prop).

geq(A, B) :-
        (   fd_get(A, AD, APs) ->
            domain_infimum(AD, AI),
            (   fd_get(B, BD, _) ->
                domain_supremum(BD, BS),
                (   AI cis_geq BS -> true
                ;   make_propagator(pgeq(A,B), Prop),
                    init_propagator(A, Prop),
                    init_propagator(B, Prop),
                    trigger_once(Prop)
                )
            ;   domain_remove_smaller_than(AD, B, AD1),
                fd_put(A, AD1, APs),
                do_queue
            )
        ;   fd_get(B, BD, BPs) ->
            domain_remove_greater_than(BD, A, BD1),
            fd_put(B, BD1, BPs),
            do_queue
        ;   A >= B
        ).

myplus(X, Y, Z) :-
        (   X == Y -> 2*X #= Z
        ;   make_propagator(pplus(X,Y,Z), Prop),
            init_propagator(X, Prop), init_propagator(Y, Prop),
            init_propagator(Z, Prop), trigger_once(Prop)
        ).

mytimes(X, Y, Z) :-
        make_propagator(ptimes(X,Y,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_once(Prop).

mydiv(X, Y, Z) :-
        make_propagator(pdiv(X,Y,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_once(Prop).

myexp(X, Y, Z) :-
        make_propagator(pexp(X,Y,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_once(Prop).

myabs(X, Y) :-
        make_propagator(pabs(X,Y), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        trigger_prop(Prop), trigger_once(Prop).

mymod(X, M, K) :-
        make_propagator(pmod(X,M,K), Prop),
        init_propagator(X, Prop), init_propagator(M, Prop),
        init_propagator(K, Prop), trigger_once(Prop).

mymax(X, Y, Z) :-
        X #=< Z, Y #=< Z,
        make_propagator(pmax(X,Y,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_once(Prop).

mymin(X, Y, Z) :-
        X #>= Z, Y #>= Z,
        make_propagator(pmin(X,Y,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), trigger_once(Prop).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Naive parsing of inequalities and disequalities can result in a lot
   of unnecessary work if expressions of non-trivial depth are
   involved: Auxiliary variables are introduced for sub-expressions,
   and propagation proceeds on them as if they were involved in a
   tighter constraint (like equality), whereas eventually only very
   little of the propagated information is actually used. For example,
   only extremal values are of interest in inequalities. Introducing
   auxiliary variables should be avoided when possible, and
   specialised propagators should be used for common constraints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%% ?X #>= ?Y
%
% X is greater than or equal to Y.

X #>= Y :-
        (   var(X), nonvar(Y), Y = Y1 - C, var(Y1), integer(C) ->
            var_leq_var_plus_const(Y1, X, C),
            reinforce(X)
        ;   var(X), nonvar(Y), Y = Y1 + C, var(Y1), integer(C) ->
            C1 is -C,
            var_leq_var_plus_const(Y1, X, C1),
            reinforce(X)
        ;   nonvar(X), var(Y), X = X1 + C, var(X1), integer(C) ->
            var_leq_var_plus_const(Y, X1, C),
            reinforce(Y)
        ;   nonvar(X), var(Y), X = X1 - C, var(X1), integer(C) ->
            C1 is - C,
            var_leq_var_plus_const(Y, X1, C1),
            reinforce(Y)
        ;   parse_clpfd(X,RX), parse_clpfd(Y,RY), geq(RX,RY), reinforce(RX)
        ).

var_leq_var_plus_const(X, Y, C) :-
        make_propagator(x_leq_y_plus_c(X,Y,C), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        trigger_once(Prop).

%% ?X #=< ?Y
%
% X is less than or equal to Y.

X #=< Y :- Y #>= X.

%% ?X #= ?Y
%
% X equals Y.

linsum(X, S, S)    --> { var(X) }, !, [vn(X,1)].
linsum(-X, S, S)   --> { var(X) }, !, [vn(X,-1)].
linsum(I, S0, S)   --> { integer(I) }, !, { S is S0 + I }, [].
linsum(N*X, S, S)  --> { integer(N), var(X) }, !, [vn(X,N)].
linsum(X*N, S, S)  --> { integer(N), var(X) }, !, [vn(X,N)].
linsum(A+B, S0, S) --> linsum(A, S0, S1), linsum(B, S1, S).

i_or_v(I) :- integer(I), !.
i_or_v(V) :- var(V).

left_right_linsum_const(Left, Right, Cs, Vs, Const) :-
        \+ ( i_or_v(Left), i_or_v(Right) ),
        \+ ( nonvar(Left), Left = A+B, maplist(i_or_v, [A,B,Right]) ),
        \+ ( nonvar(Right), Right = A+B, maplist(i_or_v, [A,B,Left]) ),
        \+ ( nonvar(Left), Left = A*B, maplist(i_or_v, [A,B,Right]) ),
        \+ ( nonvar(Right), Right = A*B, maplist(i_or_v, [A,B,Left]) ),
        phrase(linsum(Left, 0, CL), Lefts0, Rights),
        phrase(linsum(Right, 0, CR), Rights0),
        maplist(linterm_negate, Rights0, Rights),
        msort(Lefts0, Lefts),
        Lefts = [vn(First,N)|LeftsRest],
        vns_coeffs_variables(LeftsRest, N, First, Cs0, Vs0),
        filter_linsum(Cs0, Vs0, Cs, Vs),
        Const is CR - CL.
        %format("linear sum: ~w ~w ~w\n", [Cs,Vs,Const]).

linterm_negate(vn(V,N0), vn(V,N)) :- N is -N0.

vns_coeffs_variables([], N, V, [N], [V]).
vns_coeffs_variables([vn(V,N)|VNs], N0, V0, Ns, Vs) :-
        (   V == V0 ->
            N1 is N0 + N,
            vns_coeffs_variables(VNs, N1, V0, Ns, Vs)
        ;   Ns = [N0|NRest],
            Vs = [V0|VRest],
            vns_coeffs_variables(VNs, N, V, NRest, VRest)
        ).

filter_linsum([], [], [], []).
filter_linsum([C0|Cs0], [V0|Vs0], Cs, Vs) :-
        (   C0 =:= 0 ->
            constrain_to_integer(V0),
            filter_linsum(Cs0, Vs0, Cs, Vs)
        ;   Cs = [C0|Cs1], Vs = [V0|Vs1],
            filter_linsum(Cs0, Vs0, Cs1, Vs1)
        ).

X #= Y  :-
        (   left_right_linsum_const(X, Y, Cs, Vs, S) ->
            (   Cs = [] -> S =:= 0
            ;   Cs = [C|CsRest],
                gcd(CsRest, C, GCD),
                S mod GCD =:= 0,
                scalar_product(Cs, Vs, #=, S)
            )
        ;   parse_clpfd(X,RX), parse_clpfd(Y,RX), reinforce(RX)
        ).

gcd([], G, G).
gcd([N|Ns], G0, G) :-
        gcd_(N, G0, G1),
        gcd(Ns, G1, G).

gcd_(A, B, G) :-
        (   B =:= 0 -> G = A
        ;   R is A mod B,
            gcd_(B, R, G)
        ).

%% ?X #\= ?Y
%
% X is not Y.

X #\= Y :-
        (   var(X), integer(Y) ->
            neq_num(X, Y),
            do_queue,
            reinforce(X)
        ;   var(X), nonvar(Y), Y = V - C, var(V), integer(C) ->
            var_neq_var_plus_const(V, X, C)
        ;   var(X), nonvar(Y), Y = V + C, var(V), integer(C) ->
            var_neq_var_plus_const(X, V, C)
        ;   nonvar(X), var(Y), X = V + C, var(V), integer(C) ->
            var_neq_var_plus_const(Y, V, C)
        ;   nonvar(X), var(Y), X = V - C, var(V), integer(C) ->
            var_neq_var_plus_const(V, Y, C)
        ;   nonvar(X), X = abs(A), nonvar(A), A = X1 - Y1, var(X1), var(Y1), integer(Y) ->
            absdiff_neq_const(X1, Y1, Y)
        ;   integer(X), nonvar(Y), Y = abs(A), nonvar(A), A = X1 - Y1, var(X1), var(Y1) ->
            absdiff_neq_const(X1, Y1, X)
        ;   left_right_linsum_const(X, Y, Cs, Vs, S) ->
            scalar_product(Cs, Vs, #\=, S)
        ;   parse_clpfd(X, RX), parse_clpfd(Y, RY), neq(RX, RY)
        ).

% abs(X-Y) #\= C

absdiff_neq_const(X, Y, C) :-
        (   C >= 0 ->
            make_propagator(absdiff_neq(X,Y,C), Prop),
            init_propagator(X, Prop), init_propagator(Y, Prop),
            trigger_once(Prop)
        ;   constrain_to_integer(X), constrain_to_integer(Y)
        ).

% X #\= Y + C

var_neq_var_plus_const(X, Y, C) :-
        make_propagator(x_neq_y_plus_c(X,Y,C), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        trigger_once(Prop).

% X is distinct from the number N. This is used internally, and does
% not reinforce other constraints.

neq_num(X, N) :-
        (   fd_get(X, XD, XPs) ->
            domain_remove(XD, N, XD1),
            fd_put(X, XD1, XPs)
        ;   X =\= N
        ).

%% ?X #> ?Y
%
% X is greater than Y.

X #> Y  :- X #>= Y + 1.

%% #<(?X, ?Y)
%
% X is less than Y.

X #< Y  :- Y #> X.

%% #\ +Q
%
% The reifiable constraint Q does _not_ hold.

#\ Q       :- reify(Q, 0), do_queue.

%% ?P #<==> ?Q
%
% P and Q are equivalent.

L #<==> R  :- reify(L, B), reify(R, B), do_queue.

%% ?P #==> ?Q
%
% P implies Q.

L #==> R   :- reify(L, BL), reify(R, BR), myimpl(BL, BR), do_queue.

%% ?P #<== ?Q
%
% Q implies P.

L #<== R   :- reify(L, BL), reify(R, BR), myimpl(BR, BL), do_queue.

%% ?P #/\ ?Q
%
% P and Q hold.

L #/\ R    :- reify(L, 1), reify(R, 1), do_queue.

%% ?P #\/ ?Q
%
% P or Q holds.

L #\/ R    :- reify(L, BL), reify(R, BR), myor(BL, BR, 1), do_queue.

myor(X, Y, Z) :-
        make_propagator(por(X,Y,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop),
        trigger_prop(Prop).

myimpl(X, Y) :-
        make_propagator(pimpl(X,Y), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        trigger_prop(Prop).

my_reified_div(X, Y, D, Z) :-
        make_propagator(reified_div(X,Y,D,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), init_propagator(D, Prop),
        trigger_once(Prop).

my_reified_mod(X, Y, D, Z) :-
        make_propagator(reified_mod(X,Y,D,Z), Prop),
        init_propagator(X, Prop), init_propagator(Y, Prop),
        init_propagator(Z, Prop), init_propagator(D, Prop),
        trigger_once(Prop).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A constraint that is being reified need not hold. Therefore, in
   X/Y, Y can as well be 0, for example. Note that it is OK to
   constrain the *result* of an expression (which does not appear
   explicitly in the expression and is not visible to the outside),
   but not the operands, except for requiring that they be integers.
   In contrast to parse_clpfd/2, the result of an expression can now
   also be undefined, in which case the constraint cannot hold.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

parse_reified_clpfd(Expr, Result, Defined) :-
        (   cyclic_term(Expr) -> domain_error(clpfd_expression, Expr)
        ;   var(Expr) ->
            constrain_to_integer(Expr),
            Result = Expr, Defined = 1
        ;   integer(Expr) -> Result = Expr, Defined = 1
        ;   Expr = (L + R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            myplus(RL, RR, Result), DL #/\ DR #<==> Defined
        ;   Expr = (L * R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            mytimes(RL, RR, Result), DL #/\ DR #<==> Defined
        ;   Expr = (L - R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            mytimes(-1, RR, RRR),
            myplus(RL, RRR, Result), DL #/\ DR #<==> Defined
        ;   Expr = (- E) ->
            parse_reified_clpfd(E, RE, Defined),
            mytimes(-1, RE, Result)
        ;   Expr = max(L, R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            mymax(RL, RR, Result), DL #/\ DR #<==> Defined
        ;   Expr = min(L,R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            mymin(RL, RR, Result), DL #/\ DR #<==> Defined
        ;   Expr = L mod R ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            DL #/\ DR #<==> Defined1,
            my_reified_mod(RL, RR, Defined2, Result),
            Defined1 #/\ Defined2 #<==> Defined
        ;   Expr = abs(E) ->
            parse_reified_clpfd(E, RE, Defined),
            myabs(RE, Result),
            Result #>= 0
        ;   Expr = (L / R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            DL #/\ DR #<==> Defined1,
            my_reified_div(RL, RR, Defined2, Result),
            Defined1 #/\ Defined2 #<==> Defined
        ;   Expr = (L ^ R) ->
            parse_reified_clpfd(L, RL, DL), parse_reified_clpfd(R, RR, DR),
            DL #/\ DR #<==> Defined,
            myexp(RL, RR, Result)
        ;   domain_error(clpfd_expression, Expr)
        ).

reify(Expr, B) :-
        B in 0..1,
        (   cyclic_term(Expr) -> domain_error(clpfd_reifiable_expression, Expr)
        ;   var(Expr) -> B = Expr
        ;   integer(Expr) -> B = Expr
        ;   Expr = (L #>= R) ->
            parse_reified_clpfd(L, LR, LD), parse_reified_clpfd(R, RR, RD),
            make_propagator(reified_geq(LD,LR,RD,RR,B), Prop),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop), init_propagator(LD, Prop),
            init_propagator(RD, Prop), trigger_prop(Prop)
        ;   Expr = (L #> R)  -> reify(L #>= (R+1), B)
        ;   Expr = (L #=< R) -> reify(R #>= L, B)
        ;   Expr = (L #< R)  -> reify(R #>= (L+1), B)
        ;   Expr = (L #= R)  ->
            parse_reified_clpfd(L, LR, LD), parse_reified_clpfd(R, RR, RD),
            make_propagator(reified_eq(LD,LR,RD,RR,B), Prop),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop), init_propagator(LD, Prop),
            init_propagator(RD, Prop), trigger_prop(Prop)
        ;   Expr = (L #\= R) ->
            parse_reified_clpfd(L, LR, LD), parse_reified_clpfd(R, RR, RD),
            make_propagator(reified_neq(LD,LR,RD,RR,B), Prop),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop), init_propagator(LD, Prop),
            init_propagator(RD, Prop), trigger_prop(Prop)
        ;   Expr = (L #==> R) -> reify((#\ L) #\/ R, B)
        ;   Expr = (L #<== R) -> reify(R #==> L, B)
        ;   Expr = (L #<==> R) -> reify((L #==> R) #/\ (R #==> L), B)
        ;   Expr = (L #/\ R) ->
            reify(L, LR), reify(R, RR),
            make_propagator(reified_and(LR,RR,B), Prop),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop),
            trigger_prop(Prop)
        ;   Expr = (L #\/ R) ->
            reify(L, LR), reify(R, RR),
            make_propagator(reified_or(LR,RR,B), Prop),
            init_propagator(LR, Prop), init_propagator(RR, Prop),
            init_propagator(B, Prop),
            trigger_prop(Prop)
        ;   Expr = (#\ Q) ->
            reify(Q, QR),
            make_propagator(reified_not(QR,B), Prop),
            init_propagator(QR, Prop), init_propagator(B, Prop),
            trigger_prop(Prop)
        ;   domain_error(clpfd_reifiable_expression, Expr)
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
        phrase(drep_to_intervals(DR), Is0),
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
        Next cis1 B0 + n(1),
        (   N cis_gt Next -> B = B0, Rest = [N-M|NMs]
        ;   B1 cis1 max(B0,M),
            merge_remaining(NMs, B1, B, Rest)
        ).

domain(V, Dom) :-
        (   fd_get(V, Dom0, VPs) ->
            domains_intersection(Dom, Dom0, Dom1),
            %format("intersected\n: ~w\n ~w\n==> ~w\n\n", [Dom,Dom0,Dom1]),
            fd_put(V, Dom1, VPs),
            do_queue,
            reinforce(V)
        ;   domain_contains(Dom, V)
        ).

domains([], _).
domains([V|Vs], D) :- domain(V, D), domains(Vs, D).


fd_get(X, Dom, Ps) :-
        (   get_attr(X, clpfd, Attr) -> Attr = clpfd(_,_,_,Dom,Ps)
        ;   var(X) -> default_domain(Dom), Ps = []
        ).

fd_get(X, Dom, Inf, Sup, Ps) :-
        fd_get(X, Dom, Ps),
        domain_infimum(Dom, Inf),
        domain_supremum(Dom, Sup).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   By default, propagation always terminates. Currently, this is
   ensured by allowing the left and right boundaries, as well as the
   distance between the smallest and largest number occurring in the
   domain representation to be changed at most once after a constraint
   is posted, unless the domain is bounded. Set the experimental
   Prolog flag 'clpfd_propagation' to 'full' to make the solver
   propagate as much as possible. This can make queries
   non-terminating, like: X #> abs(X), or: X #> Y, Y #> X, X #> 0.
   Importantly, it can also make labeling non-terminating, as in:

   ?- B #==> X #> abs(X), indomain(B).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fd_put(X, Dom, Pos) :-
        (   current_prolog_flag(clpfd_propagation, full) ->
            put_full(X, Dom, Pos)
        ;   put_terminating(X, Dom, Pos)
        ).

put_terminating(X, Dom, Ps) :-
        Dom \== empty,
        (   Dom = from_to(F, F) -> F = n(X)
        ;   (   get_attr(X, clpfd, Attr) ->
                Attr = clpfd(Left,Right,Spread,OldDom, _OldPs),
                put_attr(X, clpfd, clpfd(Left,Right,Spread,Dom,Ps)),
                (   OldDom == Dom -> true
                ;   (   Left == (.) -> Bounded = yes
                    ;   domain_infimum(Dom, Inf), domain_supremum(Dom, Sup),
                        (   Inf = n(_), Sup = n(_) ->
                            Bounded = yes
                        ;   Bounded = no
                        )
                    ),
                    (   Bounded == yes ->
                        put_attr(X, clpfd, clpfd(.,.,.,Dom,Ps)),
                        trigger_props(Ps)
                    ;   % infinite domain; consider border and spread changes
                        domain_infimum(OldDom, OldInf),
                        (   Inf == OldInf -> LeftP = Left
                        ;   LeftP = yes
                        ),
                        domain_supremum(OldDom, OldSup),
                        (   Sup == OldSup -> RightP = Right
                        ;   RightP = yes
                        ),
                        domain_spread(OldDom, OldSpread),
                        domain_spread(Dom, NewSpread),
                        (   NewSpread == OldSpread -> SpreadP = Spread
                        ;   SpreadP = yes
                        ),
                        put_attr(X, clpfd, clpfd(LeftP,RightP,SpreadP,Dom,Ps)),
                        (   RightP == yes, Right = yes -> true
                        ;   LeftP == yes, Left = yes -> true
                        ;   SpreadP == yes, Spread = yes -> true
                        ;   trigger_props(Ps)
                        )
                    )
                )
            ;   var(X) ->
                put_attr(X, clpfd, clpfd(no,no,no,Dom, Ps))
            ;   true
            )
        ).

domain_spread(Dom, Spread) :-
        domain_smallest_finite(Dom, S),
        domain_largest_finite(Dom, L),
        Spread cis1 L - S.

smallest_finite(inf, Y, Y).
smallest_finite(n(N), _, n(N)).

domain_smallest_finite(from_to(F,T), S)   :- smallest_finite(F, T, S).
domain_smallest_finite(split(_, L, _), S) :- domain_smallest_finite(L, S).

largest_finite(sup, Y, Y).
largest_finite(n(N), _, n(N)).

domain_largest_finite(from_to(F,T), L)   :- largest_finite(T, F, L).
domain_largest_finite(split(_, _, R), L) :- domain_largest_finite(R, L).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   With terminating propagation, all relevant constraints get a
   propagation opportunity whenever a new constraint is posted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

reinforce(X) :-
        (   current_prolog_flag(clpfd_propagation, full) ->
            % full propagation propagates everything in any case
            true
        ;   collect_variables(X, [], Vs),
            maplist(reinforce_, Vs),
            do_queue
        ).

collect_variables(X, Vs0, Vs) :-
        (   fd_get(X, _, Ps) ->
            term_variables(Ps, Vs1),
            %all_collect(Vs1, [X|Vs0], Vs)
            Vs = [X|Vs1]
        ;   Vs = Vs0
        ).

all_collect([], Vs, Vs).
all_collect([X|Xs], Vs0, Vs) :-
        (   member(V, Vs0), X == V -> all_collect(Xs, Vs0, Vs)
        ;   collect_variables(X, Vs0, Vs1),
            all_collect(Xs, Vs1, Vs)
        ).

reinforce_(X) :-
        (   fd_var(X), fd_get(X, Dom, Ps) ->
            put_full(X, Dom, Ps)
        ;   true
        ).

put_full(X, Dom, Ps) :-
        Dom \== empty,
        (   Dom = from_to(F, F) -> F = n(X)
        ;   (   get_attr(X, clpfd, Attr) ->
                Attr = clpfd(_,_,_,OldDom, _OldPs),
                put_attr(X, clpfd, clpfd(no,no,no,Dom, Ps)),
                %format("putting dom: ~w\n", [Dom]),
                (   OldDom == Dom -> true
                ;   trigger_props(Ps)
                )
            ;   var(X) -> %format('\t~w in ~w .. ~w\n',[X,L,U]),
                put_attr(X, clpfd, clpfd(no,no,no,Dom, Ps))
            ;   true
            )
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A propagator is a term of the form propagator(C, State), where C
   represents a constraint, and State is a term of the form
   mutable(S,X). S can be used to destructively change the state of
   the propagator. This can be used to avoid redundant invocation of
   the same propagator, or to disable the propagator. X is a free
   variable that prevents a factorizing garbage collector from folding
   unrelated states.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_propagator(C, propagator(C, mutable(passive, _))).

trigger_props([]).
trigger_props([P|Ps]) :- trigger_prop(P), trigger_props(Ps).

trigger_prop(Propagator) :-
        arg(2, Propagator, MState),
        arg(1, MState, State),
        (   State == dead -> true
        ;   State == queued -> true
        ;   % passive
            % format("triggering: ~w\n", [Propagator]),
            setarg(1, MState, queued),
            push_queue(Propagator)
        ).

kill(MState) :- setarg(1, MState, dead).

activate_propagator(propagator(P,MState)) :-
        arg(1, MState, State),
        (   State == dead -> true
        ;   %format("running: ~w\n", [P]),
            setarg(1, MState, passive),
            run_propagator(P, MState)
        ).

disable_queue :- b_setval('$clpfd_queue_status', disabled).
enable_queue  :- b_setval('$clpfd_queue_status', enabled), do_queue.

portray_propagator(propagator(P,_), F) :- functor(P, F, _).

portray_queue(V, []) :- var(V), !.
portray_queue([P|Ps], [F|Fs]) :-
        portray_propagator(P, F),
        portray_queue(Ps, Fs).

do_queue :-
        % b_getval('$clpfd_queue', H-_),
        % portray_queue(H, Port),
        % format("queue: ~w\n", [Port]),
        (   b_getval('$clpfd_queue_status', enabled) ->
            (   fetch_propagator(Propagator) ->
                activate_propagator(Propagator),
                do_queue
            ;   true
            )
        ;   true
        ).

init_propagator(Var, Prop) :-
        (   fd_get(Var, Dom, Ps) -> fd_put(Var, Dom, [Prop|Ps])
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% lex_chain(+Lists)
%
% Constrains Lists to be lexicographically non-decreasing.

lex_chain(Lss) :-
        must_be(list(list), Lss),
        lex_chain_(Lss).

lex_chain_([]).
lex_chain_([Ls|Lss]) :-
        lex_chain_lag(Lss, Ls),
        lex_chain_(Lss).

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


%% tuples_in(+Tuples, +Relation).
%
% Relation is a ground list of lists of integers. The elements of the
% list Tuples are constrained to be elements of Relation.

tuples_in(Tuples, Relation) :-
        must_be(list, Tuples),
        must_be(list(list(integer)), Relation),
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
                fd_get(T, TDom, TPs),
                domains_intersection(TDom, FDom, TDom1),
                fd_put(T, TDom1, TPs)
            )
        ;   true
        ),
        tuple_domain(Ts, Relation1).

take_firsts([], [], []).
take_firsts([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        take_firsts(Rest, Fs, Oss).

tuple_freeze(Tuple, Relation) :-
        make_propagator(rel_tuple(mutable(Relation,_),Tuple), Prop),
        tuple_freeze(Tuple, Tuple, Prop).

tuple_freeze([],  _, _).
tuple_freeze([T|Ts], Tuple, Prop) :-
        ( var(T) ->
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
        (   fd_get(T, Dom, _) ->
            domain_contains(Dom, A)
        ;   must_be(integer, T),
            T =:= A
        ),
        all_in_domain(As, Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(pdifferent(Left,Right,X,_), _MState) :-
        (   ground(X) ->
            disable_queue,
            exclude_fire(Left, Right, X),
            enable_queue
        ;   true
        ).

run_propagator(pdistinct(Left,Right,X,_), _MState) :-
        (   ground(X) ->
            disable_queue,
            exclude_fire(Left, Right, X),
            enable_queue
        ;   %outof_reducer(Left, Right, X)
            %(   var(X) -> kill_if_isolated(Left, Right, X, MState)
            %;   true
            %),
            true
        ).

run_propagator(check_distinct(Left,Right,X), _) :-
        \+ list_contains(Left, X),
        \+ list_contains(Right, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(pneq(A, B), MState) :-
        (   nonvar(A) ->
            (   nonvar(B) -> A =\= B, kill(MState)
            ;   fd_get(B, BD0, BExp0),
                domain_remove(BD0, A, BD1),
                kill(MState),
                fd_put(B, BD1, BExp0)
            )
        ;   nonvar(B) -> run_propagator(pneq(B, A), MState)
        ;   A \== B,
            fd_get(A, _, AI, AS, _), fd_get(B, _, BI, BS, _),
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
            ;   fd_get(B, BD, BPs),
                domain_remove_greater_than(BD, A, BD1),
                kill(MState),
                fd_put(B, BD1, BPs)
            )
        ;   nonvar(B) ->
            fd_get(A, AD, APs),
            domain_remove_smaller_than(AD, B, AD1),
            kill(MState),
            fd_put(A, AD1, APs)
        ;   fd_get(A, AD, AL, AU, APs),
            fd_get(B, _, BL, BU, _),
            AU cis_geq BL,
            (   AL cis_gt BU -> kill(MState)
            ;   AU == BL -> A = B
            ;   NAL cis1 max(AL,BL),
                domains_intersection(from_to(NAL,AU), AD, NAD),
                fd_put(A, NAD, APs),
                (   fd_get(B, BD2, BL2, BU2, BPs2) ->
                    NBU cis1 min(BU2, AU),
                    domains_intersection(from_to(BL2,NBU), BD2, NBD),
                    fd_put(B, NBD, BPs2)
                ;   true
                )
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(rel_tuple(Rel, Tuple), MState) :-
        arg(1, Rel, Relation),
        (   ground(Tuple) -> kill(MState), memberchk(Tuple, Relation)
        ;   relation_unifiable(Relation, Tuple, Us, 0, Changed),
            Us = [_|_],
            (   Us = [Single] -> kill(MState), Single = Tuple
            ;   Changed =:= 0 -> true
            ;   setarg(1, Rel, Us),
                disable_queue,
                tuple_domain(Tuple, Us),
                enable_queue
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(pserialized(Var,Duration,Left,SDs), _MState) :-
        myserialized(Duration, Left, SDs, Var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% abs(X-Y) #\= C
run_propagator(absdiff_neq(X,Y,C), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), abs(X - Y) =\= C
            ;   kill(MState),
                V1 is X - C, neq_num(Y, V1),
                V2 is C + X, neq_num(Y, V2)
            )
        ;   nonvar(Y) -> kill(MState),
            V1 is C + Y, neq_num(X, V1),
            V2 is Y - C, neq_num(X, V2)
        ;   true
        ).

% X #\= Y + C
run_propagator(x_neq_y_plus_c(X,Y,C), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), X =\= Y + C
            ;   kill(MState), R is X - C, neq_num(Y, R)
            )
        ;   nonvar(Y) -> kill(MState), R is Y + C, neq_num(X, R)
        ;   true
        ).

% X #=< Y + C
run_propagator(x_leq_y_plus_c(X,Y,C), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), X =< Y + C
            ;   kill(MState),
                R is X - C,
                fd_get(Y, YD, YPs),
                domain_remove_smaller_than(YD, R, YD1),
                fd_put(Y, YD1, YPs)
            )
        ;   nonvar(Y) ->
            kill(MState),
            R is Y + C,
            fd_get(X, XD, XPs),
            domain_remove_greater_than(XD, R, XD1),
            fd_put(X, XD1, XPs)
        ;   fd_get(Y, YD, _),
            (   domain_supremum(YD, n(YSup)) ->
                YS1 is YSup + C,
                fd_get(X, XD, XPs),
                domain_remove_greater_than(XD, YS1, XD1),
                fd_put(X, XD1, XPs)
            ;   true
            ),
            (   fd_get(X, XD2, _), domain_infimum(XD2, n(XInf)) ->
                XI1 is XInf - C,
                (   fd_get(Y, YD1, YPs1) ->
                    domain_remove_smaller_than(YD1, XI1, YD2),
                    (   domain_infimum(YD2, n(YInf)),
                        domain_supremum(XD2, n(XSup)),
                        XSup =< YInf + C ->
                        kill(MState)
                    ;   true
                    ),
                    fd_put(Y, YD2, YPs1)
                ;   true
                )
            ;   true
            )
        ).

run_propagator(scalar_product(Cs0,Vs0,Op,P0), MState) :-
        coeffs_variables_const(Cs0, Vs0, Cs, Vs, 0, I),
        P is P0 - I,
        (   Op == (#\=) ->
            (   Vs = [] -> kill(MState), P =\= 0
            ;   Vs = [V], Cs = [C] ->
                kill(MState),
                (   C =:= 1 -> neq_num(V, P)
                ;   C*V #\= P
                )
            ;   true
            )
        ;   Op == (#=) ->
            (   Vs = [] -> kill(MState), P =:= 0
            ;   Vs = [V], Cs = [C] ->
                kill(MState),
                P mod C =:= 0,
                V is P // C
            ;   Cs == [1,1] -> kill(MState), Vs = [A,B], A + B #= P
            ;   Cs == [-1,1] -> kill(MState), Vs = [A,B], B - P #= A
            ;   Cs == [1,-1] -> kill(MState), Vs = [A,B], A - B #= P
            ;   sum_finite_domains(Cs, Vs, Infs, Sups, 0, 0, Inf, Sup),
                % nl, write(Infs-Sups-Inf-Sup), nl,
                D1 is P - Inf,
                D2 is Sup - P,
                (   Infs == [], Sups == [] ->
                    Inf =< P,
                    P =< Sup,
                    remove_dist_upper_lower(Cs, Vs, D1, D2)
                ;   Sups = [] ->
                    P =< Sup,
                    remove_dist_lower(Infs, D2)
                ;   Infs = [] ->
                    Inf =< P,
                    remove_dist_upper(Sups, D1)
                ;   Sups = [_], Infs = [_] ->
                    remove_lower(Sups, D2),
                    remove_upper(Infs, D1)
                ;   Infs = [_] ->
                    remove_upper(Infs, D1)
                ;   Sups = [_] ->
                    remove_lower(Sups, D2)
                ;   true
                )
            )
        ).

% X + Y = Z
run_propagator(pplus(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   X =:= 0 -> kill(MState), Y = Z
            ;   nonvar(Y) -> kill(MState), Z is X + Y
            ;   nonvar(Z) -> kill(MState), Y is Z - X
            ;   fd_get(Z, ZD, ZPs),
                fd_get(Y, YD, _),
                domain_shift(YD, X, Shifted_YD),
                domains_intersection(ZD, Shifted_YD, ZD1),
                fd_put(Z, ZD1, ZPs),
                (   fd_get(Y, YD1, YPs) ->
                    O is -X,
                    domain_shift(ZD1, O, YD2),
                    domains_intersection(YD1, YD2, YD3),
                    fd_put(Y, YD3, YPs)
                ;   true
                )
            )
        ;   nonvar(Y) -> run_propagator(pplus(Y,X,Z), MState)
        ;   nonvar(Z) ->
            fd_get(X, XD, _),
            fd_get(Y, YD, YPs),
            domain_negate(XD, XDN),
            domain_shift(XDN, Z, YD1),
            domains_intersection(YD, YD1, YD2),
            fd_put(Y, YD2, YPs),
            (   fd_get(X, XD1, XPs) ->
                domain_negate(YD2, YD2N),
                domain_shift(YD2N, Z, XD2),
                domains_intersection(XD1, XD2, XD3),
                fd_put(X, XD3, XPs)
            ;   true
            )
        ;   (   X == Y, fd_get(Z, ZD, _), \+ domain_contains(ZD, 0) ->
                neq_num(X, 0)
            ;   true
            ),
            (   fd_get(X, XD, XL, XU, XPs), fd_get(Y, YD, YL, YU, YPs),
                fd_get(Z, ZD, ZL, ZU, _) ->
                NXL cis max(XL, ZL-YU),
                NXU cis min(XU, ZU-YL),
                (   NXL == XL, NXU == XU -> true
                ;   domains_intersection(XD, from_to(NXL, NXU), NXD),
                    fd_put(X, NXD, XPs)
                ),
                (   fd_get(Y, YD2, YL2, YU2, YPs2) ->
                    NYL cis max(YL2, ZL-NXU),
                    NYU cis min(YU2, ZU-NXL),
                    (   NYL == YL2, NYU == YU2 -> true
                    ;   domains_intersection(YD2, from_to(NYL, NYU), NYD),
                        fd_put(Y, NYD, YPs2)
                    )
                ;   NYL = Y, NYU = Y
                ),
                (   fd_get(Z, ZD2, ZL2, ZU2, ZPs2) ->
                    NZL cis max(ZL2,NXL+NYL),
                    NZU cis min(ZU2,NXU+NYU),
                    (   NZL == ZL2, NZU == ZU2 -> true
                    ;   domains_intersection(ZD2, from_to(NZL,NZU), NZD),
                        fd_put(Z, NZD, ZPs2)
                    )
                ;   true
                )
            ;   true
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(ptimes(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is X * Y
            ;   X =:= 0 -> kill(MState), Z = 0
            ;   X =:= 1 -> kill(MState), Z = Y
            ;   nonvar(Z) -> kill(MState), 0 =:= Z mod X, Y is Z // X
            ;   fd_get(Y, YD, _),
                fd_get(Z, ZD, ZPs),
                domain_expand(YD, X, Scaled_YD),
                domains_intersection(ZD, Scaled_YD, ZD1),
                fd_put(Z, ZD1, ZPs),
                (   fd_get(Y, YDom2, YPs2) ->
                    domain_contract(ZD1, X, Contract),
                    domains_intersection(YDom2, Contract, NYDom),
                    fd_put(Y, NYDom, YPs2)
                ;   kill(MState), Z is X * Y
                )
            )
        ;   nonvar(Y) -> run_propagator(ptimes(Y,X,Z), MState)
        ;   nonvar(Z) ->
            (   X == Y ->
                Z >= 0,
                catch(PRoot is floor(sqrt(Z)),error(evaluation_error(float_overflow), _), true),
                (   nonvar(PRoot), PRoot**2 =:= Z ->
                    kill(MState),
                    NRoot is -PRoot,
                    fd_get(X, TXD, TXPs), % temporary variables for this section
                    (   PRoot =:= 0 -> TXD1 = from_to(n(0),n(0))
                    ;   TXD1 = split(0, from_to(n(NRoot),n(NRoot)),
                                     from_to(n(PRoot),n(PRoot)))
                    ),
                    domains_intersection(TXD, TXD1, TXD2),
                    fd_put(X, TXD2, TXPs)
                ;   % be more tolerant until GMP integer sqrt is available
                    true
                )
            ;   true
            ),
            (   fd_get(X, XD, XL, XU, XPs) ->
                fd_get(Y, YD, YL, YU, _),
                min_divide(n(Z), n(Z), YL, YU, TNXL),
                max_divide(n(Z), n(Z), YL, YU, TNXU),
                NXL cis1 max(XL,TNXL),
                NXU cis1 min(XU,TNXU),
                (   NXL == XL, NXU == XU -> true
                ;   domains_intersection(from_to(NXL,NXU), XD, XD1),
                    fd_put(X, XD1, XPs)
                ),
                (   fd_get(Y, YD2, YL2, YU2,YExp2) ->
                    min_divide(n(Z), n(Z), NXL, NXU, NYL),
                    max_divide(n(Z), n(Z), NXL, NXU, NYU),
                    (   NYL cis_leq YL2, NYU cis_geq YU2 -> true
                    ;   domains_intersection(from_to(NYL,NYU), YD2, YD3),
                        fd_put(Y, YD3, YExp2)
                    )
                ;   (   Y \== 0 -> 0 =:= Z mod Y, kill(MState), X is Z // Y
                    ;   kill(MState), Z = 0
                    )
                )
            ;   true
            ),
            (   Z =\= 0 -> neq_num(X, 0), neq_num(Y, 0)
            ;   true
            )
        ;   (   X == Y -> geq(Z, 0) ; true ),
            (   fd_get(X, XD, XL, XU, XExp), fd_get(Y, YD, YL, YU, _),
                fd_get(Z, ZD, ZL, ZU, _) ->
                min_divide(ZL,ZU,YL,YU,TXL),
                NXL cis1 max(XL,TXL),
                max_divide(ZL,ZU,YL,YU,TXU),
                NXU cis1 min(XU,TXU),
                (   NXL == XL, NXU == XU -> true
                ;   domains_intersection(from_to(NXL,NXU), XD, XD1),
                    fd_put(X, XD1, XExp)
                ),
                (   fd_get(Y,YD2,YL2,YU2,YExp2) ->
                    min_divide(ZL,ZU,XL,XU,TYL),
                    NYL cis1 max(YL2,TYL),
                    max_divide(ZL,ZU,XL,XU,TYU),
                    NYU cis1 min(YU2,TYU),
                    (   NYL == YL2, NYU == YU2 -> true
                    ;   domains_intersection(from_to(NYL,NYU), YD2, YD3),
                        fd_put(Y, YD3, YExp2)
                    )
                ;   NYL = Y, NYU = Y
                ),
                (   fd_get(Z, ZD2, ZL2, ZU2, ZExp2) ->
                    min_times(NXL,NXU,NYL,NYU,NZL),
                    max_times(NXL,NXU,NYL,NYU,NZU),
                    (   NZL cis_leq ZL2, NZU cis_geq ZU2 -> true
                    ;   domains_intersection(from_to(NZL,NZU), ZD2, ZD3),
                        fd_put(Z, ZD3, ZExp2)
                    )
                ;   true
                )
            ;   true
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X / Y = Z

run_propagator(pdiv(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Y =\= 0, Z is X // Y
            ;   fd_get(Y, YD, YL, YU, YPs),
                (   nonvar(Z) ->
                    (   Z =:= 0 ->
                        NYL is -abs(X) - 1,
                        NYU is abs(X) + 1,
                        domains_intersection(split(0, from_to(inf,n(NYL)),
                                                   from_to(n(NYU), sup)),
                                             YD, NYD),
                        fd_put(Y, NYD, YPs)
                    ;   (   sign(X) =:= sign(Z) ->
                            NYL cis max(n(X) // (n(Z)+sign(n(Z))) + n(1), YL),
                            NYU cis min(n(X) // n(Z), YU)
                        ;   NYL cis max(n(X) // n(Z), YL),
                            NYU cis min(n(X) // (n(Z)+sign(n(Z))) - n(1), YU)
                        ),
                        (   NYL = YL, NYU = YU -> true
                        ;   domains_intersection(from_to(NYL,NYU), YD, NYD),
                            fd_put(Y, NYD, YPs)
                        )
                    )
                ;   fd_get(Z, ZD, ZL, ZU, ZPs),
                    (   X >= 0, YL cis_gt n(0) ->
                        NZL cis max(n(X)//YU, ZL),
                        NZU cis min(n(X)//YL, ZU)
                    ;   % TODO: more stringent bounds, cover Y
                        NZL cis max(-abs(n(X)), ZL),
                        NZU cis min(abs(n(X)), ZU)
                    ),
                    (   NZL = ZL, NZU = ZU -> true
                    ;   domains_intersection(from_to(NZL,NZU), ZD, NZD),
                        fd_put(Z, NZD, ZPs)
                    )
                )
            )
        ;   nonvar(Y) ->
            Y =\= 0,
            (   Y =:= 1 -> kill(MState), X = Z
            ;   Y =:= -1 -> kill(MState), Z #= -X
            ;   fd_get(X, XD, XL, XU, XPs),
                (   nonvar(Z) ->
                    (   sign(Z) =:= sign(Y) ->
                        NXL cis max(n(Z)*n(Y), XL),
                        NXU cis min((abs(n(Z))+n(1))*abs(n(Y))-n(1), XU)
                    ;   Z =:= 0 ->
                        NXL cis max(-abs(n(Y)) + n(1), XL),
                        NXU cis min(abs(n(Y)) - n(1), XU)
                    ;   NXL cis max((n(Z)+sign(n(Z))*n(1))*n(Y)+n(1), XL),
                        NXU cis min(n(Z)*n(Y), XU)
                    ),
                    (   NXL == XL, NXU == XU -> true
                    ;   domains_intersection(from_to(NXL,NXU), XD, NXD),
                        fd_put(X, NXD, XPs)
                    )
                ;   fd_get(Z, ZD, ZPs),
                    domain_contract_less(XD, Y, Contracted),
                    domains_intersection(ZD, Contracted, NZD),
                    fd_put(Z, NZD, ZPs),
                    (   \+ domain_contains(NZD, 0), fd_get(X, XD2, XPs2) ->
                        domain_expand_more(NZD, Y, Expanded),
                        domains_intersection(XD2, Expanded, NXD2),
                        fd_put(X, NXD2, XPs2)
                    ;   true
                    )
                )
            )
        ;   nonvar(Z) ->
            fd_get(X, XD, XL, XU, XPs),
            fd_get(Y, YD, YL, YU, YPs),
            (   YL cis_geq n(0), XL cis_geq n(0) ->
                NXL cis max(YL*n(Z), XL),
                NXU cis min(YU*(n(Z)+n(1))-n(1), XU)
            ;   %TODO: cover more cases
                NXL = XL, NXU = XU
            ),
            (   NXL == XL, NXU == XU -> true
            ;   domains_intersection(from_to(NXL,NXU), XD, NXD),
                fd_put(X, NXD, XPs)
            )
        ;   (   X == Y -> Z = 1
            ;   fd_get(X, _, XL, XU, _),
                fd_get(Y, _, YL, YU, _),
                fd_get(Z, ZD, ZPs),
                NZU cis max(abs(XL), XU),
                NZL cis1 -NZU,
                domains_intersection(from_to(NZL,NZU), ZD, NZD0),
                (   cis_geq_zero(XL), cis_geq_zero(YL) ->
                    domain_remove_smaller_than(NZD0, 0, NZD1)
                ;   % TODO: cover more cases
                    NZD1 = NZD0
                ),
                fd_put(Z, NZD1, ZPs)
            )
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Y = abs(X)

run_propagator(pabs(X,Y), MState) :-
        (   nonvar(X) -> kill(MState), Y is abs(X)
        ;   nonvar(Y) ->
            Y >= 0,
            (   Y =:= 0 -> X = 0
            ;   fd_get(X, XD, XPs),
                YN is -Y,
                domains_intersection(split(0, from_to(n(YN),n(YN)),
                                           from_to(n(Y),n(Y))), XD, XD1),
                fd_put(X, XD1, XPs)
            )
        ;   fd_get(X, XD, XPs),
            fd_get(Y, YD, _),
            domain_negate(YD, YDNegative),
            domains_union(YD, YDNegative, XD1),
            domains_intersection(XD, XD1, XD2),
            fd_put(X, XD2, XPs),
            (   fd_get(Y, YD1, YPs1) ->
                domain_negate(XD2, XD2Neg),
                domains_union(XD2, XD2Neg, YD2),
                domain_remove_smaller_than(YD2, 0, YD3),
                domains_intersection(YD1, YD3, YD4),
                fd_put(Y, YD4, YPs1)
            ;   true
            )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% K = X mod M

run_propagator(pmod(X,M,K), MState) :-
        (   nonvar(X) ->
            (   nonvar(M) -> kill(MState), M =\= 0, K is X mod M
            ;   true
            )
        ;   nonvar(M) ->
            M =\= 0,
            (   M =:= 1 -> K = 0
            ;   fd_get(K, KD, KPs) ->
                MP is abs(M) - 1,
                MN is -MP,
                fd_get(K, KD, KPs),
                domains_intersection(from_to(n(MN), n(MP)), KD, KD1),
                fd_put(K, KD1, KPs),
                (   fd_get(X, XD, _), domain_infimum(XD, n(Min)) ->
                    K1 is Min mod M,
                    (   domain_contains(KD1, K1) -> true
                    ;   neq_num(X, Min)
                    )
                ;   true
                )
            ;   fd_get(X, XD, XPs),
                (   fail, domain_supremum(XD, n(_)), domain_infimum(XD, n(_)) ->
                    % bounded domain (propagation currently disabled)
                    kill(MState),
                    findall(E, (domain_to_list(XD, XLs),
                                   member(E, XLs), E mod M =:= K), Es),
                    list_to_domain(Es, XD1),
                    domains_intersection(XD, XD1, XD2),
                    fd_put(X, XD2, XPs)
                ;   % if possible, propagate at the boundaries
                    (   nonvar(K), domain_infimum(XD, n(Min)) ->
                        (   Min mod M =:= K -> true
                        ;   neq_num(X, Min)
                        )
                    ;   true
                    ),
                    (   nonvar(K), domain_supremum(XD, n(Max)) ->
                        (   Max mod M =:= K -> true
                        ;   neq_num(X, Max)
                        )
                    ;   true
                    )
                )
            )
        ;   true % TODO: propagate more
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z = max(X,Y)

run_propagator(pmax(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is max(X,Y)
            ;   nonvar(Z) ->
                (   Z =:= X -> kill(MState), X #>= Y
                ;   Z > X -> Z = Y
                ;   fail % Z < X
                )
            ;   fd_get(Y, YD, YInf, YSup, _),
                (   YInf cis_gt n(X) -> Z = Y
                ;   YSup cis_lt n(X) -> Z = X
                ;   YSup = n(M) ->
                    fd_get(Z, ZD, ZPs),
                    domain_remove_greater_than(ZD, M, ZD1),
                    fd_put(Z, ZD1, ZPs)
                ;   true
                )
            )
        ;   nonvar(Y) -> run_propagator(pmax(Y,X,Z), MState)
        ;   fd_get(Z, ZD, ZPs) ->
            fd_get(X, _, XInf, XSup, _),
            fd_get(Y, YD, YInf, YSup, _),
            (   YInf cis_gt YSup -> Z = Y
            ;   YSup cis_lt XInf -> Z = X
            ;   n(M) cis1 max(XSup, YSup) ->
                domain_remove_greater_than(ZD, M, ZD1),
                fd_put(Z, ZD1, ZPs)
            ;   true
            )
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z = min(X,Y)

run_propagator(pmin(X,Y,Z), MState) :-
        (   nonvar(X) ->
            (   nonvar(Y) -> kill(MState), Z is min(X,Y)
            ;   nonvar(Z) ->
                (   Z =:= X -> kill(MState), X #=< Y
                ;   Z < X -> Z = Y
                ;   fail % Z > X
                )
            ;   fd_get(Y, YD, YInf, YSup, _),
                (   YSup cis_lt n(X) -> Z = Y
                ;   YInf cis_gt n(X) -> Z = X
                ;   YInf = n(M) ->
                    fd_get(Z, ZD, ZPs),
                    domain_remove_smaller_than(ZD, M, ZD1),
                    fd_put(Z, ZD1, ZPs)
                ;   true
                )
            )
        ;   nonvar(Y) -> run_propagator(pmin(Y,X,Z), MState)
        ;   fd_get(Z, ZD, ZPs) ->
            fd_get(X, _, XInf, XSup, _),
            fd_get(Y, YD, YInf, YSup, _),
            (   YSup cis_lt YInf -> Z = Y
            ;   YInf cis_gt XSup -> Z = X
            ;   n(M) cis1 min(XInf, YInf) ->
                domain_remove_smaller_than(ZD, M, ZD1),
                fd_put(Z, ZD1, ZPs)
            ;   true
            )
        ;   true
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z = X ^ Y

run_propagator(pexp(X,Y,Z), MState) :-
        (   X == 1 -> kill(MState), Z = 1
        ;   X == 0 -> kill(MState), Z #<==> Y #= 0
        ;   Y == 1 -> kill(MState), Z = X
        ;   Y == 0 -> kill(MState), Z = 1
        ;   nonvar(X), nonvar(Y) ->
            ( Y >= 0 -> true ; X =:= -1 ),
            kill(MState),
            Z is X**Y
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% reified constraints

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The result of X/Y and X mod Y is undefined iff Y is 0.

run_propagator(reified_div(X,Y,D,Z), MState) :-
        (   Y == 0 -> kill(MState), D = 0
        ;   D == 1 -> kill(MState), Z #= X / Y
        ;   integer(Y), Y =\= 0 -> kill(MState), D = 1, Z #= X / Y
        ;   fd_get(Y, YD, _), \+ domain_contains(YD, 0) ->
            kill(MState),
            D = 1, Z #= X / Y
        ;   true
        ).

run_propagator(reified_mod(X,Y,D,Z), MState) :-
        (   Y == 0 -> kill(MState), D = 0
        ;   D == 1 -> kill(MState), Z #= X mod Y
        ;   integer(Y), Y =\= 0 -> kill(MState), D = 1, Z #= X mod Y
        ;   fd_get(Y, YD, _), \+ domain_contains(YD, 0) ->
            kill(MState),
            D = 1, Z #= X mod Y
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(reified_geq(DX,X,DY,Y,B), MState) :-
        (   DX == 0 -> kill(MState), B = 0
        ;   DY == 0 -> kill(MState), B = 0
        ;   B == 1 -> kill(MState), DX = 1, DY = 1, X #>= Y
        ;   DX == 1, DY == 1 ->
            (   var(B) ->
                (   nonvar(X) ->
                    (   nonvar(Y) ->
                        kill(MState),
                        (   X >= Y -> B = 1 ; B = 0 )
                    ;   fd_get(Y, _, YL, YU, _),
                        (   n(X) cis_geq YU -> kill(MState), B = 1
                        ;   n(X) cis_lt YL -> kill(MState), B = 0
                        ;   true
                        )
                    )
                ;   nonvar(Y) ->
                    fd_get(X, _, XL, XU, _),
                    (   XL cis_geq n(Y) -> kill(MState), B = 1
                    ;   XU cis_lt n(Y) -> kill(MState), B = 0
                    ;   true
                    )
                ;   fd_get(X, _, XL, XU, _),
                    fd_get(Y, _, YL, YU, _),
                    (   XL cis_geq YU -> kill(MState), B = 1
                    ;   XU cis_lt YL -> kill(MState), B = 0
                    ;   true
                    )
                )
            ;   B =:= 0 -> kill(MState), X #< Y
            ;   true
            )
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_eq(DX,X,DY,Y,B), MState) :-
        (   DX == 0 -> kill(MState), B = 0
        ;   DY == 0 -> kill(MState), B = 0
        ;   B == 1 -> kill(MState), DX = 1, DY = 1, X = Y
        ;   DX == 1, DY == 1 ->
            (   var(B) ->
                (   nonvar(X) ->
                    (   nonvar(Y) ->
                        kill(MState),
                        (   X =:= Y -> B = 1 ; B = 0)
                    ;   fd_get(Y, YD, _),
                        (   domain_contains(YD, X) -> true
                        ;   kill(MState), B = 0
                        )
                    )
                ;   nonvar(Y) -> run_propagator(reified_eq(DY,Y,DX,X,B), MState)
                ;   X == Y -> kill(MState), B = 1
                ;   fd_get(X, _, XL, XU, _),
                    fd_get(Y, _, YL, YU, _),
                    (   XL cis_gt YU -> kill(MState), B = 0
                    ;   YL cis_gt XU -> kill(MState), B = 0
                    ;   true
                    )
                )
            ;   B =:= 0 -> kill(MState), X #\= Y
            ;   true
            )
        ;   true
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_neq(DX,X,DY,Y,B), MState) :-
        (   DX == 0 -> kill(MState), B = 0
        ;   DY == 0 -> kill(MState), B = 0
        ;   B == 1 -> kill(MState), DX = 1, DY = 1, X #\= Y
        ;   DX == 1, DY == 1 ->
            (   var(B) ->
                (   nonvar(X) ->
                    (   nonvar(Y) ->
                        kill(MState),
                        (   X =\= Y -> B = 1 ; B = 0)
                    ;   fd_get(Y, YD, _),
                        (   domain_contains(YD, X) -> true
                        ;   B = 1
                        )
                    )
                ;   nonvar(Y) -> run_propagator(reified_neq(DY,Y,DX,X,B), MState)
                ;   X == Y -> B = 0
                ;   fd_get(X, _, XL, XU, _),
                    fd_get(Y, _, YL, YU, _),
                    (   XL cis_gt YU -> kill(MState), B = 1
                    ;   YL cis_gt XU -> kill(MState), B = 1
                    ;   true
                    )
                )
            ;   B =:= 0 -> kill(MState), X = Y
            ;   true
            )
        ;   true
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_and(X,Y,B), MState) :-
        (   var(B) ->
            (   nonvar(X) ->
                (   X =:= 0 -> B = 0
                ;   X =:= 1 -> B = Y
                )
            ;   nonvar(Y) -> run_propagator(reified_and(Y,X,B), MState)
            ;   true
            )
        ;   B =:= 0 ->
            (   X == 1 -> kill(MState), Y = 0
            ;   Y == 1 -> kill(MState), X = 0
            ;   true
            )
        ;   B =:= 1 -> kill(MState), X = 1, Y = 1
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_or(X,Y,B), MState) :-
        (   var(B) ->
            (   nonvar(X) ->
                (   X =:= 1 -> B = 1
                ;   X =:= 0 -> B = Y
                )
            ;   nonvar(Y) -> run_propagator(reified_or(Y,X,B), MState)
            ;   true
            )
        ;   B =:= 0 -> kill(MState), X = 0, Y = 0
        ;   B =:= 1 ->
            (   X == 0 -> Y = 1
            ;   Y == 0 -> X = 1
            ;   true
            )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(reified_not(X,Y), MState) :-
        (   X == 0 -> kill(MState), Y = 1
        ;   X == 1 -> kill(MState), Y = 0
        ;   Y == 0 -> kill(MState), X = 1
        ;   Y == 1 -> kill(MState), X = 0
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_propagator(pimpl(X, Y), MState) :-
        (   nonvar(X) ->
            (   X =:= 1 -> kill(MState), Y = 1
            ;   kill(MState)
            )
        ;   nonvar(Y) ->
            (   Y =:= 0 -> kill(MState), X = 0
            ;   kill(MState)
            )
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_propagator(por(X, Y, Z), MState) :-
        (   nonvar(X) ->
            (   X =:= 0 -> Y = Z
            ;   X =:= 1 -> Z = 1
            )
        ;   nonvar(Y) -> run_propagator(por(Y,X,Z), MState)
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

min_times(L1,U1,L2,U2,Min) :-
        Min cis min(min(L1*L2,L1*U2),min(U1*L2,U1*U2)).
max_times(L1,U1,L2,U2,Max) :-
        Max cis max(max(L1*L2,L1*U2),max(U1*L2,U1*U2)).


min_divide_less(L1,U1,L2,U2,Min) :-
        (   L2 cis_leq n(0), cis_geq_zero(U2) -> Min = inf
        ;   Min cis min(min(div(L1,L2),div(L1,U2)),min(div(U1,L2),div(U1,U2)))
        ).
max_divide_less(L1,U1,L2,U2,Max) :-
        (   L2 cis_leq n(0), cis_geq_zero(U2) -> Max = sup
        ;   Max cis max(max(div(L1,L2),div(L1,U2)),max(div(U1,L2),div(U1,U2)))
        ).


min_divide(L1,U1,L2,U2,Min) :-
        (   L2 = n(NL2), NL2 > 0, U2 = n(_), cis_geq_zero(L1) ->
            Min cis div(L1+U2-n(1),U2)
                                % TODO: cover more cases
        ;   L2 cis_leq n(0), cis_geq_zero(U2) -> Min = inf
        ;   Min cis min(min(div(L1,L2),div(L1,U2)),min(div(U1,L2),div(U1,U2)))
        ).
max_divide(L1,U1,L2,U2,Max) :-
        (   L2 = n(_), cis_geq_zero(L1), cis_geq_zero(L2) ->
            Max cis1 div(U1,L2)
                                % TODO: cover more cases
        ;   L2 cis_leq n(0), cis_geq_zero(U2) -> Max = sup
        ;   Max cis max(max(div(L1,L2),div(L1,U2)),max(div(U1,L2),div(U1,U2)))
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Weak arc consistent all_distinct/1 constraint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%% all_distinct(+Ls).
%
% Like all_different/1, with stronger propagation.

%all_distinct(Ls) :- all_different(Ls).
all_distinct(Ls) :-
        must_be(list, Ls),
        length(Ls, _),
        MState = mutable(shared,_),
        all_distinct(Ls, [], MState),
        do_queue.

all_distinct([], _, _).
all_distinct([X|Right], Left, MState) :-
        %\+ list_contains(Right, X),
        (   var(X) ->
            make_propagator(pdistinct(Left,Right,X,MState), Prop),
            init_propagator(X, Prop),
            trigger_prop(Prop)
%             make_propagator(check_distinct(Left,Right,X), Prop2),
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
        (   fd_get(V, VD, VPs) ->
            domain_remove(VD, Val, VD1),
            fd_put(V, VD1, VPs)
        ;   V =\= Val
        ),
        exclude_list(Vs, Val).

list_contains([X|Xs], Y) :-
        (   X == Y -> true
        ;   list_contains(Xs, Y)
        ).

kill_if_isolated(Left, Right, X, MState) :-
        append(Left, Right, Others),
        fd_get(X, XDom, _),
        (   all_empty_intersection(Others, XDom) -> kill(MState)
        ;   true
        ).

all_empty_intersection([], _).
all_empty_intersection([V|Vs], XDom) :-
        (   fd_get(V, VDom, _) ->
            domains_intersection_(VDom, XDom, empty),
            all_empty_intersection(Vs, XDom)
        ;   all_empty_intersection(Vs, XDom)
        ).

outof_reducer(Left, Right, Var) :-
        (   fd_get(Var, Dom, _) ->
            append(Left, Right, Others),
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
        (   fd_get(X, XDom, XPs) ->
            domain_subtract(XDom, Dom, NXDom),
            fd_put(X, NXDom, XPs)
        ;   true
        ),
        reduce_from_others(Xs, Dom).

num_subsets([], _Dom, Num, Num, []).
num_subsets([S|Ss], Dom, Num0, Num, NonSubs) :-
        (   fd_get(S, SDom, _) ->
            (   domain_subdomain(Dom, SDom) ->
                Num1 is Num0 + 1,
                num_subsets(Ss, Dom, Num1, Num, NonSubs)
            ;   NonSubs = [S|Rest],
                num_subsets(Ss, Dom, Num0, Num, Rest)
            )
        ;   num_subsets(Ss, Dom, Num0, Num, NonSubs)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%      serialized(+Starts, +Durations)
%
%       Constrain a set of intervals to a non-overlapping sequence.
%       Starts = [S_1,...,S_n], is a list of variables or integers,
%       Durations = [D_1,...,D_n] is a list of non-negative integers.
%       Constrains Starts and Durations to denote a set of
%       non-overlapping tasks, i.e.: S_i + D_i =< S_j or S_j + D_j =<
%       S_i for all 1 =< i < j =< n.
%
%  @see Dorndorf et al. 2000, "Constraint Propagation Techniques for the
%       Disjunctive Scheduling Problem"

serialized(Starts, Durations) :-
        must_be(list(integer), Durations),
        pair_up(Starts, Durations, SDs),
        serialize(SDs, []),
        do_queue.

pair_up([], [], []).
pair_up([A|As], [B|Bs], [A-n(B)|ABs]) :- pair_up(As, Bs, ABs).

% attribute: pserialized(Var, Duration, Left, Right)
%   Left and Right are lists of Start-Duration pairs representing
%   other tasks occupying the same resource

serialize([], _).
serialize([Start-D|SDs], Left) :-
        cis_geq_zero(D),
        (   var(Start) ->
            make_propagator(pserialized(Start,D,Left,SDs), Prop),
            init_propagator(Start, Prop),
            trigger_prop(Prop)
        ;   true
        ),
        myserialized(D, Left, SDs, Start),
        serialize(SDs, [Start-D|Left]).

% consistency check / propagation
% Currently implements 2-b-consistency

myserialized(Duration, Left, Right, Start) :-
        myserialized(Left, Start, Duration),
        myserialized(Right, Start, Duration).

earliest_start_time(Start, EST) :-
        (   fd_get(Start, D, _) ->
            domain_infimum(D, EST)
        ;   EST = n(Start)
        ).

latest_start_time(Start, LST) :-
        (   fd_get(Start, D, _) ->
            domain_supremum(D, LST)
        ;   LST = n(Start)
        ).

myserialized([], _, _).
myserialized([S_I-D_I|SDs], S_J, D_J) :-
        (   var(S_I) ->
            serialize_lower_bound(S_I, D_I, Start, D_J),
            (   var(S_I) -> serialize_upper_bound(S_I, D_I, Start, D_J)
            ;   true
            )
        ;   var(S_J) ->
            serialize_lower_bound(S_J, D_J, S, D_I),
            (   var(S_J) -> serialize_upper_bound(S_J, D_J, S, D_I)
            ;   true
            )
        ;   D_I = n(D_II), D_J = n(D_JJ),
            (   S_I + D_II =< S_J -> true
            ;   S_J + D_JJ =< S_I -> true
            ;   fail
            )
        ),
        myserialized(SDs, S_J, D_J).

serialize_lower_bound(I, D_I, J, D_J) :-
        fd_get(I, DomI, Ps),
        domain_infimum(DomI, EST_I),
        latest_start_time(J, LST_J),
        (   Sum cis EST_I + D_I, Sum cis_gt LST_J ->
            earliest_start_time(J, EST_J),
            EST cis EST_J+D_J,
            domain_remove_smaller_than(DomI, EST, DomI1),
            fd_put(I, DomI1, Ps)
        ;   true
        ).

serialize_upper_bound(I, D_I, J, D_J) :-
        fd_get(I, DomI, Ps),
        domain_supremum(DomI, LST_I),
        earliest_start_time(J, EST_J),
        (   Sum cis EST_J + D_J, Sum cis_gt LST_I ->
            latest_start_time(J, LST_J),
            LST cis LST_J-D_I,
            domain_remove_greater_than(DomI, LST, DomI1),
            fd_put(I, DomI1, Ps)
        ;   true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Reflection predicates
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%% fd_var(+Var)
%
%  True iff Var is a CLP(FD) variable.

fd_var(X) :- get_attr(X, clpfd, _).

%% fd_inf(+Var, -Inf)
%
%  Inf is the infimum of the current domain of Var.

fd_inf(X, Inf) :-
        (   fd_get(X, XD, _) ->
            domain_infimum(XD, Inf0),
            bound_portray(Inf0, Inf)
        ;   must_be(integer, X),
            Inf = X
        ).

%% fd_sup(+Var, -Sup)
%
%  Sup is the supremum of the current domain of Var.

fd_sup(X, Sup) :-
        (   fd_get(X, XD, _) ->
            domain_supremum(XD, Sup0),
            bound_portray(Sup0, Sup)
        ;   must_be(integer, X),
            Sup = X
        ).

%% fd_size(+Var, -Size)
%
%  Size is the number of elements of the current domain of Var, or the
%  atom *sup* if the domain is unbounded.

fd_size(X, S) :-
        (   fd_get(X, XD, _) ->
            domain_num_elements(XD, S0),
            bound_portray(S0, S)
        ;   must_be(integer, X),
            S = 1
        ).

%% fd_dom(+Var, -Dom)
%
%  Dom is the current domain (see in/2) of Var.

fd_dom(X, Drep) :-
        (   fd_get(X, XD, _) ->
            domain_to_drep(XD, Drep)
        ;   must_be(integer, X),
            Drep = X..X
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Hooks
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attr_unify_hook(clpfd(_,_,_,Dom,Ps), Other) :-
        (   nonvar(Other) ->
            (   integer(Other) -> true
            ;   type_error(integer, Other)
            ),
            domain_contains(Dom, Other),
            trigger_props(Ps),
            do_queue
        ;   fd_get(Other, OD, OPs),
            domains_intersection(OD, Dom, Dom1),
            append(Ps, OPs, Ps1),
            fd_put(Other, Dom1, Ps1),
            trigger_props(Ps1),
            do_queue
        ).

bound_portray(inf, inf).
bound_portray(sup, sup).
bound_portray(n(N), N).

domain_to_drep(Dom, Drep) :-
        domain_intervals(Dom, [A0-B0|Rest]),
        bound_portray(A0, A),
        bound_portray(B0, B),
        (   A == B -> Drep0 = A
        ;   Drep0 = A..B
        ),
        intervals_to_drep(Rest, Drep0, Drep).

intervals_to_drep([], Drep, Drep).
intervals_to_drep([A0-B0|Rest], Drep0, Drep) :-
        bound_portray(A0, A),
        bound_portray(B0, B),
        (   A == B -> D1 = A
        ;   D1 = A..B
        ),
        intervals_to_drep(Rest, Drep0 \/ D1, Drep).

attribute_goal(X, Goal) :-
        phrase(attribute_goals(X), Goals),
        list_dot(Goals, Goal).

attribute_goals(X) -->
        { get_attr(X, clpfd, clpfd(_,_,_,Dom,Ps)), domain_to_drep(Dom, Drep) },
        [clpfd:(X in Drep)],
        attributes_goals(Ps).

list_dot([A], A)        :- !.
list_dot([A|As], (A,G)) :- list_dot(As, G).

attributes_goals([]) --> [].
attributes_goals([propagator(P, State)|As]) -->
        (   { arg(1, State, dead) } -> []
        ;   { arg(1, State, processed) } -> []
        ;   { ( functor(P, pdifferent, _) ; functor(P, pdistinct, _) ),
              arg(4, P, mutable(processed,_)) } -> []
        ;   { attribute_goal_(P, G) } ->
            { setarg(1, State, processed) },
            [clpfd:G]
        ;   [] % { format("currently no conversion for ~w\n", [P]) }
        ),
        attributes_goals(As).

attribute_goal_(pgeq(A,B), A #>= B).
attribute_goal_(pplus(X,Y,Z), X + Y #= Z).
attribute_goal_(pneq(A,B), A #\= B).
attribute_goal_(ptimes(X,Y,Z), X*Y #= Z).
attribute_goal_(absdiff_neq(X,Y,C), abs(X-Y) #\= C).
attribute_goal_(x_neq_y_plus_c(X,Y,C), X #\= Y + C).
attribute_goal_(x_leq_y_plus_c(X,Y,C), X #=< Y + C).
attribute_goal_(pdiv(X,Y,Z), X/Y #= Z).
attribute_goal_(pexp(X,Y,Z), X^Y #= Z).
attribute_goal_(pabs(X,Y), Y #= abs(X)).
attribute_goal_(pmod(X,M,K), X mod M #= K).
attribute_goal_(pmax(X,Y,Z), Z #= max(X,Y)).
attribute_goal_(pmin(X,Y,Z), Z #= min(X,Y)).
attribute_goal_(scalar_product(Cs,Vs,Op,C), Goal) :-
        Cs = [FC|Cs1], Vs = [FV|Vs1],
        coeff_var_term(FC, FV, T0),
        unfold_product(Cs1, Vs1, T0, Left),
        Goal =.. [Op,Left,C].
attribute_goal_(pdifferent(Left, Right, X, Shared), all_different(Vs)) :-
        append(Left, [X|Right], Vs0),
        msort(Vs0, Vs),
        setarg(1, Shared, processed).
attribute_goal_(pdistinct(Left, Right, X, Shared), all_distinct(Vs)) :-
        append(Left, [X|Right], Vs0),
        msort(Vs0, Vs),
        setarg(1, Shared, processed).
attribute_goal_(pserialized(Var,D,Left,Right), serialized(Vs, Ds)) :-
        append(Left, [Var-D|Right], VDs),
        pair_up(Vs, Ds, VDs).
attribute_goal_(rel_tuple(mutable(Rel,_), Tuple), tuples_in([Tuple], Rel)).
% reified constraints
attribute_goal_(reified_neq(DX, X, DY, Y, B), (DX #/\ DY #/\ X #\= Y) #<==> B).
attribute_goal_(reified_eq(DX, X, DY, Y, B), (DX #/\ DY #/\ X #= Y) #<==> B).
attribute_goal_(reified_geq(DX, X, DY, Y, B), (DX #/\ DY #/\ X #>= Y) #<==> B).
attribute_goal_(reified_div(X, Y, D, Z), (D #= 1 #==> X / Y #= Z, Y #\= 0 #==> D #= 1)).
attribute_goal_(reified_mod(X, Y, D, Z), (D #= 1 #==> X mod Y #= Z, Y #\= 0 #==> D #= 1)).
attribute_goal_(por(X,Y,Z), X #\/ Y #<==> Z).
attribute_goal_(reified_and(X, Y, B), X #/\ Y #<==> B).
attribute_goal_(reified_or(X, Y, B), X #\/ Y #<==> B).
attribute_goal_(reified_not(X, Y), #\ X #<==> Y).
attribute_goal_(pimpl(X, Y), X #==> Y).

coeff_var_term(C, V, T) :- ( C =:= 1 -> T = V ; T = C*V ).

unfold_product([], [], P, P).
unfold_product([C|Cs], [V|Vs], P0, P) :-
        coeff_var_term(C, V, T),
        unfold_product(Cs, Vs, P0 + T, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
domain_to_list(Domain, List) :- phrase(domain_to_list(Domain), List).

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

