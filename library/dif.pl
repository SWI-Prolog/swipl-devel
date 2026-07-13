/*  Part of SWI-Prolog

    Author:        Tom Schrijvers, Markus Triska and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2023, K.U.Leuven
                              SWI-Prolog Solutions b.v.
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

:- module(dif,
          [ dif/2                               % +Term1, +Term2
          ]).
:- autoload(library(lists),[append/3, reverse/2]).


:- set_prolog_flag(generate_debug_info, false).

/** <module> The dif/2 constraint
*/

%!  dif(+Term1, +Term2) is semidet.
%
%   Constraint that expresses that  Term1   and  Term2  never become
%   identical (==/2). Fails if `Term1 ==   Term2`. Succeeds if Term1
%   can  never  become  identical  to  Term2.  In  other  cases  the
%   predicate succeeds after attaching constraints   to the relevant
%   parts of Term1 and Term2 that prevent   the  two terms to become
%   identical.

dif(X,Y) :-
    ?=(X,Y),
    !,
    X \== Y.
dif(X,Y) :-
    dif_c_c(X,Y,_).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The constraint is helt in  an   attribute  `dif`. A constrained variable
holds a term  vardif(L1,L2)  where  `L1`   is  a  list  OrNode-Value for
constraints on this variable  and  `L2`   is  the  constraint list other
variables have on me.

The `OrNode` is a term node(Pairs), where `Pairs` is a of list Var=Value
terms representing the pending unifications. The  original dif/2 call is
represented by a single OrNode.

If a unification related to an  OrNode   fails  the terms are definitely
unequal and thus we can kill all   pending constraints and succeed. If a
unequal related to an OrNode succeeds we remove it from the node. If the
node becomes empty the terms are equal and we must fail.

The following invariants must hold

  - Any variable involved in a dif/2 constraint has an attribute
    vardif(L1,L2), Where each element of both lists is a term
    OrNode-Value, L1 represents the values this variable may __not__
    become equal to and L2 represents this variable involved in other
    constraints.  I.e, L2 is only used if a dif/2 requires two variables
    to be different.
  - An OrNode has an attribute node(Pairs), where Pairs contains the
    possible unifications.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

dif_unifiable(X, Y, Us) :-
    (    current_prolog_flag(occurs_check, error)
    ->   catch(unifiable(X,Y,Us), error(occurs_check(_,_),_), false)
    ;    unifiable(X, Y, Us)
    ).

%!  dif_c_c(+X,+Y,!OrNode)
%
%   Enforce dif(X,Y) that is related to the given OrNode. If X and Y are
%   equal we reduce the OrNode.  If  they   cannot  unify  we  are done.
%   Otherwise we extend the OrNode with  new pairs and create/extend the
%   vardif/2 terms for the left hand side of  the unifier as well as the
%   right hand if this is a variable.

dif_c_c(X,Y,OrNode) :-
    (   dif_unifiable(X, Y, Unifier)
    ->  (   Unifier == []
        ->  or_one_fail(OrNode)
        ;   dif_c_c_l(Unifier, OrNode)
        )
    ;   or_succeed(OrNode)
    ).


%!  dif_c_c_l(+Unifier, +OrNode)
%
%   Combine the incoming unifier with the OrNode's current pending set,
%   then recompute the most-general unifier for the whole using
%   unifiable/3 over the accumulated left- and right-hand-side lists.
%   That gives canonical propagation and avoids the infinite oscillation
%   pair-wise simplification hits on cyclic terms.
%
%   Fails if the recomputed set is empty — that means all pending
%   equations are trivially satisfied and thus the original dif/2 terms
%   are equal, so dif/2 must fail. Calls or_succeed/1 when unifiable/3
%   itself fails: the pending equations can never all hold, so the two
%   terms are definitely unequal and dif/2 is satisfied.

dif_c_c_l(_Unifier, OrNode) :-
    nonvar(OrNode),                          % dead (or_succeed'd) node
    !.
dif_c_c_l(Unifier, OrNode) :-
    (   get_attr(OrNode, dif, node(OldPairs))
    ->  true
    ;   OldPairs = []
    ),
    append(Unifier, OldPairs, All),
    (   All == []
    ->  true                                 % nothing pending
    ;   eqs_lefts_rights(All, Xs, Ys),
        (   dif_unifiable(Xs, Ys, NewPairs)
        ->  NewPairs \== [],                 % [] ⇒ all satisfied ⇒ fail
            remove_ornode_from_pairs(OldPairs, OrNode),
            add_ornode_pairs(NewPairs, OrNode),
            put_attr(OrNode, dif, node(NewPairs))
        ;   or_succeed(OrNode)
        )
    ).

remove_ornode_from_pairs([], _).
remove_ornode_from_pairs([X=Y|T], OrNode) :-
    (   var(X) -> remove_ornode_v1(X, OrNode) ; true ),
    (   var(Y) -> remove_ornode_v2(Y, OrNode) ; true ),
    remove_ornode_from_pairs(T, OrNode).

remove_ornode_v1(X, OrNode) :-
    (   get_attr(X, dif, vardif(V1, V2))
    ->  filter_out_ornode(V1, OrNode, NV1),
        (   NV1 == [], V2 == []
        ->  del_attr(X, dif)
        ;   put_attr(X, dif, vardif(NV1, V2))
        )
    ;   true
    ).

remove_ornode_v2(Y, OrNode) :-
    (   get_attr(Y, dif, vardif(V1, V2))
    ->  filter_out_ornode(V2, OrNode, NV2),
        (   V1 == [], NV2 == []
        ->  del_attr(Y, dif)
        ;   put_attr(Y, dif, vardif(V1, NV2))
        )
    ;   true
    ).

filter_out_ornode([], _, []).
filter_out_ornode([N-Y|T], OrNode, L) :-
    (   N == OrNode
    ->  filter_out_ornode(T, OrNode, L)
    ;   L = [N-Y|LT],
        filter_out_ornode(T, OrNode, LT)
    ).

add_ornode_pairs([], _).
add_ornode_pairs([X=Y|T], OrNode) :-
    add_ornode(X, Y, OrNode),
    add_ornode_pairs(T, OrNode).

%!  add_ornode(+X, +Y, +OrNode)
%
%   Extend the vardif constraints on X and Y with the OrNode.

add_ornode(X,Y,OrNode) :-
    add_ornode_var1(X,Y,OrNode),
    (   var(Y)
    ->  add_ornode_var2(X,Y,OrNode)
    ;   true
    ).

add_ornode_var1(X,Y,OrNode) :-
    (   get_attr(X,dif,Attr)
    ->  Attr = vardif(V1,V2),
        put_attr(X,dif,vardif([OrNode-Y|V1],V2))
    ;   put_attr(X,dif,vardif([OrNode-Y],[]))
    ).

add_ornode_var2(X,Y,OrNode) :-
    (   get_attr(Y,dif,Attr)
    ->  Attr = vardif(V1,V2),
        put_attr(Y,dif,vardif(V1,[OrNode-X|V2]))
    ;   put_attr(Y,dif,vardif([],[OrNode-X]))
    ).

%!  attr_unify_hook(+VarDif, +Other)
%
%   Called after the attributed variable has been unified with Other.
%   Collects every OrNode this variable (and, for a var-var unification,
%   Other) is involved in and recomputes each one's MGU. The rebuild
%   inside dif_c_c_l/2 keeps the OrNode's pending list, this variable's
%   vardif and Other's vardif consistent — the current bindings show
%   through variable dereferencing when eqs_lefts_rights/3 walks the
%   pending list.

attr_unify_hook(vardif(V1, V2), Other) :-
    live_ornodes(V1, V2, MyOrNodes),
    (   var(Other),
        get_attr(Other, dif, vardif(OV1, OV2))
    ->  live_ornodes(OV1, OV2, TheirOrNodes),
        append(MyOrNodes, TheirOrNodes, OrNodes0),
        sort(OrNodes0, OrNodes)              % dedup by identity
    ;   OrNodes = MyOrNodes
    ),
    recompute_ornodes(OrNodes).

live_ornodes(V1, V2, OrNodes) :-
    live_ornodes_(V1, L1, T1),
    live_ornodes_(V2, T1, []),
    OrNodes = L1.

live_ornodes_([], T, T).
live_ornodes_([O-_|R], L, T) :-
    (   var(O)
    ->  L = [O|L1]
    ;   L = L1
    ),
    live_ornodes_(R, L1, T).

recompute_ornodes([]).
recompute_ornodes([O|T]) :-
    or_one_fail(O),
    recompute_ornodes(T).

%!  or_succeed(+OrNode) is det.
%
%   The dif/2 constraint related  to  OrNode   is  complete,  i.e., some
%   (sub)terms can definitely not become equal.   Next,  we can clean up
%   the constraints. We do so by setting   the  OrNode to `-` and remove
%   this _dead_ OrNode from every vardif/2 attribute we can find.

or_succeed(OrNode) :-
    (   get_attr(OrNode,dif,Attr)
    ->  Attr = node(Pairs),
        del_attr(OrNode,dif),
        OrNode = (-),
        del_or_dif(Pairs)
    ;   true
    ).

del_or_dif([]).
del_or_dif([X=Y|Xs]) :-
    cleanup_dead_nodes(X),
    cleanup_dead_nodes(Y),              % JW: what about embedded variables?
    del_or_dif(Xs).

cleanup_dead_nodes(X) :-
    (   get_attr(X,dif,Attr)
    ->  Attr = vardif(V1,V2),
        filter_dead_ors(V1,NV1),
        filter_dead_ors(V2,NV2),
        (   NV1 == [], NV2 == []
        ->  del_attr(X,dif)
        ;   put_attr(X,dif,vardif(NV1,NV2))
        )
    ;   true
    ).

filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
    (   var(Or)
    ->  List = [Or-Y|NRest]
    ;   List = NRest
    ),
    filter_dead_ors(Rest,NRest).


%!  or_one_fail(+OrNode) is semidet.
%
%   Recompute the MGU for OrNode's pending set without adding any new
%   equations. Fails when the set becomes empty (dif/2 fails).

or_one_fail(OrNode) :-
    dif_c_c_l([], OrNode).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is vardif/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is node/2. The second argument of node/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
    (   { get_attr(Var, dif, vardif(Ors,_)) }
    ->  or_nodes(Ors, Var)
    ;   or_node(Var)
    ).

or_node(O) -->
    (   { get_attr(O, dif, node(Pairs)) }
    ->  { eqs_lefts_rights(Pairs, As, Bs) },
        mydif(As, Bs),
        { del_attr(O, dif) }
    ;   []
    ).

or_nodes([], _)       --> [].
or_nodes([O-_|Os], X) -->
    (   { get_attr(O, dif, node(Eqs)) }
    ->  (   { Eqs = [LHS=_|_], LHS == X }
        ->  { eqs_lefts_rights(Eqs, As, Bs) },
            mydif(As, Bs),
            { del_attr(O, dif) }
        ;   []
        )
    ;   [] % or-node already removed
    ),
    or_nodes(Os, X).

mydif([X], [Y]) --> !, dif_if_necessary(X, Y).
mydif(Xs0, Ys0) -->
    { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
      X =.. [f|Xs], Y =.. [f|Ys]
    },
    dif_if_necessary(X, Y).

dif_if_necessary(X, Y) -->
    (   { dif_unifiable(X, Y, _) }
    ->  [dif(X,Y)]
    ;   []
    ).

eqs_lefts_rights([], [], []).
eqs_lefts_rights([A=B|ABs], [A|As], [B|Bs]) :-
    eqs_lefts_rights(ABs, As, Bs).
