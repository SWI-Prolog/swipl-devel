/*  Part of SWI-Prolog

    Author:        Tom Schrijvers, Markus Triska and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2022, K.U.Leuven
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
:- autoload(library(lists),[append/3,reverse/2]).


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
unequal related to an OrNode succeeds we   decrement  the `Count` of the
node. If the count  reaches  0  all   unifications  of  the  OrNode have
succeeded, the original terms are equal and thus we need to fail.

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
        ;   dif_c_c_l(Unifier,OrNode, U),
            subunifier(U, OrNode)
        )
    ;   or_succeed(OrNode)
    ).

subunifier([], _).
subunifier([X=Y|T], OrNode) :-
    dif_c_c(X, Y, OrNode),
    subunifier(T, OrNode).


%!  dif_c_c_l(+Unifier, +OrNode)
%
%   Extend OrNode with new elements from the   unifier.  Note that it is
%   possible that a unification against the   same variable appears as a
%   result of how unifiable acts on  sharing subterms. This is prevented
%   by simplify_ornode/3.
%
%   @see test 14 in src/Tests/attvar/test_dif.pl.

dif_c_c_l(Unifier, OrNode, U) :-
    extend_ornode(OrNode, List, Tail),
    dif_c_c_l_aux(Unifier, OrNode, List0, Tail),
    (   simplify_ornode(List0, List, U)
    ->  true
    ;   List = List0,
        or_succeed(OrNode),
        U = []
    ).

extend_ornode(OrNode, List, Vars) :-
    (   get_attr(OrNode, dif, node(Vars))
    ->  true
    ;   Vars = []
    ),
    put_attr(OrNode,dif,node(List)).

dif_c_c_l_aux([],_,List,List).
dif_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
    List = [X=Y|Rest],
    add_ornode(X,Y,OrNode),
    dif_c_c_l_aux(Unifier,OrNode,Rest,Tail).

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

%!  simplify_ornode(+OrNode) is semidet.
%
%   Simplify the possible unifications left on the original dif/2 terms.
%   There are two reasons for simplification. First   of all, due to the
%   way unifiable works we may end up with variables in the unifier that
%   do not refer to the original terms,   but  to variables in subterms,
%   e.g. `[V1 = f(a, V2), V2 = b]`.   As a result of subsequent unifying
%   variables, the unifier may end up   having  multiple entries for the
%   same variable, possibly having different values, e.g.,  `[X = a, X =
%   b]`.  As  these  can  never  be  satified  both  we  have  prove  of
%   inequality.
%
%   Finally, we remove elements from the list that have become equal. If
%   the OrNode is empty, the original terms   are equal and thus we must
%   fail.

simplify_ornode(OrNode) :-
    (   get_attr(OrNode, dif, node(Pairs0))
    ->  simplify_ornode(Pairs0, Pairs, U),
        Pairs-U \== []-[],
        put_attr(OrNode, dif, node(Pairs)),
        subunifier(U, OrNode)
    ;   true
    ).

simplify_ornode(List0, List, U) :-
    sort(1, @=<, List0, Sorted),
    simplify_ornode_(Sorted, List, U).

simplify_ornode_([], List, U) =>
    List = [],
    U = [].
simplify_ornode_([V1=V2|T], List, U), V1 == V2 =>
    simplify_ornode_(T, List, U).
simplify_ornode_([V1=Val1,V2=Val2|T], List, U), var(V1), V1 == V2 =>
    (   ?=(Val1, Val2)
    ->  Val1 == Val2,
        simplify_ornode_([V1=Val2|T], List, U)
    ;   U = [Val1=Val2|UT],
        simplify_ornode_([V2=Val2|T], List, UT)
    ).
simplify_ornode_([H|T], List, U) =>
    List = [H|Rest],
    simplify_ornode_(T, Rest, U).


%!  attr_unify_hook(+VarDif, +Other)
%
%   If two dif/2 variables are unified  we   must  join the two vardif/2
%   terms. To do so, we filter the vardif terms for the ones involved in
%   this unification. Those that  are  represent   OrNodes  that  have a
%   unification satisfied. For the rest we  remove the unifications with
%   _self_, append them and use this as new vardif term.
%
%   On unification with a value, we recursively call dif_c_c/3 using the
%   existing OrNodes.

attr_unify_hook(vardif(V1,V2),Other) :-
    (   get_attr(Other, dif, vardif(OV1,OV2))
    ->  reverse_lookups(V1, Other, OrNodes1, NV1),
        or_one_fails(OrNodes1),
        reverse_lookups(OV1, Other, OrNodes2, NOV1),
        or_one_fails(OrNodes2),
        remove_obsolete(V2, Other, NV2),
        remove_obsolete(OV2, Other, NOV2),
        append(NV1, NOV1, CV1),
        append(NV2, NOV2, CV2),
        (   CV1 == [], CV2 == []
        ->  del_attr(Other, dif)
        ;   put_attr(Other, dif, vardif(CV1,CV2))
        )
    ;   var(Other)			% unrelated variable
    ->  put_attr(Other, dif, vardif(V1,V2))
    ;   verify_compounds(V1, Other),
        verify_compounds(V2, Other)
    ).

remove_obsolete([], _, []).
remove_obsolete([N-Y|T], X, L) :-
    (   Y==X
    ->  remove_obsolete(T, X, L)
    ;   L=[N-Y|RT],
        remove_obsolete(T, X, RT)
    ).

reverse_lookups([],_,[],[]).
reverse_lookups([N-X|NXs],Value,Nodes,Rest) :-
    (   X == Value
    ->  Nodes = [N|RNodes],
        Rest = RRest
    ;   Nodes = RNodes,
        Rest = [N-X|RRest]
    ),
    reverse_lookups(NXs,Value,RNodes,RRest).

verify_compounds([],_).
verify_compounds([OrNode-Y|Rest],X) :-
    (   var(Y)
    ->  true
    ;   OrNode == (-)
    ->  true
    ;   dif_c_c(X,Y,OrNode)
    ),
    verify_compounds(Rest,X).

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
%   Some unification related to OrNode succeeded.   We can decrement the
%   `Count` of the OrNode. If this  reaches   0,  the original terms are
%   equal and we must fail.

or_one_fail(OrNode) :-
    simplify_ornode(OrNode).

or_one_fails([]).
or_one_fails([N|Ns]) :-
    or_one_fail(N),
    or_one_fails(Ns).


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
