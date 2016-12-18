/*  Part of SWI-Prolog

    Author:        Tom Schrijvers, Markus Triska and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2016, K.U.Leuven
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
:- use_module(library(lists)).
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
    X \== Y,
    dif_c_c(X,Y,_).

dif_unifiable(X, Y, Us) :-
    (    current_prolog_flag(occurs_check, error) ->
         catch(unifiable(X,Y,Us), error(occurs_check(_,_),_), false)
    ;    unifiable(X, Y, Us)
    ).

dif_c_c(X,Y,OrNode) :-
    (       dif_unifiable(X, Y, Unifier) ->
            ( Unifier == [] ->
                    or_one_fail(OrNode)
            ;
                    dif_c_c_l(Unifier,OrNode)
            )
    ;
            or_succeed(OrNode)
    ).


dif_c_c_l(Unifier,OrNode) :-
    length(Unifier,N),
    extend_ornode(OrNode,N,List,Tail),
    dif_c_c_l_aux(Unifier,OrNode,List,Tail).

extend_ornode(OrNode,N,List,Vars) :-
    ( get_attr(OrNode,dif,Attr) ->
            Attr = node(M,Vars),
            O is N + M - 1
    ;
            O = N,
            Vars = []
    ),
    put_attr(OrNode,dif,node(O,List)).

dif_c_c_l_aux([],_,List,List).
dif_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
    List = [X=Y|Rest],
    add_ornode(X,Y,OrNode),
    dif_c_c_l_aux(Unifier,OrNode,Rest,Tail).

add_ornode(X,Y,OrNode) :-
    add_ornode_var1(X,Y,OrNode),
    ( var(Y) ->
            add_ornode_var2(X,Y,OrNode)
    ;
            true
    ).

add_ornode_var1(X,Y,OrNode) :-
    ( get_attr(X,dif,Attr) ->
            Attr = vardif(V1,V2),
            put_attr(X,dif,vardif([OrNode-Y|V1],V2))
    ;
            put_attr(X,dif,vardif([OrNode-Y],[]))
    ).

add_ornode_var2(X,Y,OrNode) :-
    ( get_attr(Y,dif,Attr) ->
            Attr = vardif(V1,V2),
            put_attr(Y,dif,vardif(V1,[OrNode-X|V2]))
    ;
            put_attr(Y,dif,vardif([],[OrNode-X]))
    ).

attr_unify_hook(vardif(V1,V2),Other) :-
    ( var(Other) ->
            reverse_lookups(V1,Other,OrNodes1,NV1),
            or_one_fails(OrNodes1),
            get_attr(Other,dif,OAttr),
            OAttr = vardif(OV1,OV2),
            reverse_lookups(OV1,Other,OrNodes2,NOV1),
            or_one_fails(OrNodes2),
            remove_obsolete(V2,Other,NV2),
            remove_obsolete(OV2,Other,NOV2),
            append(NV1,NOV1,CV1),
            append(NV2,NOV2,CV2),
            ( CV1 == [], CV2 == [] ->
                    del_attr(Other,dif)
            ;
                    put_attr(Other,dif,vardif(CV1,CV2))
            )
    ;
            verify_compounds(V1,Other),
            verify_compounds(V2,Other)
    ).

remove_obsolete([], _, []).
remove_obsolete([N-Y|T], X, L) :-
    (   Y==X ->
        remove_obsolete(T, X, L)
    ;   L=[N-Y|RT],
        remove_obsolete(T, X, RT)
    ).

reverse_lookups([],_,[],[]).
reverse_lookups([N-X|NXs],Value,Nodes,Rest) :-
    ( X == Value ->
            Nodes = [N|RNodes],
            Rest = RRest
    ;
            Nodes = RNodes,
            Rest = [N-X|RRest]
    ),
    reverse_lookups(NXs,Value,RNodes,RRest).

verify_compounds([],_).
verify_compounds([OrNode-Y|Rest],X) :-
    ( var(Y) ->
            true
    ; OrNode == (-) ->
            true
    ;
            dif_c_c(X,Y,OrNode)
    ),
    verify_compounds(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
or_succeed(OrNode) :-
    ( attvar(OrNode) ->
            get_attr(OrNode,dif,Attr),
            Attr = node(_Counter,Pairs),
            del_attr(OrNode,dif),
            OrNode = (-),
            del_or_dif(Pairs)
    ;
            true
    ).

or_one_fails([]).
or_one_fails([N|Ns]) :-
    or_one_fail(N),
    or_one_fails(Ns).

or_one_fail(OrNode) :-
    ( attvar(OrNode) ->
            get_attr(OrNode,dif,Attr),
            Attr = node(Counter,Pairs),
            NCounter is Counter - 1,
            ( NCounter == 0 ->
                    fail
            ;
                    put_attr(OrNode,dif,node(NCounter,Pairs))
            )
    ;
            fail
    ).

del_or_dif([]).
del_or_dif([X=Y|Xs]) :-
    cleanup_dead_nodes(X),
    cleanup_dead_nodes(Y),
    del_or_dif(Xs).

cleanup_dead_nodes(X) :-
    ( attvar(X) ->
            get_attr(X,dif,Attr),
            Attr = vardif(V1,V2),
            filter_dead_ors(V1,NV1),
            filter_dead_ors(V2,NV2),
            ( NV1 == [], NV2 == [] ->
                    del_attr(X,dif)
            ;
                    put_attr(X,dif,vardif(NV1,NV2))
            )
    ;
            true
    ).

filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
    ( var(Or) ->
            List = [Or-Y|NRest]
    ;
            List = NRest
    ),
    filter_dead_ors(Rest,NRest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is vardif/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is node/2. The second argument of node/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
    (   { get_attr(Var, dif, vardif(Ors,_)) } ->
        or_nodes(Ors, Var)
    ;   or_node(Var)
    ).

or_node(O) -->
    (   { get_attr(O, dif, node(_, Pairs)) } ->
        { eqs_lefts_rights(Pairs, As, Bs) },
        mydif(As, Bs),
        { del_attr(O, dif) }
    ;   []
    ).

or_nodes([], _)       --> [].
or_nodes([O-_|Os], X) -->
    (   { get_attr(O, dif, node(_, Eqs)) } ->
        (   { Eqs = [LHS=_|_], LHS == X } ->
            { eqs_lefts_rights(Eqs, As, Bs) },
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
      X =.. [f|Xs], Y =.. [f|Ys] },
    dif_if_necessary(X, Y).

dif_if_necessary(X, Y) -->
    (   { dif_unifiable(X, Y, _) } ->
        [dif(X,Y)]
    ;   []
    ).

eqs_lefts_rights([], [], []).
eqs_lefts_rights([A=B|ABs], [A|As], [B|Bs]) :-
    eqs_lefts_rights(ABs, As, Bs).
