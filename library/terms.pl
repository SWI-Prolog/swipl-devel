/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c) 2008-2022, University of Amsterdam,
                             VU University
                             SWI-Prolog Solutions b.v.
    Amsterdam All rights reserved.

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

:- module(terms,
          [ term_hash/2,                % @Term, -HashKey
            term_hash/4,                % @Term, +Depth, +Range, -HashKey
            term_size/2,                % @Term, -Size
            term_variables/2,           % @Term, -Variables
            term_variables/3,           % @Term, -Variables, +Tail
            variant/2,                  % @Term1, @Term2
            subsumes/2,                 % +Generic, @Specific
            subsumes_chk/2,             % +Generic, @Specific
            cyclic_term/1,              % @Term
            acyclic_term/1,             % @Term
            term_subsumer/3,            % +Special1, +Special2, -General
            term_factorized/3,          % +Term, -Skeleton, -Subsitution
            mapargs/3,                  % :Goal, ?Term1, ?Term2
            mapsubterms/3,              % :Goal, ?Term1, ?Term2
            mapsubterms_var/3,          % :Goal, ?Term1, ?Term2
            foldsubterms/4,             % :Goal, +Term, +State0, -State
            foldsubterms/5,             % :Goal, +Term1, ?Term2, +State0, -State
            same_functor/2,             % ?Term1, ?Term2
            same_functor/3,             % ?Term1, ?Term2, -Arity
            same_functor/4              % ?Term1, ?Term2, ?Name, ?Arity
          ]).

:- meta_predicate
    mapargs(2,?,?),
    mapsubterms(2,?,?),
    mapsubterms_var(2,?,?),
    foldsubterms(3,+,+,-),
    foldsubterms(4,+,?,+,-).

:- autoload(library(rbtrees),
	    [ rb_empty/1,
	      rb_lookup/3,
	      rb_insert/4,
	      rb_new/1,
	      rb_visit/2,
	      ord_list_to_rbtree/2,
	      rb_update/5
	    ]).
:- autoload(library(error), [instantiation_error/1]).


/** <module> Term manipulation

Compatibility library for term manipulation  predicates. Most predicates
in this library are provided as SWI-Prolog built-ins.

@compat YAP, SICStus, Quintus.  Not all versions of this library define
        exactly the same set of predicates, but defined predicates are
        compatible.
*/

%!  term_size(@Term, -Size) is det.
%
%   True if Size is the size  in   _cells_  occupied  by Term on the
%   global (term) stack. A _cell_ is 4  bytes on 32-bit machines and
%   8 bytes on 64-bit machines. The  calculation does take _sharing_
%   into account. For example:
%
%   ```
%   ?- A = a(1,2,3), term_size(A,S).
%   S = 4.
%   ?- A = a(1,2,3), term_size(a(A,A),S).
%   S = 7.
%   ?- term_size(a(a(1,2,3), a(1,2,3)), S).
%   S = 11.
%   ```
%
%   Note that small objects such as atoms  and small integers have a
%   size 0. Space is allocated for   floats, large integers, strings
%   and compound terms.

term_size(Term, Size) :-
    '$term_size'(Term, _, Size).

%!  variant(@Term1, @Term2) is semidet.
%
%   Same as SWI-Prolog =|Term1 =@= Term2|=.

variant(X, Y) :-
    X =@= Y.

%!  subsumes_chk(@Generic, @Specific)
%
%   True if Generic can be made equivalent to Specific without
%   changing Specific.
%
%   @deprecated Replace by subsumes_term/2.

subsumes_chk(Generic, Specific) :-
    subsumes_term(Generic, Specific).

%!  subsumes(+Generic, @Specific)
%
%   True  if  Generic  is  unified   to  Specific  without  changing
%   Specific.
%
%   @deprecated It turns out that calls to this predicate almost
%   always should have used subsumes_term/2.  Also the name is
%   misleading.  In case this is really needed, one is adviced to
%   follow subsumes_term/2 with an explicit unification.

subsumes(Generic, Specific) :-
    subsumes_term(Generic, Specific),
    Generic = Specific.

%!  term_subsumer(+Special1, +Special2, -General) is det.
%
%   General is the most specific term   that  is a generalisation of
%   Special1 and Special2. The  implementation   can  handle  cyclic
%   terms.
%
%   @compat SICStus
%   @author Inspired by LOGIC.PRO by Stephen Muggleton

%       It has been rewritten by  Jan   Wielemaker  to use the YAP-based
%       red-black-trees as mapping rather than flat  lists and use arg/3
%       to map compound terms rather than univ and lists.

term_subsumer(S1, S2, G) :-
    cyclic_term(S1),
    cyclic_term(S2),
    !,
    rb_empty(Map),
    lgg_safe(S1, S2, G, Map, _).
term_subsumer(S1, S2, G) :-
    rb_empty(Map),
    lgg(S1, S2, G, Map, _).

lgg(S1, S2, G, Map0, Map) :-
    (   S1 == S2
    ->  G = S1,
        Map = Map0
    ;   compound(S1),
        compound(S2),
        functor(S1, Name, Arity),
        functor(S2, Name, Arity)
    ->  functor(G, Name, Arity),
        lgg(0, Arity, S1, S2, G, Map0, Map)
    ;   rb_lookup(S1+S2, G0, Map0)
    ->  G = G0,
        Map = Map0
    ;   rb_insert(Map0, S1+S2, G, Map)
    ).

lgg(Arity, Arity, _, _, _, Map, Map) :- !.
lgg(I0, Arity, S1, S2, G, Map0, Map) :-
    I is I0 + 1,
    arg(I, S1, Sa1),
    arg(I, S2, Sa2),
    arg(I, G, Ga),
    lgg(Sa1, Sa2, Ga, Map0, Map1),
    lgg(I, Arity, S1, S2, G, Map1, Map).


%!  lgg_safe(+S1, +S2, -G, +Map0, -Map) is det.
%
%   Cycle-safe version of the  above.  The   difference  is  that we
%   insert compounds into the mapping table   and  check the mapping
%   table before going into a compound.

lgg_safe(S1, S2, G, Map0, Map) :-
    (   S1 == S2
    ->  G = S1,
        Map = Map0
    ;   rb_lookup(S1+S2, G0, Map0)
    ->  G = G0,
        Map = Map0
    ;   compound(S1),
        compound(S2),
        functor(S1, Name, Arity),
        functor(S2, Name, Arity)
    ->  functor(G, Name, Arity),
        rb_insert(Map0, S1+S2, G, Map1),
        lgg_safe(0, Arity, S1, S2, G, Map1, Map)
    ;   rb_insert(Map0, S1+S2, G, Map)
    ).

lgg_safe(Arity, Arity, _, _, _, Map, Map) :- !.
lgg_safe(I0, Arity, S1, S2, G, Map0, Map) :-
    I is I0 + 1,
    arg(I, S1, Sa1),
    arg(I, S2, Sa2),
    arg(I, G, Ga),
    lgg_safe(Sa1, Sa2, Ga, Map0, Map1),
    lgg_safe(I, Arity, S1, S2, G, Map1, Map).


%!  term_factorized(+Term, -Skeleton, -Substiution)
%
%   Is true when Skeleton is  Term   where  all subterms that appear
%   multiple times are replaced by a  variable and Substitution is a
%   list of Var=Value that provides the subterm at the location Var.
%   I.e., After unifying all substitutions  in Substiutions, Term ==
%   Skeleton. Term may be cyclic. For example:
%
%     ==
%     ?- X = a(X), term_factorized(b(X,X), Y, S).
%     Y = b(_G255, _G255),
%     S = [_G255=a(_G255)].
%     ==

term_factorized(Term, Skeleton, Substitutions) :-
    rb_new(Map0),
    add_map(Term, Map0, Map),
    rb_visit(Map, Counts),
    common_terms(Counts, Common),
    (   Common == []
    ->  Skeleton = Term,
        Substitutions = []
    ;   ord_list_to_rbtree(Common, SubstAssoc),
        insert_vars(Term, Skeleton, SubstAssoc),
        mk_subst(Common, Substitutions, SubstAssoc)
    ).

add_map(Term, Map0, Map) :-
    (   primitive(Term)
    ->  Map = Map0
    ;   rb_update(Map0, Term, Old, New, Map)
    ->  New is Old+1
    ;   rb_insert(Map0, Term, 1, Map1),
        assoc_arg_map(1, Term, Map1, Map)
    ).

assoc_arg_map(I, Term, Map0, Map) :-
    arg(I, Term, Arg),
    !,
    add_map(Arg, Map0, Map1),
    I2 is I + 1,
    assoc_arg_map(I2, Term, Map1, Map).
assoc_arg_map(_, _, Map, Map).

primitive(Term) :-
    var(Term),
    !.
primitive(Term) :-
    atomic(Term),
    !.
primitive('$VAR'(_)).

common_terms([], []).
common_terms([H-Count|T], List) :-
    !,
    (   Count == 1
    ->  common_terms(T, List)
    ;   List = [H-_NewVar|Tail],
        common_terms(T, Tail)
    ).

insert_vars(T0, T, _) :-
    primitive(T0),
    !,
    T = T0.
insert_vars(T0, T, Subst) :-
    rb_lookup(T0, S, Subst),
    !,
    T = S.
insert_vars(T0, T, Subst) :-
    functor(T0, Name, Arity),
    functor(T,  Name, Arity),
    insert_arg_vars(1, T0, T, Subst).

insert_arg_vars(I, T0, T, Subst) :-
    arg(I, T0, A0),
    !,
    arg(I, T,  A),
    insert_vars(A0, A, Subst),
    I2 is I + 1,
    insert_arg_vars(I2, T0, T, Subst).
insert_arg_vars(_, _, _, _).

mk_subst([], [], _).
mk_subst([Val0-Var|T0], [Var=Val|T], Subst) :-
    functor(Val0, Name, Arity),
    functor(Val,  Name, Arity),
    insert_arg_vars(1, Val0, Val, Subst),
    mk_subst(T0, T, Subst).


%!  mapargs(:Goal, ?Term1, ?Term2)
%
%   Term1 and Term2 have the  same   functor  (name/arity)  and for each
%   matching pair of arguments call(Goal, A1, A2) is true.

mapargs(Goal, Term1, Term2) :-
    same_functor(Term1, Term2, Arity),
    mapargs_(1, Arity, Goal, Term1, Term2).

mapargs_(I, Arity, Goal, Term1, Term2) :-
    I =< Arity,
    !,
    arg(I, Term1, A1),
    arg(I, Term2, A2),
    call(Goal, A1, A2),
    I2 is I+1,
    mapargs_(I2, Arity, Goal, Term1, Term2).
mapargs_(_, _, _, _, _).


%!  mapsubterms(:Goal, +Term1, -Term2) is det.
%!  mapsubterms_var(:Goal, +Term1, -Term2) is det.
%
%   Recursively map sub terms of Term1 into  subterms of Term2 for every
%   pair for which call(Goal,  ST1,   ST2)  succeeds.  Procedurably, the
%   mapping for each (sub) term pair `T1/T2` is defined as:
%
%     - If `T1` is a variable
%       - mapsubterms/3 unifies `T2` with `T1`.
%       - mapsubterms_var/3 treats variables as other terms.
%     - If call(Goal, T1, T2) succeeds we are done.  Note that the
%       mapping does not continue in `T2`.  If this is desired, `Goal`
%       must call mapsubterms/3 explicitly as part of its conversion.
%     - If `T1` is a dict, map all values, i.e., the _tag_ and _keys_
%       are left untouched.
%     - If `T1` is a list, map all elements, i.e., the list structure
%       is left untouched.
%     - If `T1` is a compound, use same_functor/3 to instantiate `T2`
%       and recurse over the term arguments left to right.
%     - Otherwise `T2` is unified with `T1`.
%
%   Both predicates are implemented using foldsubterms/5.

mapsubterms(Goal, Term1, Term2) :-
    foldsubterms(map2(Goal), Term1, Term2, _, _).
mapsubterms_var(Goal, Term1, Term2) :-
    foldsubterms(map2_var(Goal), Term1, Term2, _, _).

map2(Goal, Term1, Term2, _, _) :-
    nonvar(Term1),
    call(Goal, Term1, Term2).

map2_var(Goal, Term1, Term2, _, _) :-
    call(Goal, Term1, Term2).

%!  foldsubterms(:Goal3, +Term1, +State0, -State) is semidet.
%!  foldsubterms(:Goal4, +Term1, ?Term2, +State0, -State) is semidet.
%
%   The predicate foldsubterms/5 calls   call(Goal4, SubTerm1, SubTerm2,
%   StateIn, StateOut) for each subterm,  including variables, in Term1.
%   If this call fails, `StateIn`  and   `StateOut`  are  the same. This
%   predicate may be used to map  subterms   in  a term while collecting
%   state about the mapped subterms. The foldsubterms/4 variant does not
%   map the term.

foldsubterms(Goal, Term1, State0, State) :-
    foldsubterms(fold1(Goal), Term1, _, State0, State).

fold1(Goal, Term1, _Term2, State0, State) :-
    call(Goal, Term1, State0, State).

foldsubterms(Goal, Term1, Term2, State0, State) :-
    call(Goal, Term1, Term2, State0, State),
    !.
foldsubterms(Goal, Term1, Term2, State0, State) :-
    is_dict(Term1),
    !,
    dict_pairs(Term1, Tag, Pairs1),
    fold_dict_pairs(Pairs1, Pairs2, Goal, State0, State),
    dict_pairs(Term2, Tag, Pairs2).
foldsubterms(Goal, Term1, Term2, State0, State) :-
    is_list(Term1),
    !,
    fold_some(Term1, Term2, Goal, State0, State).
foldsubterms(Goal, Term1, Term2, State0, State) :-
    compound(Term1),
    !,
    same_functor(Term1, Term2, Arity),
    foldsubterms_(1, Arity, Goal, Term1, Term2, State0, State).
foldsubterms(_, Term, Term, State, State).

fold_dict_pairs([], [], _, State, State).
fold_dict_pairs([K-V0|T0], [K-V|T], Goal, State0, State) :-
    foldsubterms(Goal, V0, V, State0, State1),
    fold_dict_pairs(T0, T, Goal, State1, State).

fold_some([], [], _, State, State).
fold_some([H0|T0], [H|T], Goal, State0, State) :-
    foldsubterms(Goal, H0, H, State0, State1),
    fold_some(T0, T, Goal, State1, State).

foldsubterms_(I, Arity, Goal, Term1, Term2, State0, State) :-
    I =< Arity,
    !,
    arg(I, Term1, A1),
    arg(I, Term2, A2),
    foldsubterms(Goal, A1, A2, State0, State1),
    I2 is I+1,
    foldsubterms_(I2, Arity, Goal, Term1, Term2, State1, State).
foldsubterms_(_, _, _, _, _, State, State).


%!  same_functor(?Term1, ?Term2) is semidet.
%!  same_functor(?Term1, ?Term2, -Arity) is semidet.
%!  same_functor(?Term1, ?Term2, ?Name, ?Arity) is semidet.
%
%   True when Term1 and Term2  are  terms   that  have  the same functor
%   (Name/Arity). The arguments must be sufficiently instantiated, which
%   means either Term1 or Term2 must  be   bound  or both Name and Arity
%   must be bound.
%
%   If  Arity  is  0,  Term1  and  Term2   are  unified  with  Name  for
%   compatibility.
%
%   @compat SICStus

same_functor(Term1, Term2) :-
    same_functor(Term1, Term2, _Name, _Arity).

same_functor(Term1, Term2, Arity) :-
    same_functor(Term1, Term2, _Name, Arity).

same_functor(Term1, Term2, Name, Arity) :-
    (   nonvar(Term1)
    ->  functor(Term1, Name, Arity, Type),
        functor(Term2, Name, Arity, Type)
    ;   nonvar(Term2)
    ->  functor(Term2, Name, Arity, Type),
        functor(Term1, Name, Arity, Type)
    ;   functor(Term2, Name, Arity),
        functor(Term1, Name, Arity)
    ).

