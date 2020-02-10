/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(wfs,
          [ call_residual_program/2,            % :Goal, -Clauses

            call_delays/2,                      % :Goal, -Delays
            delays_residual_program/2,          % +Delays, -Clauses
            answer_residual/2,                  % :Goal, :Residual

            op(900, fy, tnot)
          ]).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(error),[instantiation_error/1,permission_error/3]).
:- autoload(library(lists),[list_to_set/2,member/2]).


/** <module> Well Founded Semantics interface

The library(wfs) provides  the  user  interface   to  the  Well  Founded
Semantics (WFS) support in SWI-Prolog.
*/

:- meta_predicate
    call_delays(0, :),
    delays_residual_program(:, :),
    call_residual_program(0, :),
    answer_residual(:, :).

%!  call_delays(:Goal, -Delays)
%
%   True when Goal is true with Delays.   Delays is `true` if the answer
%   is unconditionally true and a conjuctions   of tabled goals that are
%   _unknown_ according to the Well  Founded Semantics otherwise. Delays
%   only contains the unknown goals used for proving Goal. The predicate
%   call_delays/2  is  semantically  equivalent   to  call/1,  including
%   management of the delay list.

call_delays(Goal, Delays) :-
    '$wfs_call'(Goal, Delays).

%!  delays_residual_program(+Delays, -Clauses)
%
%   Given a delay as returned by call_delays/2, produce a set of clauses
%   the represents the complete residual   program responsible for these
%   delays, The program contains at least one loop through tnot/1 and is
%   either inconsistent or has multiple models   according to the stable
%   model semantics.

delays_residual_program(GM:Delays, M:Clauses) :-
    phrase(residual_program(Delays, GM, [], _), Program),
    maplist(unqualify_clause(M), Program, Clauses0),
    list_to_set(Clauses0, Clauses).

%!  call_residual_program(:Goal, -Clauses)
%
%   Call Goal and return the full residual program as a list of Clauses.

call_residual_program(Goal, M:Clauses) :-
    '$wfs_call'(Goal, 0:R0),                    % 0: leave qualified
    phrase(residual_program(R0, M, [], _), Program),
    maplist(unqualify_clause(M), Program, Clauses).


residual_program(Var, _, _, _) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
residual_program(M:G, _, Done0, Done) -->
    !,
    residual_program(G, M, Done0, Done).
residual_program(true, _, Done, Done) -->
    !.
residual_program(undefined, _, Done, Done) -->
    !.
residual_program(G, M, Done, Done) -->
    { member(M:G2, Done),
      G2 =@= G
    }, !.
residual_program((A;B), M, Done0, Done) -->
    !,
    residual_program(A, M, Done0, Done1),
    residual_program(B, M, Done1, Done).
residual_program((A,B), M, Done0, Done) -->
    !,
    residual_program(A, M, Done0, Done1),
    residual_program(B, M, Done1, Done).
residual_program(tnot(A), M, Done0, Done) -->
    !,
    residual_program(A, M, Done0, Done).
residual_program(Goal0, M, Done0, Done) -->
    { predicate_property(M:Goal0, imported_from(M2))
    },
    !,
    residual_program(Goal0, M2, Done0, Done).
residual_program(Goal, M, Done0, Done) -->
    { M:'$table_mode'(Goal, Variant, ModeArgs),
      (   current_table(M:Variant, Trie)
      ->  true
      ;   '$tabling':more_general_table(M:Variant, Trie)
      ),
      !,
      '$tbl_table_status'(Trie, _Status, M:Variant, Skeleton),
      copy_term(Skeleton, Skeleton2),
      (   (   '$tbl_is_trienode'(ModeArgs)
          ->  '$tbl_answer'(Trie, Skeleton2, Condition0)
          ;   '$tbl_answer'(Trie, Skeleton2, ModeArgs, Condition0)
          ),
          Skeleton2 =@= Skeleton
      ->  Skeleton2 = Skeleton
      ),
      as_cond(Condition0, Condition)
    },
    [ (M:Goal :- Condition) ],
    residual_program(Condition, M, [M:Goal|Done0], Done).
residual_program(Goal, M, Done, Done) -->
    { format(user_error, 'OOPS: Missing Call? ~p', [M:Goal])
    },
    [ (M:Goal :- ???) ].

as_cond((M:Variant)/ModeArgs, M:Goal) :-
    !,
    M:'$table_mode'(Goal, Variant, ModeArgs).
as_cond(Goal, Goal).

unqualify_clause(M, (Head0 :- Body0), (Head :- Body)) :-
    unqualify(Head0, M, Head),
    unqualify(Body0, M, Body).

%!  answer_residual(:Goal, :Residual)
%
%   True when Goal resolves to a tabled   predicate  and Residual is the
%   _residual_ goal associated with an answer   for Goal. Residual is in
%   its most general form a disjunction   (;/2) of conjunctions (,/2) of
%   tabled goals.

answer_residual(Goal, M:Residual) :-
    predicate_property(Goal, tabled(_)),
    !,
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Goal, Trie),
    '$tbl_table_status'(Trie, _Status, Goal, Skeleton),
    '$tbl_answer'(Trie, Skeleton, Condition),
    unqualify(Condition, M, Residual).
answer_residual(Goal, _) :-
    permission_error(answer_residual, non_tabled_procedure, Goal).

unqualify((A0;B0), M, G) :-
    !,
    G = (A;B),
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify((A0,B0), M, G) :-
    !,
    G = (A,B),
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify(tnot(A0), M, G) :-
    !,
    G = tnot(A),
    unqualify(A0, M, A).
unqualify(M:G0, MG, G) :-
    '$c_current_predicate'(_, MG:G0),
    predicate_property(MG:G0, imported_from(M)),
    !,
    G = G0.
unqualify(M:G0, M, G) :-
    !,
    G = G0.
unqualify(system:G0, _, G) :-
    !,
    G = G0.
unqualify(G, _, G).
