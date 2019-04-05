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
          [ (tnot)/1,                           % :Goal
            unknown/0,
            answer_residual/2,                  % :Goal, :Residual

            op(900, fy, tnot)
          ]).
:- use_module(library(error)).

/** <module> Well Founded Semantics interface

The library(wfs) provides  the  user  interface   to  the  Well  Founded
Semantics (WFS) support in SWI-Prolog.
*/

:- meta_predicate
    tnot(0),
    answer_residual(:, :).

:- table
    unknown/0.

%!  tnot(:Goal)
%
%   This predicate provides a tabled alternative to \+/1 (or not/1) that
%   provides well founded semantics.

% tnot/1 is defined in boot/tabling.pl

%!  unknown
%
%   Expresses the value _bottom_ from the well founded semantics.

unknown :-
    tnot(unknown).

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
unqualify(M:G0, M, G) :-
    !,
    G = G0.
unqualify(G, _, G).
