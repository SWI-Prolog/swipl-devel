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

:- module(tables,
          [ abolish_all_tables/0,
            abolish_table_pred/1,               % +PI
            tfindall/3,                         % +Template, :Goal, -Answers
            't not'/1,                          % :Goal

            get_returns_for_call/2,             % :CallTerm, ?AnswerTerm

            set_pil_on/0,
            set_pil_off/0
          ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(dialect/xsb)).

:- meta_predicate
    abolish_table_pred(:),
    tfindall(+, 0, -),
    't not'(0),
    get_returns_for_call(:, :).

%!  abolish_table_pred(:PI)
%
%

abolish_table_pred(M:Name/Arity) :-
    !,
    functor(Head, Name, Arity),
    abolish_table_subgoals(M:Head).
abolish_table_pred(PI) :-
    type_error(predicate_indicator, PI).

%!  't not'(:Goal)
%
%   Tabled negation.

't not'(Goal) :-
    tnot(Goal).

%!  tfindall(+Template, :Goal, -Answers)
%
%   More safe findall wrt. tabling.  For now just findall/3.

tfindall(Template, Goal, Answers) :-
    xsb_findall(Template, Goal, Answers).

%!  set_pil_on.
%!  set_pil_off.
%
%   Dummy predicates

set_pil_on.
set_pil_off.

%!  get_returns_for_call(:CallTerm, -AnswerTerm) is nondet.
%
%   True if AnswerTerm appears in the tables for the _variant_ CallTerm.

get_returns_for_call(M:CallTerm, AnswerTerm) :-
    current_table(M:CallTerm, Trie),
    '$tbl_table_status'(Trie, _Status, AnswerTerm, Skeleton),
    trie_gen(Trie, Skeleton, _).

