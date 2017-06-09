/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011, VU University Amsterdam
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

:- module('$predopts',
          [
          ]).

:- multifile
    predicate_options:option_decl/3,
    predicate_options:pred_option/3.
:- multifile                            % provided by library(predicate_options)
    system:predicate_option_type/2,
    system:predicate_option_mode/2.

:- public
    option_clauses//4.

%!  expand_predicate_options(:PI, +Arg, +OptionList, -Clauses) is det.
%
%   Term-expansion code for predicate_options(PI, Arg, OptionList).

expand_predicate_options(PI, Arg, Options,
                         [ predicate_options:option_decl(Head, M, Arg),
                           (:-multifile(M:'$pred_option'/4))
                         | OptionClauses
                         ]) :-
    canonical_pi(PI, CPI),
    prolog_load_context(module, M0),
    strip_module(M0:CPI, M, Name/Arity),
    functor(Head, Name, Arity),
    (   is_list(Options)
    ->  true
    ;   throw(error(type_error(list, Options), _))
    ),
    phrase(option_clauses(Options, Head, M, Arg), OptionClauses0),
    qualify_list(OptionClauses0, M0, OptionClauses).

qualify_list([], _, []).
qualify_list([H0|T0], M, [H|T]) :-
    qualify(H0, M, H),
    qualify_list(T0, M, T).

qualify(M:Term, M, Term) :- !.
qualify(QTerm, _, QTerm).


option_clauses([], _, _, _) --> [].
option_clauses([H|T], Head, M, A) -->
    option_clause(H, Head, M),
    option_clauses(T, Head, M, A).

option_clause(Var, _, _) -->
    { var(Var),
      !,
      throw(error(instantiation_error, _))
    }.
option_clause(pass_to(PI0, Arg), Head, M) -->
    !,
    { canonical_pi(PI0, PI),
      strip_module(M:PI, TM, Name/Arity),
      functor(THead, Name, Arity),
      Clause = ('$pred_option'(Head, pass_to(PI0, Arg), Opt, Seen) :-
                  \+ memberchk(PI-Arg, Seen),
                  predicate_options:pred_option(TM:THead, Opt, [PI-Arg|Seen]))
    },
    [ M:Clause ].
option_clause(Option, Head, M) -->
    { Option =.. [Name|ModeAndTypes],
      !,
      modes_and_types(ModeAndTypes, Args, Body),
      Opt =.. [Name|Args],
      Clause = ('$pred_option'(Head, Option, Opt, _) :- Body)
    },
    [ M:Clause ].
option_clause(Option, _, _) -->
    { throw(error(type_error(option_specifier, Option)))
    }.

modes_and_types([], [], true).
modes_and_types([H|T], [A|AT], Body) :-
    mode_and_type(H, A, Body0),
    (   T == []
    ->  Body = Body0,
        AT = []
    ;   Body0 == true
    ->  modes_and_types(T, AT, Body)
    ;   Body = (Body0,Body1),
        modes_and_types(T, AT, Body1)
    ).


mode_and_type(-Type, A, (predicate_option_mode(output, A), Body)) :-
    !,
    type_goal(Type, A, Body).
mode_and_type(+Type, A, Body) :-
    !,
    type_goal(Type, A, Body).
mode_and_type(Type, A, Body) :-
    type_goal(Type, A, Body).

type_goal(Type, A, predicate_option_type(Type, A)).


%!  canonical_pi(+PIIn, -PIout)

canonical_pi(M:Name//Arity, M:Name/PArity) :-
    integer(Arity),
    !,
    PArity is Arity+2.
canonical_pi(Name//Arity, Name/PArity) :-
    integer(Arity),
    !,
    PArity is Arity+2.
canonical_pi(PI, PI).


                 /*******************************
                 *             EXPAND           *
                 *******************************/

system:term_expansion((:- predicate_options(PI, Arg, Options)), Clauses) :-
    expand_predicate_options(PI, Arg, Options, Clauses).
