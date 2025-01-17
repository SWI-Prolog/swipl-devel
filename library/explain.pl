/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam,
                              VU University Amsterdam
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

:- module(prolog_explain,
          [ explain/1,
            explain/2
          ]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(lists),[flatten/2]).
:- autoload(library(prolog_code), [pi_head/2]).
:- autoload(library(solution_sequences), [distinct/2]).

:- if(exists_source(library(pldoc/man_index))).
:- autoload(library(pldoc/man_index), [man_object_property/2]).
:- endif.

/** <module> Describe Prolog Terms

The   library(explain)   describes   prolog-terms.   The   most   useful
functionality is its cross-referencing function.

```
?- explain(subset(_,_)).
"subset(_, _)" is a compound term
    from 2-th clause of lists:subset/2
    Referenced from 46-th clause of prolog_xref:imported/3
    Referenced from 68-th clause of prolog_xref:imported/3
lists:subset/2 is a predicate defined in
    /staff/jan/lib/pl-5.6.17/library/lists.pl:307
    Referenced from 2-th clause of lists:subset/2
    Possibly referenced from 2-th clause of lists:subset/2
```

Note that PceEmacs can jump to definitions   and gxref/0 can be used for
an overview of dependencies.
*/

%!  explain(@Term) is det
%
%   Give an explanation on Term. Term  can   be  any Prolog data object.
%   Some terms have a specific meaning:
%
%     - A (partial) reference to a predicate gives the predicates,
%       its main properties and references to the predicates.  Partial
%       references are:
%       - Module:Name/Arity
%       - Module:Head
%       - Name/Arity
%       - Name//Arity
%       - Name
%       - Module:Name
%     - Some predicate properties.  This lists predicates as above
%       the have this property.  The specification can be of the
%       shape `Module:Property` or just `Property`.  The qualified
%       version limits the result to predicates defined in Module.
%       Supported properties are:
%       - dynamic
%       - thread_local
%       - multifile
%       - tabled

explain(Item) :-
    explain(Item, Explanation),
    print_message(information, explain(Explanation)),
    fail.
explain(_).

                /********************************
                *           BASIC TYPES         *
                *********************************/

%!  explain(@Term, -Explanation) is nondet.
%
%   True when Explanation is an explanation of Term. The explaination is
%   a list of elements that  is printed using print_message(information,
%   explain(Explanation)).

explain(Var, [isa(Var, 'unbound variable')]) :-
    var(Var),
    !.
explain(I, [isa(I, 'an integer')]) :-
    integer(I),
    !.
explain(F, [isa(F, 'a floating point number')]) :-
    float(F),
    !.
explain(Q, [isa(Q, 'a rational (Q) number'),T]) :-
    rational(Q),
    (   catch(F is float(Q), error(evaluation_error(_),_), fail)
    ->  T = ' with approximate floating point value ~w'-[F]
    ;   T = ' that can not be represented as a floating point number'
    ),
    !.
explain(S, [isa(S, 'a string of length ~D'-[Len])]) :-
    string(S),
    string_length(S, Len),
    !.
explain([], [isa([], 'a special constant denoting an empty list')]) :-
    !.
explain(A, [isa(A, 'an atom of length ~D'-[Len])]) :-
    atom(A),
    atom_length(A, Len).
explain(A, Explanation) :-
    atom(A),
    current_op(Pri, F, A),
    op_type(F, Type),
    Explanation = [ isa(A, 'a ~w (~w) operator of priority ~d'-[Type, F, Pri]) ].
explain(A, Explanation) :-
    atom(A),
    !,
    explain_atom(A, Explanation).
explain([H|T], Explanation) :-
    List = [H|T],
    is_list(T),
    !,
    length(List, L),
    (   Explanation = [ isa(List, 'a proper list with ~d elements'-[L]) ]
    ;   maplist(printable, List),
        Explanation = [ indent, 'Text is "~s"'-[List] ]
    ).
explain(List, Explanation) :-
    List = [_|_],
    !,
    length(List, L),
    !,
    Explanation = [isa(List, 'is a not-closed list with ~D elements'-[L])].
explain(Dict, Explanation) :-
    is_dict(Dict, Tag),
    !,
    dict_pairs(Dict, Tag, Pairs),
    length(Pairs, Count),
    Explanation = [isa(Dict, 'is a dict with tag ~p and ~D keys'-[Tag, Count])].
explain(Name//NTArity, Explanation) :-
    atom(Name),
    integer(NTArity),
    NTArity >= 0,
    !,
    Arity is NTArity + 2,
    explain(Name/Arity, Explanation).
explain(Name/Arity, Explanation) :-
    atom(Name),
    integer(Arity),
    Arity >= 0,
    !,
    functor(Head, Name, Arity),
    distinct(Module, known_predicate(Module:Head)),
    (   Module == system
    ->  true
    ;   \+ predicate_property(Module:Head, imported_from(_))
    ),
    explain_predicate(Module:Head, Explanation).
explain(Module:Name/Arity, Explanation) :-
    atom(Module), atom(Name), integer(Arity),
    !,
    functor(Head, Name, Arity),
    explain_predicate(Module:Head, Explanation).
explain(Module:Property, Explanation) :-
    atom(Property),
    explain_property(Property, Module, Explanation).
explain(Module:Head, Explanation) :-
    atom(Module), callable(Head),
    predicate_property(Module:Head, _),
    !,
    explain_predicate(Module:Head, Explanation).
explain(Term, Explanation) :-
    compound(Term),
    compound_name_arity(Term, _Name, Arity),
    numbervars(Term, 0, _, [singletons(true)]),
    Explanation = [isa(Term, 'is a compound term with arity ~D'-[Arity])].
explain(Term, Explanation) :-
    explain_functor(Term, Explanation).

%!  known_predicate(:Head)
%
%   Succeeds if we know anything about this predicate.  Undefined
%   predicates are considered `known' for this purpose, so we can
%   provide referenced messages on them.

known_predicate(M:Head) :-
    var(M),
    current_predicate(_, M2:Head),
    (   predicate_property(M2:Head, imported_from(M))
    ->  true
    ;   M = M2
    ).
known_predicate(Pred) :-
    predicate_property(Pred, undefined).
known_predicate(_:Head) :-
    functor(Head, Name, Arity),
    '$in_library'(Name, Arity, _Path).

op_type(X, prefix) :-
    atom_chars(X, [f, _]).
op_type(X, infix) :-
    atom_chars(X, [_, f, _]).
op_type(X, postfix) :-
    atom_chars(X, [_, f]).

printable(C) :-
    integer(C),
    code_type(C, graph).


                /********************************
                *             ATOMS             *
                *********************************/

explain_atom(A, Explanation) :-
    referenced(A, Explanation).
explain_atom(A, Explanation) :-
    current_predicate(A, Module:Head),
    (   Module == system
    ->  true
    ;   \+ predicate_property(Module:Head, imported_from(_))
    ),
    explain_predicate(Module:Head, Explanation).
explain_atom(A, Explanation) :-
    predicate_property(Module:Head, undefined),
    functor(Head, A, _),
    explain_predicate(Module:Head, Explanation).
explain_atom(A, Explanation) :-
    explain_property(A, _, Explanation).

%!  explain_property(+Property, ?Module, -Explanation) is nondet
%
%   Explain  predicates  that  have  some    property.  Only  does  user
%   predicates.

explain_property(Prop, M, Explanation) :-
    explainable_property(Prop),
    (   var(M)
    ->  freeze(M, module_property(M, class(user)))
    ;   true
    ),
    Pred = M:_,
    predicate_property(Pred, Prop),
    \+ predicate_property(Pred, imported_from(_)),
    \+ hide_reference(Pred),
    explain_predicate(Pred, Explanation).

explainable_property(dynamic).
explainable_property(thread_local).
explainable_property(multifile).
explainable_property(tabled).

                /********************************
                *            FUNCTOR             *
                *********************************/

explain_functor(Head, Explanation) :-
    referenced(Head, Explanation).
explain_functor(Head, Explanation) :-
    current_predicate(_, Module:Head),
    \+ predicate_property(Module:Head, imported_from(_)),
    explain_predicate(Module:Head, Explanation).
explain_functor(Head, Explanation) :-
    predicate_property(M:Head, undefined),
    (   functor(Head, N, A),
        Explanation = [ pi(M:N/A), 'is an undefined predicate' ]
    ;   referenced(M:Head, Explanation)
    ).


                /********************************
                *           PREDICATE           *
                *********************************/

lproperty(built_in,     [' built-in']).
lproperty(thread_local, [' thread-local']).
lproperty(dynamic,      [' dynamic']).
lproperty(multifile,    [' multifile']).
lproperty(transparent,  [' meta']).

tproperty(Pred, Explanation) :-
    (   predicate_property(Pred, number_of_clauses(Count))
    ->  Explanation = [' with ~D clauses '-[Count]]
    ;   predicate_property(Pred, thread_local)
    ->  thread_self(Me),
        Explanation = [' without clauses in thread ',
                       ansi(code, '~p', [Me]) ]
    ;   Explanation = [' without clauses']
    ).
tproperty(Pred, [' imported from module ', module(Module)]) :-
    predicate_property(Pred, imported(Module)).
tproperty(Pred, [' defined in ', url(File:Line)]) :-
    predicate_property(Pred, file(File)),
    predicate_property(Pred, line_count(Line)).
tproperty(Pred, [' that can be autoloaded']) :-
    predicate_property(Pred, autoload).

%!  explain_predicate(:Head, -Explanation) is det.

explain_predicate(Pred, Explanation) :-
    Pred = Module:Head,
    functor(Head, Name, Arity),
    (   predicate_property(Pred, non_terminal)
    ->  What = 'non-terminal'
    ;   What = 'predicate'
    ),
    (   predicate_property(Pred, undefined)
    ->  Explanation = [ pi(Module:Name/Arity),
                        ansi([bold,fg(default)], ' is an undefined ~w', [What])
                      ]
    ;   (   var(Module)
        ->  U0 = [ pi(Name/Arity),
                   ansi([bold,fg(default)], ' is a', [])
                 ]
        ;   U0 = [ pi(Module:Name/Arity),
                   ansi([bold,fg(default)], ' is a', [])
                 ]
        ),
        findall(Utter, (lproperty(Prop, Utter),
                        predicate_property(Pred, Prop)),
                U1),
        U2 = [ansi([bold,fg(default)], ' ~w', [What]) ],
        findall(Utter, tproperty(Pred, Utter),
                U3),
        flatten([U0, U1, U2, U3], Explanation)
    ).
explain_predicate(Pred, Explanation) :-
    distinct(Explanation, predicate_summary(Pred, Explanation)).
explain_predicate(Pred, Explanation) :-
    referenced(Pred, Explanation).

:- if(current_predicate(man_object_property/2)).
predicate_summary(Pred, Explanation) :-
    Pred = _Module:Head,
    functor(Head, Name, Arity),
    man_object_property(Name/Arity, summary(Summary)),
    source_file(Pred, File),
    current_prolog_flag(home, Home),
    sub_atom(File, 0, _, _, Home),
    Explanation = [indent, 'Summary: "~w"'-[Summary] ].
:- else.
predicate_summary(_Pred, _Explanation) :-
    fail.
:- endif.


                /********************************
                *          REFERENCES           *
                *********************************/

referenced(Term, Explanation) :-
    current_predicate(_, Module:Head),
    (   predicate_property(Module:Head, built_in)
    ->  current_prolog_flag(access_level, system)
    ;   true
    ),
    \+ predicate_property(Module:Head, imported_from(_)),
    Module:Head \= help_index:predicate(_,_,_,_,_),
    nth_clause(Module:Head, N, Ref),
    '$xr_member'(Ref, Term),
    utter_referenced(Module:Head, N, Ref,
                     'Referenced', Explanation).
referenced(_:Head, Explanation) :-
    current_predicate(_, Module:Head),
    (   predicate_property(Module:Head, built_in)
    ->  current_prolog_flag(access_level, system)
    ;   true
    ),
    \+ predicate_property(Module:Head, imported_from(_)),
    nth_clause(Module:Head, N, Ref),
    '$xr_member'(Ref, Head),
    utter_referenced(Module:Head, N, Ref,
                     'Possibly referenced', Explanation).

utter_referenced(_Module:class(_,_,_,_,_,_), _, _, _, _) :-
    current_prolog_flag(xpce, true),
    !,
    fail.
utter_referenced(_Module:lazy_send_method(_,_,_), _, _, _, _) :-
    current_prolog_flag(xpce, true),
    !,
    fail.
utter_referenced(_Module:lazy_get_method(_,_,_), _, _, _, _) :-
    current_prolog_flag(xpce, true),
    !,
    fail.
utter_referenced(From, _, _, _, _) :-
    hide_reference(From),
    !,
    fail.
utter_referenced(pce_xref:defined(_,_,_), _, _, _, _) :-
    !,
    fail.
utter_referenced(pce_xref:called(_,_,_), _, _, _, _) :-
    !,
    fail.
utter_referenced(pce_principal:send_implementation(_, _, _),
                 _, Ref, Text, Explanation) :-
    current_prolog_flag(xpce, true),
    !,
    xpce_method_id(Ref, Id),
    Explanation = [indent, '~w from ~w'-[Text, Id]].
utter_referenced(pce_principal:get_implementation(Id, _, _, _),
                 _, Ref, Text, Explanation) :-
    current_prolog_flag(xpce, true),
    !,
    xpce_method_id(Ref, Id),
    Explanation = [indent, '~w from ~w'-[Text, Id]].
utter_referenced(Head, N, Ref, Text, Explanation) :-
    clause_property(Ref, file(File)),
    clause_property(Ref, line_count(Line)),
    !,
    pi_head(PI, Head),
    Explanation = [ indent,
                    '~w from ~d-th clause of '-[Text, N],
                    pi(PI), ' at ', url(File:Line)
                  ].
utter_referenced(Head, N, _Ref, Text, Explanation) :-
    pi_head(PI, Head),
    Explanation = [ indent,
                    '~w from ~d-th clause of '-[Text, N],
                    pi(PI)
                  ].

xpce_method_id(Ref, Id) :-
    clause(Head, _Body, Ref),
    strip_module(Head, _, H),
    arg(1, H, Id).

hide_reference(pce_xref:exported(_,_)).
hide_reference(pce_xref:defined(_,_,_)).
hide_reference(pce_xref:called(_,_,_)).
hide_reference(prolog_xref:called(_,_,_,_,_)).
hide_reference(prolog_xref:pred_mode(_,_,_)).
hide_reference(prolog_xref:exported(_,_)).
hide_reference(prolog_xref:dynamic(_,_,_)).
hide_reference(prolog_xref:imported(_,_,_)).
hide_reference(prolog_xref:pred_comment(_,_,_,_)).
hide_reference(_:'$mode'(_,_)).
hide_reference(_:'$pldoc'(_,_,_,_)).
hide_reference(_:'$pldoc_link'(_,_)).
hide_reference(prolog_manual_index:man_index(_,_,_,_,_)).


                /********************************
                *           MESSAGES            *
                *********************************/

:- multifile
    prolog:message//1.

prolog:message(explain(Explanation)) -->
    report(Explanation).

report(Explanation) -->
    { string(Explanation),
      !,
      split_string(Explanation, "\n", "", Lines)
    },
    lines(Lines).
report(Explanation) -->
    { is_list(Explanation) },
    report_list(Explanation).

lines([]) -->
    [].
lines([H]) -->
    !,
    [ '~s'-[H] ].
lines([H|T]) -->
    [ '~s'-[H], nl ],
    lines(T).

report_list([]) -->
    [].
report_list([H|T]) -->
    report1(H),
    report_list(T).

report1(indent) -->
    !,
    [ '~t~6|'-[] ].
report1(String) -->
    { atomic(String) },
    [ '~w'-[String] ].
report1(Fmt-Args) -->
    !,
    [ Fmt-Args ].
report1(url(Location)) -->
    [ url(Location) ].
report1(url(URL, Label)) -->
    [ url(URL, Label) ].
report1(pi(PI)) -->
    { pi_nt(PI, NT) },
    [ ansi(code, '~q', [NT]) ].
report1(ansi(Style, Fmt, Args)) -->
    [ ansi(Style, Fmt, Args) ].
report1(isa(Obj, Fmt-Args)) -->
    !,
    [ ansi(code, '~p', [Obj]),
      ansi([bold,fg(default)], ' is ', []),
      ansi([bold,fg(default)], Fmt, Args)
    ].
report1(isa(Obj, Descr)) -->
    [ ansi(code, '~p', [Obj]),
      ansi([bold,fg(default)], ' is ~w', [Descr])
    ].

pi_nt(Module:Name/Arity, NT),
    atom(Module), atom(Name), integer(Arity),
    Arity >= 2,
    functor(Head, Name, Arity),
    predicate_property(Module:Head, non_terminal) =>
    Arity2 is Arity - 2,
    NT = Module:Name//Arity2.
pi_nt(PI, NT) =>
    NT = PI.
