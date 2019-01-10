/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2018, University of Amsterdam,
                              VU University Amsterdam
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
:- if(exists_source(library(pldoc/man_index))).
:- use_module(library(pldoc/man_index)).
:- elif(exists_source(library(helpidx))).
:- use_module(library(helpidx)).
:- endif.
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Describe Prolog Terms

The   library(explain)   describes   prolog-terms.   The   most   useful
functionality is its cross-referencing function.

==
?- explain(subset(_,_)).
"subset(_, _)" is a compound term
        Referenced from 2-th clause of lists:subset/2
        Referenced from 46-th clause of prolog_xref:imported/3
        Referenced from 68-th clause of prolog_xref:imported/3
lists:subset/2 is a predicate defined in
        /staff/jan/lib/pl-5.6.17/library/lists.pl:307
        Referenced from 2-th clause of lists:subset/2
        Possibly referenced from 2-th clause of lists:subset/2
==

Note  that  the  help-tool  for   XPCE    provides   a   nice  graphical
cross-referencer.
*/

%!  explain(@Term) is det
%
%   Give an explanation on Term. The  argument   may  be any Prolog data
%   object. If the argument is an atom,  a term of the form `Name/Arity`
%   or a term of the form   `Module:Name/Arity`, explain/1 describes the
%   predicate as well as possible references to it. See also gxref/0.

explain(Item) :-
    explain(Item, Explanation),
    writeln(Explanation),
    fail.
explain(_).

                /********************************
                *           BASIC TYPES         *
                *********************************/

%!  explain(@Term, -Explanation) is nondet.
%
%   True when Explanation is an explanation of Term.

explain(Var, Explanation) :-
    var(Var),
    !,
    utter(Explanation, '"~w" is an unbound variable', [Var]).
explain(I, Explanation) :-
    integer(I),
    !,
    utter(Explanation, '"~w" is an integer', [I]).
explain(F, Explanation) :-
    float(F),
    !,
    utter(Explanation, '"~w" is a floating point number', [F]).
explain(S, Explanation) :-
    string(S),
    !,
    utter(Explanation, '"~w" is a string', S).
explain([], Explanation) :-
    !,
    utter(Explanation, '"[]" is a special constant denoting an empty list', []).
explain(A, Explanation) :-
    atom(A),
    utter(Explanation, '"~w" is an atom', [A]).
explain(A, Explanation) :-
    atom(A),
    current_op(Pri, F, A),
    op_type(F, Type),
    utter(Explanation, '"~w" is a ~w (~w) operator of priority ~d',
          [A, Type, F, Pri]).
explain(A, Explanation) :-
    atom(A),
    !,
    explain_atom(A, Explanation).
explain([H|T], Explanation) :-
    is_list(T),
    !,
    List = [H|T],
    length(List, L),
    (   utter(Explanation, '"~p" is a proper list with ~d elements',
              [List, L])
    ;   maplist(printable, List),
        utter(Explanation, '~t~8|Text is "~s"',  [List])
    ).
explain([H|T], Explanation) :-
    !,
    length([H|T], L),
    !,
    utter(Explanation, '"~p" is a not-closed list with ~d elements',
          [[H|T], L]).
explain(Name/Arity, Explanation) :-
    atom(Name),
    integer(Arity),
    !,
    functor(Head, Name, Arity),
    known_predicate(Module:Head),
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
explain(Module:Head, Explanation) :-
    callable(Head),
    !,
    explain_predicate(Module:Head, Explanation).
explain(Term, Explanation) :-
    numbervars(Term, 0, _, [singletons(true)]),
    utter(Explanation, '"~W" is a compound term',
          [Term, [quoted(true), numbervars(true)]]).
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
    ),
    !.
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
    between(32, 126, C).

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
        utter(Explanation,
              '~w:~w/~d is an undefined predicate', [M,N,A])
    ;   referenced(M:Head, Explanation)
    ).


                /********************************
                *           PREDICATE           *
                *********************************/

lproperty(built_in,     ' built-in', []).
lproperty(dynamic,      ' dynamic', []).
lproperty(multifile,    ' multifile', []).
lproperty(transparent,  ' meta', []).

tproperty(imported_from(Module), ' imported from module ~w', [Module]).
tproperty(file(File),           ' defined in~n~t~8|~w', [File]).
tproperty(line_count(Number),   ':~d', [Number]).
tproperty(autoload,             ' that can be autoloaded', []).

combine_utterances(Pairs, Explanation) :-
    maplist(first, Pairs, Fmts),
    atomic_list_concat(Fmts, Format),
    maplist(second, Pairs, ArgList),
    flatten(ArgList, Args),
    utter(Explanation, Format, Args).

first(A-_B, A).
second(_A-B, B).

%!  explain_predicate(:Head, -Explanation) is det.

explain_predicate(Pred, Explanation) :-
    Pred = Module:Head,
    functor(Head, Name, Arity),

    (   predicate_property(Pred, undefined)
    ->  utter(Explanation,
              '~w:~w/~d is an undefined predicate', [Module,Name,Arity])
    ;   (   var(Module)
        ->  U0 = '~w/~d is a' - [Name, Arity]
        ;   U0 = '~w:~w/~d is a' - [Module, Name, Arity]
        ),
        findall(Fmt-Arg, (lproperty(Prop, Fmt, Arg),
                          predicate_property(Pred, Prop)),
                U1),
        U2 = ' predicate' - [],
        findall(Fmt-Arg, (tproperty(Prop, Fmt, Arg),
                          predicate_property(Pred, Prop)),
                U3),
        flatten([U0, U1, U2, U3], Utters),
        combine_utterances(Utters, Explanation)
    ).
:- if(current_predicate(man_object_property/2)).
explain_predicate(Pred, Explanation) :-
    Pred = _Module:Head,
    functor(Head, Name, Arity),
    man_object_property(Name/Arity, summary(Summary)),
    source_file(Pred, File),
    current_prolog_flag(home, Home),
    sub_atom(File, 0, _, _, Home),
    utter(Explanation, '~t~8|Summary: ``~w''''', [Summary]).
:- elif(current_predicate(predicate/5)).
explain_predicate(Pred, Explanation) :-
    predicate_property(Pred, built_in),
    Pred = _Module:Head,
    functor(Head, Name, Arity),
    predicate(Name, Arity, Summary, _, _),
    utter(Explanation, '~t~8|Summary: ``~w''''', [Summary]).
:- endif.
explain_predicate(Pred, Explanation) :-
    referenced(Pred, Explanation).

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
utter_referenced(pce_xref:exported(_,_), _, _, _, _) :-
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
    utter(Explanation, '~t~8|~w from ~w', [Text, Id]).
utter_referenced(pce_principal:get_implementation(Id, _, _, _),
                 _, Ref, Text, Explanation) :-
    current_prolog_flag(xpce, true),
    !,
    xpce_method_id(Ref, Id),
    utter(Explanation, '~t~8|~w from ~w', [Text, Id]).
utter_referenced(Module:Head, N, _Ref, Text, Explanation) :-
    functor(Head, Name, Arity),
    utter(Explanation,
          '~t~8|~w from ~d-th clause of ~w:~w/~d',
          [Text, N, Module, Name, Arity]).

xpce_method_id(Ref, Id) :-
    clause(Head, _Body, Ref),
    strip_module(Head, _, H),
    arg(1, H, Id).



                /********************************
                *             UTTER            *
                *********************************/

utter(Explanation, Fmt, Args) :-
    format(string(Explanation), Fmt, Args).


