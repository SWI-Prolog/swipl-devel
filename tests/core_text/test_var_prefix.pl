/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_var_prefix, [test_var_prefix/0]).
:- use_module(library(plunit)).
:- use_module(library(listing), [portray_clause/3]).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(lists), [member/2]).

/** <module> Test the var_prefix flag

Tests reading and writing terms with the Prolog flag `var_prefix` set to
a character.
*/

test_var_prefix :-
    run_tests([ var_prefix_flag,
                var_prefix_read,
                var_prefix_write
              ]).

:- set_prolog_flag(test_var_prefix_q:var_prefix, ?).
:- op(250, yfx, test_var_prefix_q:(?)).     % as defined by xpce
:- set_prolog_flag(test_var_prefix_u:var_prefix, '_').

%!  rd(+String, -Term, -Bindings) is det.
%!  rd(+Module, +String, -Term, -Bindings) is det.
%
%   Read String in the context of a module with var_prefix `?`.

rd(String, Term, Bindings) :-
    rd(test_var_prefix_q, String, Term, Bindings).
rd(M, String, Term, Bindings) :-
    term_string(Term, String, [module(M), variable_names(Bindings)]).

%!  wr(+Module, +Term, +Bindings, -String) is det.

wr(M, Term, Bindings, String) :-
    with_output_to(string(String),
                   write_term(Term, [ module(M),
                                      quoted(true),
                                      numbervars(true),
                                      variable_names(Bindings)
                                    ])).

%!  round_trip(+Module, +String) is semidet.
%
%   True if reading String, writing it and reading the result produces
%   a variant.

round_trip(M, String) :-
    rd(M, String, T, B),
    wr(M, T, B, Out),
    rd(M, Out, T2, _),
    T2 =@= T.

%!  style_warnings(+String, -Messages) is det.
%
%   Load String as a clause into a module with var_prefix `?` and
%   collect  the  singleton  warnings  as  singletons(Names)  and   the
%   multiton warnings as multitons(Count).

style_warnings(String, Messages) :-
    setup_call_cleanup(
        asserta((user:thread_message_hook(Term, warning, _) :-
                    style_message(Term, Msg),
                    assertz(style_msg(Msg))), Ref),
        setup_call_cleanup(
            open_string(String, In),
            load_files(test_var_prefix_q:style_warnings, [stream(In)]),
            close(In)),
        erase(Ref)),
    findall(Msg, retract(style_msg(Msg)), Messages).

:- dynamic style_msg/1.

style_message(singletons(_, Names), singletons(Names)).
style_message(compiler_warnings(_, Warnings), multitons(Count)) :-
    aggregate_all(count, member(multiton(_), Warnings), Count).

:- begin_tests(var_prefix_flag).

test(value, V == ?) :-
    current_prolog_flag(test_var_prefix_q:var_prefix, V).
test(default, V == false) :-
    current_prolog_flag(test_var_prefix:var_prefix, V).
test(true_is_underscore, V == '_') :-
    setup_call_cleanup(
        set_prolog_flag(test_var_prefix_t:var_prefix, true),
        current_prolog_flag(test_var_prefix_t:var_prefix, V),
        set_prolog_flag(test_var_prefix_t:var_prefix, false)).
test(reset, V == false) :-
    set_prolog_flag(test_var_prefix_t:var_prefix, #),
    set_prolog_flag(test_var_prefix_t:var_prefix, false),
    current_prolog_flag(test_var_prefix_t:var_prefix, V).
test(letter, error(domain_error(var_prefix, a))) :-
    set_prolog_flag(test_var_prefix_t:var_prefix, a).
test(solo, error(domain_error(var_prefix, !))) :-
    set_prolog_flag(test_var_prefix_t:var_prefix, !).
test(dot, error(domain_error(var_prefix, '.'))) :-
    set_prolog_flag(test_var_prefix_t:var_prefix, '.').
test(two_chars, error(domain_error(var_prefix, '??'))) :-
    set_prolog_flag(test_var_prefix_t:var_prefix, '??').
test(type, error(type_error(atom, 1))) :-
    set_prolog_flag(test_var_prefix_t:var_prefix, 1).

:- end_tests(var_prefix_flag).

:- begin_tests(var_prefix_read).

test(var, [T, B] =@= [f(X,X), ['?x'=X]]) :-
    rd("f(?x,?x)", T, B).
test(atoms, T == f('Foo', '_foo', foo)) :-
    rd("f(Foo,_foo,foo)", T, _).
test(split_symbol, [T, B] =@= [X = Y, ['?x'=X, '?y'=Y]]) :-
    rd("?x=?y", T, B).
test(dict, [T, B] =@= [X{a:1}, ['?x'=X]]) :-
    rd("?x{a:1}", T, B).
test(anon, [T, B] =@= [f(_,_), []]) :-
    rd("f(?_,?_)", T, B).
test(symbol, T == f(?, ?-, ??)) :-
    rd("f(?, ?-, ??)", T, _).
test(infix, T == ?(a,b)) :-
    rd("a? b", T, _).
test(digit, [T, B] =@= [f(X), ['?1'=X]]) :-
    rd("f(?1)", T, B).
test(option, [T, B] =@= [f(X,'Y'), ['?x'=X]]) :-
    term_string(T, "f(?x,Y)", [var_prefix(?), variable_names(B)]).
test(option_false, T =@= f(_)) :-
    term_string(T, "f(X)", [module(test_var_prefix_q), var_prefix(false)]).
test(option_error, error(domain_error(var_prefix, a))) :-
    term_string(_, "f(X)", [var_prefix(a)]).
test(singletons, Names == ['?x', '?_x', '?_X']) :-
    term_string(_, "f(?x,?_x,?_X,?_,?y,?y)",
                [module(test_var_prefix_q), singletons(Singletons)]),
    maplist(arg(1), Singletons, Names).
test(warn_singletons, Messages == [singletons(['?x'])]) :-
    style_warnings("f(?x,?_x,?_X,?_,?y,?y,?_1,?__z).", Messages).
test(warn_multitons, Messages == [multitons(3)]) :-      % ?_x, ?_X, ?__z
    style_warnings("f(?x,?x,?_x,?_x,?_X,?_X,?_1,?_1,?__z,?__z).", Messages).
test(underscore, [T, B] =@= [f(X,'Y',_), ['_x'=X]]) :-
    rd(test_var_prefix_u, "f(_x,Y,_)", T, B).

:- end_tests(var_prefix_read).

:- begin_tests(var_prefix_write).

test(round_trip, S == []) :-
    findall(S0, ( member(S0,
                         [ "f(?x,?x,Foo,_foo)",
                           "?x=?y",
                           "?x{a:1}",
                           "f(?_,?_)",
                           "f(?,?-)",
                           "a? b",
                           "?(a,?x)",
                           "?x?(?y)",
                           "=?(?x)",
                           "'?x'",
                           "f(_foo,'_foo','Foo')"
                         ]),
                  \+ round_trip(test_var_prefix_q, S0)
                ), S).
test(round_trip_u, S == []) :-
    findall(S0, ( member(S0,
                         [ "f(_x,_x,Foo,_)",
                           "f('_x', x)"
                         ]),
                  \+ round_trip(test_var_prefix_u, S0)
                ), S).
test(infix, S == "a? ?x") :-
    rd("?(a,?x)", T, B),
    wr(test_var_prefix_q, T, B, S).
test(numbervars, S == "f(?A,?_,?Foo,?x)") :-
    wr(test_var_prefix_q, f('$VAR'(0),'$VAR'('_'),'$VAR'('Foo'),'$VAR'('?x')),
       [], S).
test(numbervars_u, S == "f(_A,_,_Foo)") :-
    wr(test_var_prefix_u, f('$VAR'(0),'$VAR'('_'),'$VAR'('Foo')), [], S).
test(numbervars_user, S == "f(A,?x)") :-
    wr(user, f('$VAR'(0),'$VAR'('?x')), [], S).
test(fresh_var, true) :-
    wr(test_var_prefix_q, f(_), [], S),
    sub_string(S, 0, 4, _, "f(?_").
test(variable_names, S == "f(?x,?A)") :-
    wr(test_var_prefix_q, f(X,Y), ['?x'=X, 'A'=Y], S).
test(variable_names_error, error(domain_error(variable_name, x))) :-
    wr(test_var_prefix_q, f(X), [x=X], _).
test(portray_clause, T2 =@= T) :-
    rd("h(?x,?_y) :- b(?x,?z,Foo,_foo)", T, _),
    with_output_to(string(S),
                   portray_clause(current_output, T,
                                  [module(test_var_prefix_q)])),
    rd(S, T2, _).

:- end_tests(var_prefix_write).
