/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2022, VU University Amsterdam
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

:- module(test_sandbox,
	  [ test_sandbox/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test set for library(sandbox)


@tbd	A lot more tests.  In particular test and precise the relation
	between sandboxs and modules.
*/

test_sandbox :-
	run_tests([ sandbox
		  ]).

:- begin_tests(sandbox).
:- use_module(library(sandbox)).
:- use_module(library(aggregate)).
:- use_module(library(settings)).

my_call(G) :-
	phrase({G},_).

test(cleanup) :-
	safe_goal(setup_call_cleanup(true,true,true)).
test(time) :-
	safe_goal(time(true)).
test(setof) :-
	safe_goal(setof(X, Y^between(1, Y, X), _Xs)).
test(phrase) :-
	safe_goal(phrase("hello", `hello`, [])).
test(apply) :-
	safe_goal(forall(true,true)).
test(aggregate) :-
	safe_goal(aggregate(count, between(1,10,_), _Count)).
test(aggregate) :-
	safe_goal(aggregate(sum(I), X^between(1,X,I), _Count)).
test(dcg, error(permission_error(call, sandboxed, open(_,_,_)))) :-
	safe_goal(my_call(open(_,_,_))).
test(contexr, error(permission_error(call, sandboxed, @(_,_)))) :-
	safe_goal(@(open(_,_,_), user)).
test(assert) :-
	safe_goal(assert(bar)).
test(assert, error(permission_error(call, sandboxed, _))) :-
	safe_goal(assert(foo:bar)).
test(predicate_property) :-
	safe_goal(predicate_property(member(_,_), _P)).
test(predicate_property, error(permission_error(call, sandboxed, _))) :-
	safe_goal(predicate_property(lists:member(_,_), _P)).
test(flag, error(permission_error(call, sandboxed, _))) :-
	safe_goal(set_prolog_flag(threads, false)).
test(flag) :-
	safe_goal(set_prolog_flag(double_quotes, chars)).
test(portray_goal, error(permission_error(call, sandboxed, _))) :-
	safe_goal(write_term(a, [portray_goal(shell)])).
test(portray_goal, error(permission_error(call, sandboxed, _))) :-
	safe_goal(term_string(a, _, [portray_goal(shell)])).
test(portray_goal, error(permission_error(call, sandboxed, _))) :-
	safe_goal(format("~W", [a, [portray_goal(shell)]])).
test(portray_goal, error(instantiation_error)) :-
	safe_goal(write_term(a, _)).
test(write_term) :-
	safe_goal(write_term(a, [quoted(true)])),
	safe_goal(term_string(a, _, [quoted(true)])),
	safe_goal(format("~W", [a, [quoted(true)]])).
test(message, error(permission_error(call, sandboxed, _))) :-
	safe_goal(print_message(error, format("~@", [shell(true)]))).
test(message, error(permission_error(call, sandboxed, _))) :-
	safe_goal(print_message(error, error(format("~@", [shell(true)]), _))).
test(message, error(permission_error(call, sandboxed, _))) :-
	safe_goal(message_to_string(debug("~@", [shell(true)]), _)).
test(message, error(instantiation_error)) :-
	safe_goal(print_message(error, _)).
test(message, error(instantiation_error)) :-
	safe_goal(print_message(error, error(_, _))).
test(throw, error(permission_error(call, sandboxed, _))) :-
	safe_goal(throw(error(format("~@", [true]), _))).
test(throw, error(permission_error(call, sandboxed, _))) :-
	safe_goal(throw(format("~w", [a]))).
test(throw, error(instantiation_error)) :-
	safe_goal(throw(_)).
test(throw, error(instantiation_error)) :-
	safe_goal(throw(error(_, _))).
test(throw) :-
	safe_goal(throw(error(type_error(integer, a), _))),
	safe_goal(throw(my_ball)).
test(catch_print) :-
	safe_goal(catch(true, E, print_message(error, E))),
	safe_goal(catch(true, E, throw(E))).
test(catch_print, error(instantiation_error)) :-
	safe_goal(catch(true, _, throw(_))).
test(catch_print, error(permission_error(call, sandboxed, _))) :-
	safe_goal(catch(throw(format("~@", [true])), E, print_message(error, E))).
test(message) :-
	safe_goal(print_message(informational, format("~w", [a]))),
	safe_goal(print_message(informational, format("~@", [writeln(a)]))).
test(use_module, error(_)) :-
	safe_goal(use_module(library('../library/lists'))).
test(use_module, error(_)) :-
	safe_goal(use_module(library('dialect/../lists'))).
test(use_module) :-
	safe_goal(use_module(library(lists))).
% Settings of other modules may hold, e.g., passwords.
test(setting, error(permission_error(call, sandboxed, _))) :-
	safe_goal(setting(app:password, _)).
test(setting, error(permission_error(call, sandboxed, _))) :-
	safe_goal(setting(_:_, _)).
test(setting) :-
	safe_goal(setting(my_setting, _)),
	safe_goal(setting(_, _)).

:- end_tests(sandbox).
