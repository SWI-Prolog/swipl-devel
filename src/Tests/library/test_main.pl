/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(test_main,
          [ test_main/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(main)).
:- use_module(library(debug)).
:- use_module(library(optparse)).

/** <module> Test library(main) option processing
*/

test_main :-
    run_tests([ argv_options_unguided,
                argv_options_bool,
                argv_options_typed,
                argv_options_pos
              ]).

:- begin_tests(argv_options_unguided).

test(unguided, Pos-Opt == []-[]) :-
    argv_options([], Pos, Opt).
test(unguided, Pos-Opt == [a1]-[]) :-
    argv_options([a1], Pos, Opt).
test(unguided, Pos-Opt == [a1]-[name(arg)]) :-
    argv_options(['--name=arg', a1], Pos, Opt).
test(unguided, Pos-Opt == [a1]-[name(42)]) :-
    argv_options(['--name=42', a1], Pos, Opt).
test(unguided, Pos-Opt == [a1]-[pi(3.14)]) :-
    argv_options(['--pi=3.14', a1], Pos, Opt).
test(unguided, Pos-Opt == [a0,a1]-[pi(3.14)]) :-
    argv_options([a0, '--pi=3.14', a1], Pos, Opt).

:- end_tests(argv_options_unguided).

:- begin_tests(argv_options_bool).

opt_type(true,  true,    boolean).
opt_type(t,     true,    boolean).
opt_type(false, false,   boolean(false)).
opt_type(f,     false,   boolean(false)).

test(guided, Pos-Opt == []-[]) :-
    argv_options([], Pos, Opt).
test(remaining, Pos-Opt == ['--true']-[]) :-
    argv_options(['--', '--true'], Pos, Opt).
test(position, Pos-Opt == [aap, noot]-[true(true)]) :-
    argv_options([aap, '--true', noot], Pos, Opt).
% normal booleans
test(true, Pos-Opt == []-[true(true)]) :-
    argv_options(['-t'], Pos, Opt).
test(true, Pos-Opt == []-[true(true)]) :-
    argv_options(['--true'], Pos, Opt).
test(true, Pos-Opt == []-[true(false)]) :-
    argv_options(['--no-true'], Pos, Opt).
test(true, Pos-Opt == []-[true(false)]) :-
    argv_options(['--notrue'], Pos, Opt).
test(true, Pos-Opt == []-[true(true)]) :-
    argv_options(['--true=true'], Pos, Opt).
test(true, Pos-Opt == []-[true(true)]) :-
    argv_options(['--true=on'], Pos, Opt).
test(true, Pos-Opt == []-[true(false)]) :-
    argv_options(['--true=false'], Pos, Opt).
test(true, Pos-Opt == []-[true(false)]) :-
    argv_options(['--true=off'], Pos, Opt).
% negated booleans
test(false, Pos-Opt == []-[false(false)]) :-
    argv_options(['-f'], Pos, Opt).
test(false, Pos-Opt == []-[false(false)]) :-
    argv_options(['--false'], Pos, Opt).
test(false, Pos-Opt == []-[false(true)]) :-
    argv_options(['--no-false'], Pos, Opt).
test(false, Pos-Opt == []-[false(true)]) :-
    argv_options(['--nofalse'], Pos, Opt).
test(false, Pos-Opt == []-[false(true)]) :-
    argv_options(['--false=true'], Pos, Opt).
test(false, Pos-Opt == []-[false(true)]) :-
    argv_options(['--false=on'], Pos, Opt).

:- end_tests(argv_options_bool).

:- begin_tests(argv_options_typed).

opt_type(atom,    atom,    atom).
opt_type(a,       atom,    atom).
opt_type(dice,    dice,    between(1,6)).
opt_type(d,       dice,    between(1,6)).
opt_type(percent, percent, between(0.0,100.0)).
opt_type(p,       percent, between(0.0,100.0)).
opt_type(integer, integer, integer).
opt_type(i,       integer, integer).
opt_type(float,   float,   float).
opt_type(f,       float,   float).
opt_type(string,  string,  string).
opt_type(s,       string,  string).
opt_type(enum,    enum,    oneof([silent,warning,error])).
opt_type(e,       enum,    oneof([silent,warning,error])).
opt_type(t,       term,    term).
opt_type(tv,      term,    term([variable_names(_)])).

% int tests
test(int, Pos-Opt == []-[integer(42)]) :-
    argv_options(['-i', '42'], Pos, Opt).
test(int, Pos-Opt == []-[integer(42)]) :-
    argv_options(['-i42'], Pos, Opt).
test(int, error(opt_error(value_type(i,integer,xyz)))) :-
    argv_options(['-ixyz'], _Pos, _Opt, []).
test(int, error(opt_error(value_type(i,integer,'3.2')))) :-
    argv_options(['-i', '3.2'], _Pos, _Opt, []).
% range tests
test(int, Pos-Opt == []-[dice(6)]) :-
    argv_options(['-d', '6'], Pos, Opt).
test(int, error(opt_error(value_type(d,between(1,6),'0')))) :-
    argv_options(['-d', '0'], _Pos, _Opt, []).
test(percent, Pos-Opt == []-[percent(50.0)]) :-
    argv_options(['-p', '50'], Pos, Opt).
test(percent, error(opt_error(value_type(p,between(0.0,100.0),'-10')))) :-
    argv_options(['-p', '-10'], _Pos, _Opt, []).
% Float tests
test(float, Pos-Opt == []-[float(-3.14)]) :-
    argv_options(['-f', '-3.14'], Pos, Opt).
% Atom tests
test(atom, Pos-Opt == []-[atom(aap)]) :-
    argv_options(['-a', 'aap'], Pos, Opt).
test(atom, Pos-Opt == []-[atom(aap)]) :-
    argv_options(['-aaap'], Pos, Opt).
test(atom, Pos-Opt == []-[atom(aap)]) :-
    argv_options(['--atom=aap'], Pos, Opt).
test(atom, Pos-Opt == []-[atom(aap)]) :-
    argv_options(['--atom', 'aap'], Pos, Opt).
% enum tests
test(enum, Pos-Opt == []-[enum(silent)]) :-
    argv_options(['-e', 'silent'], Pos, Opt).
test(enum, error(opt_error(value_type(e, oneof([silent,warning,error]),
                                      'fatal')))) :-
    argv_options(['-e', 'fatal'], _Pos, _Opt, []).
test(term, Opt =@= [term(p(_))]) :-
    argv_options(['-t', 'p(X)'], _Pos, Opt, []).
test(term, Opt =@= [term(p(X)-['X'=X])]) :-
    argv_options(['--tv', 'p(X)'], _Pos, Opt, []).
test(pass, Pos == ['--x', v]) :-
    argv_options(['--x', v], Pos, Opt, [unknown_option(pass)]),
    assertion(Opt == []).
test(pass, Pos == ['--x', v]) :-
    argv_options(['--atom', a, '--x', v], Pos, Opt, [unknown_option(pass)]),
    assertion(Opt == [atom(a)]).
test(pass, Pos == ['--x', v]) :-
    argv_options(['--atom', a, '--x', v, '-i', '42'], Pos, Opt,
                 [unknown_option(pass)]),
    assertion(Opt == [atom(a), integer(42)]).
test(pass_short, Pos == ['-z', v]) :-
    argv_options(['-z', v], Pos, Opt, [unknown_option(pass)]),
    assertion(Opt == []).
test(pass_short, Pos == ['-z', v]) :-
    argv_options(['-zi', '42', v], Pos, Opt, [unknown_option(pass)]),
    assertion(Opt == [integer(42)]).

:- end_tests(argv_options_typed).

:- begin_tests(argv_options_pos).

opt_type(a,    atom,    atom).

test(no_opts_after_pos, Pos-Opt == []-[atom(aap)]) :-
    argv_options(['-a', 'aap'], Pos, Opt, [options_after_arguments(false)]).
test(no_opts_after_pos, Pos-Opt == [a0, '-a', 'aap']-[]) :-
    argv_options([a0, '-a', 'aap'], Pos, Opt, [options_after_arguments(false)]).
test(no_opts_after_pos, Pos-Opt == [a0, '-a', 'aap']-[atom(t)]) :-
    argv_options(['-a', 't', a0, '-a', 'aap'], Pos, Opt,
                 [options_after_arguments(false)]).

:- end_tests(argv_options_pos).
