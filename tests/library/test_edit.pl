/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
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


:- module(test_edit,
          [ test_edit/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(edit)).

/** <module> Tests for library(edit)

An external editor is started with shell/1 on a command made from an
edit_command/2 template.  File names that could end the quotes around
the file name or that the shell expands are refused.
*/

test_edit :-
    run_tests([ edit_command ]).

:- begin_tests(edit_command,
               [ condition(current_prolog_flag(unix, true)),
                 setup(set_editor(Old)),
                 cleanup(set_prolog_flag(editor, Old))
               ]).

test(plain, Command == 'vi +3 \'/tmp/a.pl\'') :-
    command('/tmp/a.pl', Command).
test(quote, error(domain_error(shell_safe_file_name, _))) :-
    command('/tmp/x\';touch pwned;\'.pl', _).
test(dollar, error(domain_error(shell_safe_file_name, _))) :-
    command('/tmp/$(id).pl', _).
test(backquote, error(domain_error(shell_safe_file_name, _))) :-
    command('/tmp/`id`.pl', _).

:- end_tests(edit_command).

set_editor(Old) :-
    current_prolog_flag(editor, Old),
    set_prolog_flag(editor, vi).

command(File, Command) :-
    prolog_edit:external_edit_command(#{file:File, line:3}, Command).
