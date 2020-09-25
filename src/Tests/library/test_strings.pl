/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v.
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

:- module(test_strings,
          [ test_strings/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(strings)).

test_strings :-
    run_tests([strings]).

:- begin_tests(strings).

test(plain, A == "hello world") :-
    A = {|string(To)||hello world|}.
test(interpolate, A == "hello world") :-
    To = world,
    A = {|string(To)||hello {To}|}.
test(interpolate, error(existence_error(template_var, 'ToX'))) :-
    To = world,
    _ = {|string(To)||hello {ToX}|}.
test(interpolate, A == "hello world") :-
    To = no_world,
    A = {|string(To)||hello {ToX,world}|}.
test(dedent, A == "hello\n  world\n") :-
    A = {|string||
	 | hello
         |   world
         |}.
test(dedent, A == "hello\n  world\n") :-
    A = {|string||
	 hello
           world
         |}.
test(dedent, A == "hello\n  world") :-
    A = {|string||
	 | hello
         |   world|}.

:- end_tests(strings).
