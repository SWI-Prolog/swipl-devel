/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2011, University of Amsterdam
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

:- module(test_settings,
	  [ test_settings/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(settings)).
:- use_module(library(readutil)).

test_settings :-
	run_tests([ settings
		  ]).

:- begin_tests(settings).

:- setenv('VNC_DISPLAY', '11').

:- setting(test, atom,    hello, 'Test setting').
:- setting(port, integer, 5900+env('VNC_DISPLAY'), 'Test user arithmetic').

%%	reset
%
%	Hack to cleanup setting defaults

reset :-
	retractall(settings:st_default(_,_,_)),
	retractall(settings:st_value(_,_,_)),
	retractall(settings:local_file(_)).

%================================================================

test(get, X == hello) :-
	setting(test, X).
test(ch_default, [X == hello_world, cleanup(reset)]) :-
	set_setting_default(test, hello_world),
	setting(test, X).
test(save_default, [ [X,Terms] == [hello_world,[]],
		     cleanup((reset, delete_file(DB)))
		   ]) :-
	tmp_file(settings, DB),
	set_setting_default(test, hello_world),
	save_settings(DB),
	read_file_to_terms(DB, Terms, []),
	load_settings(DB),
	setting(test, X).
test(restore_default, [ [X] == [hello_world],
			cleanup(reset)
		      ]) :-
	set_setting_default(test, hello_world),
	set_setting(test, hello_universe),
	restore_setting(test),
	setting(test, X).

test(arith, Port == 5911) :-
	setting(port, Port).

:- end_tests(settings).

