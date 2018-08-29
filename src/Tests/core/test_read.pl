/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, University of Amsterdam
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

:- module(test_read, [test_read/0]).
:- use_module(library(plunit)).

/** <module> Read tests

Test term reading, notably option processing

@author	Jan Wielemaker
*/

test_read :-
	run_tests([ read_term
		  ]).

:- begin_tests(read_term).

test(singletons, Names == ['_a','_A','_0','A']) :-
	term_string(_, "a(_,_a,_A,_0,A)",
		    [ singletons(Singletons)
		    ]),
	maplist(arg(1), Singletons, Names).
test(warn_singletons, Messages =@= [singletons(a(_,_,_,_,_), ['_a','A'])]) :-
	catch_messages(_,
		       term_string(_, "a(_,_a,_A,_0,A)",
				   [ singletons(warning)
				   ]),
		       Messages).

:- end_tests(read_term).

%%	catch_messages(+Kind, :Goal, -Messages) is semidet.

:- thread_local
	message/1.
:- meta_predicate
	catch_messages(?, 0, -).

catch_messages(Kind, Goal, Messages) :-
	setup_call_cleanup(
	    asserta((user:thread_message_hook(Term, Kind, _) :-
		        \+ \+ (prolog_load_context(variable_names, VarNames),
			       bind_variable_names(VarNames),
			       assertz(message(Term)))), Ref),
	    once(Goal),
	    erase(Ref)),
	findall(Msg, retract(message(Msg)), Messages).

bind_variable_names([]).
bind_variable_names([Name='$VAR'(Int)|T]) :- !,
	var_name(Int, Name),
	bind_variable_names(T).
bind_variable_names([_|T]) :-
	bind_variable_names(T).

var_name(N, Name) :-
	atom_codes(Name, [C]),
	between(0'A, 0'Z, C),
	N is C - 0'A.
