/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2014, VU University, Amsterdam
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

:- module(test_resource_error, [test_resource_error/0]).
:- use_module(library(plunit)).

/** <module> Test Resource overflows

This module is a Unit test for handling resource overflows.

@author	Jan Wielemaker
*/

test_resource_error :-
	run_tests([ resource_error
		  ]).

small_stacks(limits(LLimit, GLimit)) :-
	prolog_stack_property(local, limit(LLimit)),
	prolog_stack_property(global, limit(GLimit)),
	set_prolog_stack(local, limit(1*1024*1024)),
	set_prolog_stack(global, limit(1*1024*1024)).

restore_stacks(limits(LLimit, GLimit)) :-
	set_prolog_stack(local, limit(LLimit)),
	set_prolog_stack(global, limit(GLimit)).


:- begin_tests(resource_error,
	       [ setup(small_stacks(Restore)),
		 cleanup(restore_stacks(Restore))
	       ]).

choice.
choice.

local_overflow :-
	choice,
	local_overflow.

global_overflow(X) :-			% Causes gracefully signalled overflow
	global_overflow(s(X)).

string_overflow(StringList) :-
	string_overflow2(StringList),
	is_list(StringList).		% avoid GC of list

string_overflow2([H|T]) :-		% Causes PL_throw() overflow
	format(string(H), '~txx~1000000|', []),
	string_overflow2(T).

:- dynamic
        tmp/0.

big_clause(N) :-
        link_clause(N, Body),
        retractall(tmp),
        Clause = (tmp :- Body),
        assert(Clause).

link_clause(1, X=X) :- !.
link_clause(N, (X=X, G)) :-
        N2 is N - 1,
        link_clause(N2, G).


test(local, throws(error(resource_error(stack), local))) :-
	local_overflow.
test(global, throws(error(resource_error(stack), global))) :-
	global_overflow(0).
test(string, throws(error(resource_error(stack), global))) :-
	catch(string_overflow(_), E1, true),
	E1 = error(resource_error(stack), global),
	string_overflow(_).
test(string, throws(error(resource_error(stack), global))) :-
	catch(string_overflow(_), E1, true),
	E1 = error(resource_error(stack), global),
	global_overflow(_).
test(length, throws(error(resource_error(stack), global))) :-
	length(L, 10000000),
	is_list(L).			% avoid GC
test(tight_stacks, throws(error(resource_error(stack), global))) :-
	between(1, 20, X),
	trim_stacks,
	Len is 1<<X,
	length(List, Len),
	numbervars(List, 0, _),
	fail.
test(cleanup_handler, [setup((thread_create(big_clause(500000), Id, []),
                              thread_join(Id, true))),
                       throws(error(resource_error(stack), local)),
                       cleanup(retractall(tmp))]) :-
	setup_call_cleanup(true, tmp, true).
:- end_tests(resource_error).
