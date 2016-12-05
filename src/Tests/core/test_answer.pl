/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2011-2016, University of Amsterdam
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

:- module(test_answer,
	  [ test_answer/0,
	    test_answer/2
	  ]).

:- prolog_load_context(directory, Here),
   atom_concat(Here, '../../../../packages/clib', ClibDir0),
   absolute_file_name(ClibDir0, ClibDir),
   asserta(user:file_search_path(library, ClibDir)),
   asserta(user:file_search_path(foreign, ClibDir)).

:- if(absolute_file_name(foreign(unix), _,
			 [ file_type(executable),
			   file_errors(fail),
			   access(read)
			 ])).

:- use_module(library(plunit)).
:- use_module(library(unix)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(dif)).

/** <module> Test unit for toplevel replies

This module tests the way in which  the toplevel replies to queries. The
implementation is based on pipes, which   are  provided by library(unix)
from the clib package. This means that   this  test can only be executed
after library(unix) has been compiled.
*/

test_answer :-
	run_tests([ answer
		  ]).

%%	toplevel_answer(+GoalAtom, -Answer:string) is det.
%
%	Run GoalAtom in a seperate thread and   catch the output that is
%	produces by Prolog.

toplevel_answer(GoalAtom, Answer) :-
	pipe(Read, Write),
	pipe(Read2, Write2),
	thread_create(send_bindings(Read2, Write), Id, []),
	format(Write2, '(~w), !.~n', [GoalAtom]),
	close(Write2),
	read_string(Read, _, Answer),
	close(Read),
	thread_join(Id, Reply),
	assertion(Reply == true).

send_bindings(In, Out) :-
	set_prolog_IO(In, Out, Out),
	set_prolog_flag(toplevel_residue_vars, true),
	prolog,
	close(In),
	close(Out).

%%	test_answer(+Query, -OkReplies) is semidet.
%
%	True if Query produces one of the outputs in OkReplies.

test_answer(QueryAtom, Replies) :-
	toplevel_answer(QueryAtom, Output),
	debug(test_answer, 'Got: ~q', [Output]),
	term_string(Written, Output,
		    [ variable_names(OutBindings),
		      comments(OutComments)
		    ]),
	member(ReplyAtom, Replies),
	term_string(Reply, ReplyAtom,
		    [ variable_names(ReplyBindings0),
		      comments(ReplyComments)
		    ]),
	maplist(anon_binding, OutBindings, ReplyBindings0, ReplyBindings),
	(   debug(test_answer, 'Comments: ~p vs ~p',
		  [OutComments, ReplyComments]),
	    maplist(compare_comment, OutComments, ReplyComments)
	->  true
	;   debug(test_answer, '~p', [ OutComments \= ReplyComments ]),
	    fail
	),
	(   Written+OutBindings =@= Reply+ReplyBindings
	->  true
	;   debug(test_answer, '~q',
		  [ Written+OutBindings \=@= Reply+ReplyBindings ]),
	    fail
	).

anon_binding(Name=_, GName=Var, Name=Var) :-
	sub_atom(GName, 0, _, _, '_'),
	sub_atom(GName, 1, 1, _, C),
	char_type(C, digit), !.
anon_binding(_, Binding, Binding).

compare_comment(_-C, _-C).

hidden :-
	dif(_X, a).

:- begin_tests(answer, [sto(rational_trees)]).

test(simple, true) :-
	test_answer('A=1', ['A=1']).
test(simple, true) :-
	test_answer('A=1, B=2', ['A=1, B=2']).
test(separated, true) :-
	test_answer('X = 2, Y = 1, Z = 2', ['X = Z, Z = 2, Y = 1']).
test(same, true) :-
	test_answer('A=1, B=1', ['A=B, B=1']).
test(same, true) :-
	test_answer('A=a(B), B=A', ['A=B, B=a(B)']).
test(cycle, true) :-
	test_answer('A=a(A)', ['A=a(A)']).
test(cycle, true) :-
	test_answer('A=a(A), B=a(a(B))', ['A=B, B=a(a(B))']).
test(double_cycle, true) :-
	test_answer('X = s(X,Y), Y = s(X,X)',
		    [ 'X = Y, Y = s(_S1, _S1), % where
		          _S1 = s(_S1, s(_S1, _S1))'
		    ]).
test(freeze, true) :-
	test_answer('freeze(X, writeln(X))', ['freeze(X, writeln(X))']).
test(hidden1, true) :-
	test_answer('test_answer:hidden',
		    [ '% with pending residual goals
		      dif(_1,a)'
		    ]).
test(hidden2, true) :-
	test_answer('test_answer:hidden, A = a',
		    [ 'A = a,
		      % with pending residual goals
		      dif(_1,a)'
		    ]).
test(hidden3, true) :-
	test_answer('test_answer:hidden, A = a, dif(B, b)',
		    [ 'A = a,
		      dif(B, b),
		      % with pending residual goals
		      dif(_1,a)'
		    ]).

:- end_tests(answer).

:- else.				% No foreign(unix) found

test_answer :-
	format(user_error, 'Skipped toplevel answer tests; requires clib~n', []).

test_answer(_QueryAtom, _Replies).	% satisfy exports

:- endif.


