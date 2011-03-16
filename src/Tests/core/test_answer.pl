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
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(library(lists)).

/** <module> Test unit for toplevel replies

This module tests the way in which  the toplevel replies to queries. The
implementation is based on pipes, which   are  provided by library(unix)
from the clib package. This means that   this  test can only be executed
after library(unix) has been compiled.
*/

test_answer :-
	run_tests([ answer
		  ]).

%%	toplevel_answer(+GoalAtom, -AnswerAtom) is det.
%
%	Run GoalAtom in a seperate thread and   catch the output that is
%	produces by Prolog.

toplevel_answer(GoalAtom, Atom) :-
	pipe(Read, Write),
	pipe(Read2, Write2),
	thread_create(send_bindings(Read2, Write), Id, []),
	format(Write2, '(~w), !.~n', [GoalAtom]),
	close(Write2),
	read_stream_to_codes(Read, Codes),
	atom_codes(Atom, Codes),
	close(Read),
	thread_join(Id, Reply),
	assertion(Reply == true).

send_bindings(In, Out) :-
	set_prolog_IO(In, Out, Out),
	prolog,
	close(In),
	close(Out).

%	test_answer(+Query, -OkReplies) is semidet.
%
%	True if Query produces one of the outputs in OkReplies.

test_answer(QueryAtom, Replies) :-
	toplevel_answer(QueryAtom, Output),
	atom_to_term(Output, Written, OutBindings),
	member(ReplyAtom, Replies),
	atom_to_term(ReplyAtom, Reply, ReplyBindings),
	Written+OutBindings =@= Reply+ReplyBindings.

:- begin_tests(answer).

test(simple, true) :-
	test_answer('A=1', ['A=1']).
test(simple, true) :-
	test_answer('A=1, B=2', ['A=1, B=2']).
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

:- end_tests(answer).

:- else.				% No foreign(unix) found

test_answer :-
	format(user_error, 'Skipped toplevel answer tests; requires clib~n', []).

:- endif.


