/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(tcp_test,
	  [ server/1,			% +Port
	    client/1			% +Address
	  ]).
:- use_module(library(socket)).
:- use_module(streampool).

server(Port) :-
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	tcp_open_socket(Socket, In, _Out),
	add_stream_to_pool(In, accept(Socket)),
	stream_pool_main_loop.

accept(Socket) :-
	tcp_accept(Socket, Slave, Peer),
	print_message(informational, connect(Peer)),
	tcp_open_socket(Slave, In, Out),
	add_stream_to_pool(In, client(In, Out, Peer)).

client(In, Out, Peer) :-
	read(In, Term),
	(   Term == end_of_file
	->  print_message(informational, close(Peer)),
	    close(In),
	    close(Out)
	;   (   catch(action(Term, In, Out), E, true)
	    ->	(   var(E)
		->  true
		;   tcp_send(Out, exception(E))
		)
	    ;	tcp_send(Out, no)
	    )
	).

		 /*******************************
		 *	      ACTION		*
		 *******************************/

action(echo(X), _In, Out) :-
	tcp_send(Out, X).
action(wait(X), _In, Out) :-
	sleep(X),
	tcp_send(Out, yes).
action(slow_read, In, Out) :-
	sleep(5),
	read(In, Term),
	tcp_send(Out, Term).
	

		 /*******************************
		 *	    CLIENT SIDE		*
		 *******************************/

:- dynamic
	client/2.

client(Address) :-
	tcp_socket(S),
	tcp_connect(S, Address),
	tcp_open_socket(S, In, Out),
	asserta(client(In, Out)),
	test,
	retract(client(In, Out)),
	close(Out),
	close(In).

echo(echo-1) :-
	X = 'Hello World',
	client(In, Out),
	tcp_send(Out, echo(X)),
	tcp_reply(In, X).
echo(echo-2) :-
	findall(A, between(0, 100000, A), X),
	client(In, Out),
	tcp_send(Out, echo(X)),
	tcp_reply(In, X).

slow(slow-1) :-
	client(In, Out),
	tcp_send(Out, wait(5)),
	tcp_reply(In, yes).
slow(slow-1) :-
	client(In, Out),
	tcp_send(Out, slow_read),
	findall(A, between(0, 100000, A), X),
	tcp_send(Out, X),
	tcp_reply(In, X).


		 /*******************************
		 *	      UTIL		*
		 *******************************/

tcp_send(Out, Term) :-
	format(Out, '~q.~n', [Term]),
	flush_output(Out).

tcp_reply(In, Reply) :-
	read(In, Term),
	reply(Term, In, Reply).

reply(exception(E), _, _) :-
	throw(E).
reply(T, _, T).

		 /*******************************
		 *        TEST MAIN-LOOP	*
		 *******************************/

testset(echo).
testset(slow).

:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	forall(testset(Set), runtest(Set)),
	report_blocked,
	report_failed.

report_blocked :-
	findall(Head-Reason, blocked(Head, Reason), L),
	(   L \== []
        ->  format('~nThe following tests are blocked:~n', []),
	    (	member(Head-Reason, L),
		format('    ~p~t~40|~w~n', [Head, Reason]),
		fail
	    ;	true
	    )
        ;   true
	).
report_failed :-
	findall(X, failed(X), L),
	length(L, Len),
	(   Len > 0
        ->  format('~n*** ~w tests failed ***~n', [Len]),
	    fail
        ;   format('~nAll tests passed~n', [])
	).

runtest(Name) :-
	format('Running test set "~w" ', [Name]),
	flush,
	functor(Head, Name, 1),
	nth_clause(Head, _N, R),
	clause(Head, _, R),
	(   catch(Head, Except, true)
	->  (   var(Except)
	    ->  put(.), flush
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(Head, Reason)),
		put(!), flush
	    ;   test_failed(R, Except)
	    )
	;   test_failed(R, fail)
	),
	fail.
runtest(_) :-
	format(' done.~n').
	
test_failed(R, Except) :-
	clause(Head, _, R),
	functor(Head, Name, 1),
	arg(1, Head, TestName),
	clause_property(R, line_count(Line)),
	clause_property(R, file(File)),
	(   Except == failed
	->  format('~N~w:~d: Test ~w(~w) failed~n',
		   [File, Line, Name, TestName])
	;   message_to_string(Except, Error),
	    format('~N~w:~d: Test ~w(~w):~n~t~8|ERROR: ~w~n',
		   [File, Line, Name, TestName, Error])
	),
	assert(failed(Head)).

blocked(Reason) :-
	throw(blocked(Reason)).

