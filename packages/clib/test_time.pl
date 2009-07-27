:- module(test_time,
	  [ test_time/0,
	    list_alarms/0
	  ]).

:- use_module(library(debug)).
:- use_module(library(plunit)).

:- asserta(file_search_path(foreign, '.')).
:- [time].

dbg :-
	time:time_debug(1).

test_time :-
	run_tests([ time
		  ]).

:- begin_tests(time).

test(bg) :-
	bg(4).
test(flood) :-
	flood_test.

:- end_tests(time).


		 /*******************************
		 *     MULTI-THREAD TIMEOUT	*
		 *******************************/

bg(N) :-
	findall(Id, (between(1, N, _),
		     thread_create(t(100, 0.05), Id, [])),
		IDS),
	join_all(IDS).

join_all([]).
join_all([H|T]) :-
	thread_join(H, Status),
	assertion(Status == true),
	join_all(T).


t(N, Time) :-
	thread_create(worker, Worker, []),
	t(N, Time, Worker),
	thread_send_message(Worker, done),
	thread_join(Worker, Status),
	assertion(Status == true).

t(0, _, _) :- !.
t(N, Time, Worker) :-
	thread_self(Me),
	thread_send_message(Worker, work(Time, Me)),
	thread_get_message(done(E)),
	assertion(E == time_limit_exceeded),
	N2 is N - 1,
	t(N2, Time, Worker).

worker :-
	thread_get_message(Msg),
	(   Msg = work(N, Sender)
	->  thread_self(Me),
	    debug(work, '[~w] Start working ~w sec', [Me, N]),
	    r(N, E),
	    thread_send_message(Sender, done(E)),
	    worker
	;   true
	).

r(N, E) :-
	catch(call_with_time_limit(N, (repeat, fail)),
	      E, true).

w(N) :-
	alarm(N, writeln(hello), Id),
	writeln(Id).


		 /*******************************
		 *	     FLOODING		*
		 *******************************/

:- dynamic
	x/1.

flood_test :-
	retractall(x(_)),
	forall(between(1, 100, X),
	       alarm(1, got(X), _,
		     [ remove(true)
		     ])),
	get_time(Now),
	repeat,
	   get_time(End),
	   End - Now > 2, !,
        (   forall(between(1, 100, X), x(X))
	->  retractall(x(_))
	;   forall(between(1, 100, X),
		   (   x(X)
		   ->  true
		   ;   format('Failed: ~D~n', [X])
		   ))
	).

got(X) :-
	assert(x(X)).


		 /*******************************
		 *	      DEBUG		*
		 *******************************/

list_alarms :-
	(   current_alarm(At, Callable, Id, Status),
	    format('~p ~p ~p ~p~n', [At, Callable, Id, Status]),
	    fail
	;   true
	).
