:- module(th_resource,
	  [ th_resource/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test to see what happens if we create threads exhausting virtual memory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

th_resource :-
	length(L, 20),
	create_threads(L),
	kiss_threads(L),
	join_threads(L).

create_threads([]).
create_threads([H|T]) :-
	thread_create(thread_get_message(_), H,
		      [ local(infinite),
			global(infinite),
			trail(infinite)
		      ]),
	create_threads(T).

kiss_threads([]).
kiss_threads([H|T]) :-
	catch(thread_send_message(H, done), _, true),
	kiss_threads(T).

join_threads([]).
join_threads([H|T]) :-
	thread_join(H, Status),
	check_status(Status),
	join_threads(T).

check_status(true).
check_status(exception(error(resource_error(virtual_memory), _))).
