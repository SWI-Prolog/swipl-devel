:- module(outofmem,
	  [ outofmem/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make too many threads for virtual  memory   and  see  whether the system
recovers gracefully.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

outofmem :-
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
	thread_send_message(H, done),
	kiss_threads(T).

join_threads([]).
join_threads([H|T]) :-
	thread_join(H, Status),
	check_status(Status),
	join_threads(T).

check_status(true).
check_status(exception(error(resource_error(virtual_memory), _))).
