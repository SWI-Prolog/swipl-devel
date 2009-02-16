:- module(queue_resource,
	  [ queue_resource/0
	  ]).
:- use_module(library(lists)).

%%	queue_resource
%
%	Create a thread with limited resources and   send it a big term.
%	If it tries to retrieve  this  term   the  thread  must die on a
%	resource error. Note that  on  32-bit   system  the  list  takes
%	12*10,000 bytes is 120K and  should   thus  overflow  the client
%	thread.

queue_resource :-
	thread_self(Me),
	thread_create(client(Me), Id, [ global(100) ]),
	thread_get_message(ready),
	numlist(1, 10000, L),
	thread_send_message(Id, L),
	thread_join(Id, Status),
	(   Status == exception(error(resource_error(stack), global))
	->  true
	;   format(user_error,
		   'ERROR: queue_resource/0: wrong status: Status~n', []),
	    fail
	).

client(Main) :-
	thread_send_message(Main, ready),
	thread_get_message(_Msg).
