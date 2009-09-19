:- module(queue_resource,
	  [ queue_resource/0
	  ]).
:- use_module(library(lists)).

%%	queue_resource
%
%	Create a thread with limited resources and   send it a big term.
%	If it tries to retrieve  this  term   the  thread  must die on a
%	resource error. Note that a SWI-Prolog   list takes 12 bytes per
%	cell (32-bit)
%
%	@tbd	Express stack limits in cells!

queue_resource :-
	thread_self(Me),
	thread_create(client(Me), Id, [ global(100) ]),
	thread_get_message(ready(Limit)),
	Length is (Limit+10000)//12,
	numlist(1, Length, L),
	thread_send_message(Id, L),
	thread_join(Id, Status),
	(   Status == exception(error(resource_error(stack), global))
	->  true
	;   format(user_error,
		   'ERROR: queue_resource/0: wrong status: ~p~n', [Status]),
	    fail
	).

client(Main) :-
	statistics(globallimit, Limit),
	thread_send_message(Main, ready(Limit)),
	thread_get_message(_Msg).
