:- module(test_shared_dynamic,
	  [ test_shared_dynamic/0,
	    test_shared_dynamic/1
	  ]).

:- dynamic(foo/1).
:- dynamic(failed/1).

test_shared_dynamic :-
	test_shared_dynamic(50000).

test_shared_dynamic(N) :-
	message_queue_create(Queue),
        thread_create(producer(Queue, N), Producer, []),
        thread_create(consumer(Queue), Consumer, []),
	thread_join(Producer, PStat),
	thread_join(Consumer, CStat),
	message_queue_destroy(Queue),
	PStat == true,
	CStat == true,
	\+ failed(_).

producer(Queue, N) :-
	(   between(1, N, X),
	    assert(foo(X)),
	    thread_send_message(Queue, X),
	    fail
        ;   thread_send_message(Queue, done)
	).

consumer(Queue) :-
        repeat,
        thread_get_message(Queue, X),
	(   X == done
	->  !
	;   (   retract(foo(X))
	    ->  true
	    ;   writeln(failed(X)),
		assert(failed(X))
	    ),
	    fail
	).

