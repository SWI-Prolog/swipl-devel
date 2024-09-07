/*  Part of SWI-Prolog

    Author:        Younes A
    E-mail:        dev@younes.io

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

:- module(test_rwlocks,
	  [ test_rwlocks/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rwlocks)).
:- use_module(library(thread)).

test_rwlocks :-
	run_tests([ rwlocks
		  ]).

:- begin_tests(rwlocks).

test(priority_handling, [thread]) :-
    retractall(started(_)),
    message_queue_create(Queue),
    thread_create(with_rwlock(test_lock,
                              (assertz(started(write)),
                               thread_send_message(Queue, write_done)),
                              write),
                 WriteThread, []),
    thread_create(with_rwlock(test_lock,
                              (assertz(started(read)),
                               thread_send_message(Queue, read_done)),
                              read),
                 ReadThread, []),
    thread_get_message(Queue, write_done),
    thread_get_message(Queue, read_done),
    maplist(thread_join, [WriteThread, ReadThread]),
    findall(X, started(X), Started),
    debug(test_rwlocks, 'Started order: ~w', [Started]),
    assertion(Started == [write, read]),
    message_queue_destroy(Queue).

test(timeout_handling, [throws(time_limit_exceeded(rwlock))]) :-
    thread_create(with_rwlock(test_lock, thread_get_message(_), write), WriteThread, []),
    thread_get_message(WriteThread, ready),
    catch(with_rwlock(test_lock, true, write, [timeout(0.5)]),
          time_limit_exceeded(rwlock),
          ( thread_signal(WriteThread, throw(abort_timeout_test)),
            retract_rwlock_data(test_lock),
            rethrow(time_limit_exceeded(rwlock))
          )),
    thread_join(WriteThread, _).

% Helper predicate to clean up any leftover rwlock data
retract_rwlock_data(LockId) :-
    retractall(rwlocks:access(LockId, _, _)),
    retractall(rwlocks:waiting(LockId, _, _, _)).

% Helper predicate to re-throw the exception
rethrow(Exception) :-
    throw(Exception).

test(concurrent_reads, [thread]) :-
    flag(concurrent_read_counter, _, 0),
    message_queue_create(Queue),
    thread_create(with_rwlock(test_lock,
                              (flag(concurrent_read_counter, X, X+1),
                               thread_send_message(Queue, done)),
                              read),
                 T1, []),
    thread_create(with_rwlock(test_lock,
                              (flag(concurrent_read_counter, Y, Y+1),
                               thread_send_message(Queue, done)),
                              read),
                 T2, []),
    thread_get_message(Queue, done),
    thread_get_message(Queue, done),
    maplist(thread_join, [T1, T2]),
    flag(concurrent_read_counter, FinalCount, FinalCount),
    debug(test_rwlocks, 'Final concurrent read count: ~w', [FinalCount]),
    assertion(FinalCount == 2),
    message_queue_destroy(Queue).

test(write_blocks_reads, [thread]) :-
    flag(read_counter, _, 0),
    message_queue_create(Queue),
    thread_create(with_rwlock(test_lock,
                              thread_send_message(Queue, write_done),
                              write),
                 WriteThread, []),
    thread_create((   catch(with_rwlock(test_lock,
                                        (flag(read_counter, _, 1), fail),
                                        read,
                                        [timeout(0.5)]),
                            time_limit_exceeded(rwlock),
                            true)
                  ->  thread_send_message(Queue, read_blocked)
                  ;   thread_send_message(Queue, read_succeeded)
                  ),
                 ReadThread, []),
    thread_get_message(Queue, write_done),
    thread_get_message(Queue, ReadResult),
    maplist(thread_join, [WriteThread, ReadThread]),
    flag(read_counter, ReadCount, ReadCount),
    debug(test_rwlocks, 'Read result: ~w, Read count: ~w', [ReadResult, ReadCount]),
    assertion(ReadResult == read_blocked),
    assertion(ReadCount == 0),
    message_queue_destroy(Queue).

test(invalid_mode, [throws(error(type_error(rwlock_mode, invalid), _))]) :-
    with_rwlock(test_lock, true, invalid).

:- end_tests(rwlocks).
