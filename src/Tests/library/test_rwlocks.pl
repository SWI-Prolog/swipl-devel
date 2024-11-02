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

% Declare dynamic predicates for thread safety
:- thread_local
    started/1,                % Used in priority_handling test
    read_done/1,             % Used in concurrent_reads test
    read_attempted/1.        % Used in write_blocks_reads test

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
    retractall(read_done(_)),
    message_queue_create(Queue),
    thread_create(with_rwlock(test_lock,
                              (thread_self(Id),
                               assertz(read_done(Id)),
                               % Wait for other thread to also get read lock
                               thread_get_message(Queue, other_done),
                               thread_send_message(Queue, done)),
                              read,
                              [timeout(1)]),  % Timeout to prevent deadlock
                 T1, []),
    thread_create(with_rwlock(test_lock,
                              (thread_self(Id),
                               assertz(read_done(Id)),
                               % Signal first thread we're done
                               thread_send_message(Queue, other_done),
                               thread_send_message(Queue, done)),
                              read,
                              [timeout(1)]),  % Timeout to prevent deadlock
                 T2, []),
    % Wait for both threads to complete
    thread_get_message(Queue, done),
    thread_get_message(Queue, done),
    maplist(thread_join, [T1, T2]),
    % Verify both threads completed
    findall(Id, read_done(Id), Threads),
    length(Threads, Count),
    assertion(Count == 2),
    message_queue_destroy(Queue).

test(write_blocks_reads, [thread]) :-
    retractall(read_attempted(_)),
    message_queue_create(Queue),
    % Start write thread and wait for it to acquire the lock
    thread_create(with_rwlock(test_lock,
                              (thread_send_message(Queue, write_ready),
                               thread_get_message(Queue, continue)),
                              write),
                 WriteThread, []),
    thread_get_message(Queue, write_ready),
    % Try to get read lock while write lock is held
    thread_create((   catch(with_rwlock(test_lock,
                                        (thread_self(Id),
                                         assertz(read_attempted(Id))),
                                        read,
                                        [timeout(0.5)]),
                            time_limit_exceeded(rwlock),
                            true)
                  ->  thread_send_message(Queue, read_blocked)
                  ;   thread_send_message(Queue, read_succeeded)
                  ),
                 ReadThread, []),
    % Wait for read attempt to complete
    thread_get_message(Queue, ReadResult),
    % Allow write thread to finish
    thread_send_message(Queue, continue),
    % Clean up threads
    maplist(thread_join, [WriteThread, ReadThread]),
    % Verify read was blocked (no read_attempted facts should exist)
    findall(Id, read_attempted(Id), ReadAttempts),
    length(ReadAttempts, AttemptCount),
    debug(test_rwlocks, 'Read result: ~w, Read attempts: ~w', [ReadResult, AttemptCount]),
    assertion(ReadResult == read_blocked),
    assertion(AttemptCount == 0),
    message_queue_destroy(Queue).

test(invalid_mode, [throws(error(type_error(rwlock_mode, invalid), _))]) :-
    with_rwlock(test_lock, true, invalid).

:- end_tests(rwlocks).
