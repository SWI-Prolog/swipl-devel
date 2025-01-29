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
:- use_module(library(apply)).

% Declare dynamic predicates for thread safety
:- thread_local
    started/1,                % Used in priority_handling test
    read_done/1,             % Used in concurrent_reads test
    read_attempted/1.        % Used in write_blocks_reads test

test_rwlocks :-
    run_tests([ rwlocks
	      ]).

:- begin_tests(rwlocks).

% Helper predicate to clean up any leftover rwlock data
cleanup_rwlock(LockId) :-
    (   rwlocks:access(LockId, _, _)
    ->  format(user_error, 'rwlocks:access/3 not cleaned for ~p~n', [LockId]),
        retractall(rwlocks:access(LockId, _, _))
    ;   true
    ),
    (   rwlocks:waiting(LockId, _, _, _)
    ->  format(user_error, 'rwlocks:waiting/4 not cleaned for ~p~n', [LockId]),
        retractall(rwlocks:waiting(LockId, _, _, _))
    ;   true
    ).

% Helper predicate to clean up thread-local data
cleanup_thread_data :-
    retractall(started(_)),
    retractall(read_done(_)),
    retractall(read_attempted(_)).

test(basic_write_lock, [cleanup(cleanup_rwlock(test_lock))]) :-
    with_rwlock(test_lock, true, write),
    assertion(\+ rwlocks:access(test_lock, _, _)).

test(basic_read_lock, [cleanup(cleanup_rwlock(test_lock))]) :-
    with_rwlock(test_lock, true, read),
    assertion(\+ rwlocks:access(test_lock, _, _)).

test(concurrent_reads, [setup(cleanup_thread_data), cleanup(cleanup_rwlock(test_lock))]) :-
    message_queue_create(Queue),
    thread_create(
        (   catch(
                with_rwlock(test_lock,
                           (thread_self(Id),
                            assertz(read_done(Id)),
                            thread_send_message(Queue, done)),
                           read,
                           [timeout(1)]),
                time_limit_exceeded(rwlock),
                thread_send_message(Queue, timeout))
        ),
        T1,
        []
    ),
    thread_create(
        (   catch(
                with_rwlock(test_lock,
                           (thread_self(Id),
                            assertz(read_done(Id)),
                            thread_send_message(Queue, done)),
                           read,
                           [timeout(1)]),
                time_limit_exceeded(rwlock),
                thread_send_message(Queue, timeout))
        ),
        T2
    ),
    % Wait for both threads to complete
    thread_get_message(Queue, Msg1),
    thread_get_message(Queue, Msg2),
    maplist(thread_join, [T1, T2]),
    message_queue_destroy(Queue),
    % Verify both threads completed successfully
    assertion(Msg1-Msg2 == done-done),
    % Verify both threads completed
    findall(Id, read_done(Id), Threads),
    assertion(Threads = [_,_]).

test(write_blocks_reads, [setup(cleanup_thread_data), cleanup(cleanup_rwlock(test_lock))]) :-
    message_queue_create(Queue),
    % Start write thread
    thread_create(
        (   catch(
                with_rwlock(test_lock,
                           (thread_send_message(Queue, write_ready),
                            thread_get_message(Queue, continue)),
                           write,
                           [timeout(2)]),
                time_limit_exceeded(rwlock),
                thread_send_message(Queue, write_timeout))
        ),
        WriteThread
    ),
    thread_get_message(Queue, WriteStatus),
    (   WriteStatus == write_ready
    ->  % Try to get read lock while write lock is held
        thread_create(
            (   catch(
                    with_rwlock(test_lock,
                               (thread_self(Id),
                                assertz(read_attempted(Id))),
                               read,
                               [timeout(0.5)]),
                    time_limit_exceeded(rwlock),
                    true)
            ->  thread_send_message(Queue, read_blocked)
            ;   thread_send_message(Queue, read_succeeded)
            ),
            ReadThread
        ),
        % Wait for read attempt to complete
        thread_get_message(Queue, ReadResult),
        % Allow write thread to finish
        thread_send_message(Queue, continue),
        % Clean up threads
        maplist(thread_join, [WriteThread, ReadThread]),
        message_queue_destroy(Queue),
        % Verify read was blocked
        assertion(\+ read_attempted(Id)),
        assertion(ReadResult == read_blocked)
    ;   % Write thread timed out, clean up and fail
        message_queue_destroy(Queue),
        fail
    ).

test(invalid_mode, [throws(error(type_error(rwlock_mode, invalid), _))]) :-
    with_rwlock(test_lock, true, invalid).

:- end_tests(rwlocks).
