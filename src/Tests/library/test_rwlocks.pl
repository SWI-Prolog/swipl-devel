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

:- asserta(user:file_search_path(library, './library')).
:- use_module(library(rwlocks)).

:- begin_tests(rwlocks).                                                                                                                                          
                                                                                                                                                                      
    test(priority_handling) :-
        retractall(started(_)),
        with_rwlock(test_lock, 
                    (assertz(started(write)), 
                    sleep(1), 
                    writeln('Write operation completed')), 
                    write),
        with_rwlock(test_lock, 
                    (assertz(started(read)), 
                    sleep(2), 
                    writeln('Read operation completed')), 
                    read),
        findall(X, started(X), Started),
        writeln(Started),
        assertion(Started == [write, read]).                                                                                                                    
                                                                                                                                                                      
    test(timeout_handling, [throws(time_limit_exceeded(rwlock))]) :-
        thread_create(with_rwlock(test_lock, sleep(2), write), WriteThread, []),
        sleep(0.1),  % Give some time for the write lock to be acquired
        catch(with_rwlock(test_lock, true, write, [timeout(0.5)]),
            time_limit_exceeded(rwlock),
            ( thread_signal(WriteThread, throw(abort_timeout_test)),
                retract_rwlock_data(test_lock),
                rethrow(time_limit_exceeded(rwlock))
            )),
        thread_join(WriteThread, _).

    % Helper predicate to clean up any leftover rwlock data
    retract_rwlock_data(LockId) :-
        retractall(access(LockId, _, _)),
        retractall(waiting(LockId, _, _, _)).

    % Helper predicate to re-throw the exception
    rethrow(Exception) :-
        throw(Exception).                                                                                                          
                                                                                                                                                                      
    test(concurrent_reads) :-
        flag(concurrent_read_counter, _, 0),
        thread_create(with_rwlock(test_lock, 
                                (flag(concurrent_read_counter, X, X+1), 
                                sleep(0.5)), 
                                read),
                    T1, []),
        thread_create(with_rwlock(test_lock, 
                                (flag(concurrent_read_counter, Y, Y+1), 
                                sleep(0.5)), 
                                read),
                    T2, []),
        thread_join(T1),
        thread_join(T2),
        flag(concurrent_read_counter, FinalCount, FinalCount),
        assertion(FinalCount == 2).                                                                                                                                  
                                                                                                                                                                      
    test(write_blocks_reads) :-
        flag(read_counter, _, 0),
        thread_create(with_rwlock(test_lock, sleep(1), write), WriteThread, []),
        sleep(0.1),  % Give some time for the write lock to be acquired
        (   catch(with_rwlock(test_lock, 
                            (flag(read_counter, _, 1), fail),  % Always fail after incrementing
                            read, 
                            [timeout(0.5)]),
                time_limit_exceeded(rwlock),
                true)
        ->  true
        ;   fail  % Fail if the read operation succeeded
        ),
        thread_join(WriteThread),
        flag(read_counter, ReadCount, ReadCount),
        assertion(ReadCount == 0).                                                                                                      
                                                                                                                                                                      
    test(invalid_mode, [throws(error(type_error(rwlock_mode, invalid), _))]) :-                                                                                       
        with_rwlock(test_lock, true, invalid).                                                                                                                        
                                                                                                                                                                      
:- end_tests(rwlocks).  