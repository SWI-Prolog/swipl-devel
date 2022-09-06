/*  Part of SWI-Prolog

    Author:        Keri Harris
    E-mail:        keri@gentoo.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
                         VU University Amsterdam
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

:- module(test_cgc_1,
	  [ test_cgc_1/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test calls assert/1 + retract/1  repeatedly in a loosely concurrent
pattern: when  one thread is  cleaning up by  calling retract/1, another
thread is calling assert/1.

With a  little luck,  or  the  addition  of  a well-placed  usleep()  in
pl-index.c:HashDefinition(),  it's possible  that retract/1  may mark  a
just-created index as "dirty" even though the index does not include the
clause being retracted:

thread1                            thread2
----------------------------------------------------------------------
assert/1: new clause 1
 - addClauseToIndex()
   clause: 1, index: 1, is_list: FALSE
 - addClauseToBucket()
   clause: 1, key: 1

                                   assert/1: new clause 2
                                   - new (empty) index created
                                     index: 2, is_list: TRUE
retract/1: clause 1
  - set CL_ERASED
                                   - considering clause: 1 (CL_ERASED)
                                   - considering clause: 2
                                   - addClauseToIndex()
                                     clause 2, index 2, is_list: TRUE
                                   - addClauseToBucket()
                                     clause 2, key: 1
  - deleteActiveClauseFromIndexes()
    index: 2 selected
  - deleteActiveClauseFromIndex()
    key: 1, index: 2, is_list: 1
  - deleteActiveClauseFromBucket()
    clause: 1 not present in index: 2
    erased_clauses++
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test_cgc_1 :-
        current_prolog_flag(threads, true),
        !,
        chained_detached_goal(1, 1000).
test_cgc_1.

chained_detached_goal(I, Max) :-
        (   I >= Max
        ->  true
        ;   II is I + 1,
            call_detached(chained_detached_goal(II, Max))
        ).

:-dynamic
        foo/2.

:- meta_predicate call_detached(0).
call_detached(Goal) :-
	ignore(foo(_, foo(bar))),
	gensym('thread-', ThreadId),
	setup_call_catcher_cleanup(
		assert(foo(ThreadId, foo(bar))),
		thread_create(catch(ignore(detached_goal_2(Goal, ThreadId)), _, true),
		              _,
		              [alias(ThreadId),detached(true)]),
		exception(_),
		retractall(foo(ThreadId, _))).

:-meta_predicate
	detached_goal_2(0, +).

detached_goal_2(Goal, ThreadId) :-
	call(Goal),
	sleep(0.0001),
	retractall(foo(ThreadId, _)).
