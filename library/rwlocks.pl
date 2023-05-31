/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(rwlocks,
	  [ with_rwlock/3,	% +LockId, :Goal, +Mode
	    with_rwlock/4	% +LockId, :Goal, +ModeSpec, +Options
	  ]).
:- autoload(library(error), [must_be/2, type_error/2]).
:- autoload(library(lists), [member/2]).
:- autoload(library(option), [option/2]).

:- meta_predicate
       with_rwlock(+,0,+),
       with_rwlock(+,0,+,+).

/** <module> Read/write locks

This library implements  _read/write_  locks   on  top  of with_mutex/2.
_Read/write_ locks are synchronization objects   that allow for multiple
readers or a single writer to be active.
*/

%!  with_rwlock(+LockId, :Goal, +ModeSpec).
%!  with_rwlock(+LockId, :Goal, +ModeSpec, +Options).
%
%   Run Goal, synchronized with LockId in   ModeSpec. ModeSpec is one of
%   `read`, `write`, `read(Priority)` or  `write(Priority)`. The default
%   `read` priority is 100 and  the   default  `write`  priority is 200.
%   These values prioritize writers over readers. Goal may start if
%
%     - If there is no goal waiting with higher priority __and__
%       - It is a read goal and no write goal is running __or__
%       - It is a write goal and no other goal is running.
%
%  If  Goal  may  not  start   immediately    the   thread  waits  using
%  thread_wait/2. The Options `timeout`  and   `deadline`  are passed to
%  thread_wait/2. If the time limit is exceeded an exception is raised.
%
%  _Read/write_ locks are widely critized for   their  poor behaviour on
%  several  workloads.  They  perform  well   in  scenarios  where  read
%  operations take long, and write operations   are  relatively fast and
%  occur  only  occasionally.   _Transactions_,    as   implemented   by
%  transaction/1,2 are often a better alternative.
%
%  This predicate uses a normal mutex and a flag with the same name. See
%  with_mutex/2 and flag/3. Neither the  mutex   nor  the flag should be
%  used directly.
%
%  @throws time_limit_exceeded(rwlock) if  a  timeout   or  deadline  is
%  specified and this is exceeded.
%
%  @bug The current implementation is written   in Prolog and comes with
%  significant overhead. It is intended to synchronize slow operations.

with_rwlock(LockId, Goal, ModeSpec) :-
    with_rwlock(LockId, Goal, ModeSpec, []).

with_rwlock(LockId, Goal, ModeSpec, Options) :-
    must_be(atom, LockId),
    must_be(callable, Goal),
    rwmode(ModeSpec, Mode, Pri),

    flag(LockId, Id, Id+1),
    (   with_mutex(LockId, may_start(LockId, Mode, Pri, Id))
    ->  true
    ;   wait(LockId, Mode, Pri, Id, Options)
    ),
    call_cleanup(once(Goal),
		 with_mutex(LockId, completed(LockId, Id))).


rwmode(read,  Mode,  Pri) =>
    Mode = read,
    Pri = 100.
rwmode(write, Mode, Pri) =>
    Mode = write,
    Pri = 200.
rwmode(read(X), Mode, Pri), number(X) =>
    Mode = read,
    Pri = X.
rwmode(write(X), Mode, Pri), number(X) =>
    Mode = write,
    Pri = X.
rwmode(Mode, _, _) =>
    type_error(rwlock_mode, Mode).

:- dynamic
       (   access/3,		% LockId, Mode, Id
	   waiting/4		% LockId, Mode, Pri, Id
       ) as volatile.

may_start(LockId, _Mode, Pri, _) :-
    waiting(LockId, _, WPri, _),
    WPri > Pri,
    !,
    fail.
may_start(LockId, read, _Pri, Id) :-
    \+ access(LockId, write, _),
    !,
    asserta(access(LockId, read, Id)).
may_start(LockId, write, _Pri, Id) :-
    \+ access(LockId, _, _),
    !,
    asserta(access(LockId, write, Id)).

wait(LockId, Mode, Pri, Id, Options) :-
    deadline_option(DOption, Options),
    assertz(waiting(LockId, Mode, Pri, Id)),
    (   thread_wait(\+ waiting(LockId, _, _, Id),
		    [ wait_preds([waiting/4])
		    | DOption
		    ])
    ->  true
    ;   retractall(waiting(LockId, _, _, Id)),
	throw(time_limit_exceeded(rwlock))
    ).

deadline_option([deadline(Time)], Options) :-
    (   option(deadline(Time), Options)
    ->  true
    ;   option(timeout(Rel), Options)
    ->  get_time(Now),
	Time is Now+Rel
    ),
    !.
deadline_option([], _).

completed(LockId, Id) :-
    retractall(access(LockId, _, Id)),
    with_mutex(LockId, wakeup(LockId)).

wakeup(LockId) :-
    findall(t(Mode,Pri,Id), waiting(LockId, Mode, Pri, Id), Triples),
    sort(2, >=, Triples, Sorted),
    member(t(Mode,Pri,Id), Sorted),
    (   Mode == write
    ->  \+ access(LockId, _, _)
    ;   \+ access(LockId, _, _)
    ), !,
    retractall(waiting(LockId, _, _, Id)).
wakeup(_).
