/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(mutex_destroy_race,
	  [ mutex_destroy_race/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Destroy named mutexes while other threads use them

Some threads repeatedly create and destroy a named mutex while others
lock it and ask for its properties.  Threads that found the mutex used
to access it after it was freed.
*/

mutex_destroy_race :-
	findall(T, ( between(1, 3, _),
		     thread_create(churn(2000), T, [])
		   ), Churners),
	findall(T, ( between(1, 3, _),
		     thread_create(use(2000), T, [])
		   ), Users),
	append(Churners, Users, Threads),
	maplist(join, Threads).

join(T) :-
	thread_join(T, Status),
	assertion(Status == true).

churn(N) :-
	forall(between(1, N, _),
	       ( catch(mutex_create(_, [alias(mdr_mutex)]), _, true),
		 catch(with_mutex(mdr_mutex, true), _, true),
		 catch(mutex_destroy(mdr_mutex), _, true)
	       )).

use(N) :-
	forall(between(1, N, _),
	       ( catch(( mutex_lock(mdr_mutex),
			 mutex_unlock(mdr_mutex)
		       ), _, true),
		 catch(with_mutex(mdr_mutex, true), _, true),
		 catch(forall(mutex_property(_, status(_)), true), _, true),
		 catch(forall(mutex_property(mdr_mutex, _), true), _, true)
	       )).
