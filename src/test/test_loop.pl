/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010, University of Amsterdam
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


:- set_prolog_flag(optimise, true).
:- set_prolog_flag(test_concurrent, true).
:- [test].

%%	test_loop
%
%	Run the normal test-suite as an   infinite loop. We use length/2
%	rather than repeat to shift  the  global   stack  a  bit in each
%	iteration and thus schedule GC/shifts differently.

test_loop :-
	set_prolog_flag(verbose, silent),
	forall(length(L,N),
	       ( format('RUN ~D~n', [N]),
		 test
	       )),
	is_list(L).			% avoid GC

%%	test_loops(+Threads)
%
%	Create a number of threads, each running test_loop/0

test_loops(N) :-
	forall(between(1, N, I),
	       (   atom_concat('tester_', I, Alias),
		   thread_create(test_loop, _, [alias(Alias)])
	       )).

%%	graph
%
%	Run  the  thread-monitor  in  separate    thread   to  see  nice
%	test-results.

graph :-
	pce_dispatch([]),
	prolog_ide(thread_monitor).

