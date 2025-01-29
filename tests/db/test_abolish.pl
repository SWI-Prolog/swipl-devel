/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

:- module(test_abolish, [test_abolish/0]).
:- use_module(library(plunit)).

/** <module> Test abolish with clause garbage collection

The predicate abolish/1 cannot really delete a   predicate  as it may be
active in choicepoints or other threads. So, it deletes the clauses, and
resets the predicate properties to their   default. This normally leaves
the predicate in a "dirty" state.

Before  3716d10f2117d14f09a5b6a79fb715facec857a4,  there  was    a  race
condition that could result in a clause  list to be considered clean and
as a result the predicate considered to   be defined, preventing the use
of assert/retract on them.

This module runs a test for   this,  copying the assert/abolish sequence
from Logtalk. The nmber of iterations is rather low. For stress testing,
a much higher value such as 100M iterations should be used.
*/

test_abolish :-
    run_tests([ abolish_cgc
	      ]).

:- begin_tests(abolish_cgc).

run_abolish_test(N) :-
    forall(between(1, N, _), create_delete).

create_delete :-
    asserta('$xpto#0._dcl'(_24976, _24978, _24980, _24982)),
    asserta('$xpto#0._dcl'(_25800, _25802, _25804, _25806, _25808, _25810)),
    asserta('$xpto#0._def'(_26628, _26630, _26632)),
    asserta('$xpto#0._def'(_27450, _27452, _27454, _27456, _27458)),
    asserta('$xpto#0._super'(_28276, _28278, _28280, _28282, _28284)),
    asserta('$xpto#0._dcl'(_29102, _29104, _29106, _29108, _29110, _29112)),
    asserta('$xpto#0._def'(_29930, _29932, _29934, _29936, _29938)),
    asserta('$xpto#0._ddef'(_30756, _30758, _30760)),
    asserta('$xpto#0._alias'(_31578, _31580, _31582)),
    abolish('$xpto#0._dcl'/4),
    abolish('$xpto#0._dcl'/6),
    abolish('$xpto#0._def'/3),
    abolish('$xpto#0._def'/5),
    abolish('$xpto#0._super'/5),
    abolish('$xpto#0._dcl'/6),
    abolish('$xpto#0._def'/5),
    abolish('$xpto#0._ddcl'/2),
    abolish('$xpto#0._ddef'/3),
    abolish('$xpto#0._alias'/3).

test(abolish_cgc) :-
    run_abolish_test(50 000).

:- end_tests(abolish_cgc).
