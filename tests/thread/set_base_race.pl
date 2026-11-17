/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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
:- module(set_base_race,
	  [ set_base_race/0
	  ]).
:- use_module(library(plunit)).

/** <module> Set the import module while it is used

set_module/1 with base(Module) freed the cell holding the import module,
also if it did not change, while other threads walked the import modules
to resolve an undefined predicate.  This is also done for each (re)load
of a module file.  Run under AddressSanitizer to detect the access to
freed memory.
*/

set_base_race :-
	run_tests([ set_base_race
		  ]).

:- begin_tests(set_base_race,
               [ setup((current_prolog_flag(gc_code, Old),
                        set_prolog_flag(gc_code, module))),
                 cleanup(set_prolog_flag(gc_code, Old))
               ]).

test(base, Status == true-true) :-
	set_prolog_flag(set_base_race_m:unknown, fail),
	thread_create(set_base(20000), A, []),
	thread_create(call_undefined(20000), B, []),
	thread_join(A, SA),
	thread_join(B, SB),
	Status = SA-SB.

:- end_tests(set_base_race).

set_base(N) :-
	forall(between(1, N, _),
	       set_module(set_base_race_m:base(user))).

call_undefined(N) :-
	forall(between(1, N, I),
	       (   I mod 2 =:= 0
	       ->  \+ set_base_race_m:no_such_predicate
	       ;   set_base_race_m:atom_length(abc, 3)
	       )).
