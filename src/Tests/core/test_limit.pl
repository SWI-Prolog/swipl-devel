/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2014, University of Amsterdam
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

:- module(test_limit,
	  [ test_limit/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_limit :-
	run_tests([ program_space
		  ]).

:- begin_tests(program_space, []).

test(fetch) :-
	module_property(user, program_size(Size)),
	assertion(Size > 1000).
test(fetch, fail) :-
	module_property(user, program_space(_)).
test(too_low, error(permission_error(limit, program_space, user))) :-
	set_module(user:program_space(1000)).
test(retract, Size == Size0) :-
	set_module(program_space_limit:program_space(0)),
	dynamic(program_space_limit:test/0),
	module_property(program_space_limit, program_size(Size0)),
	assertz(program_space_limit:test),
	retract(program_space_limit:test),
	module_property(program_space_limit, program_size(Size)).
test(abolish, Size == Size0) :-
	set_module(program_space_limit:program_space(0)),
	module_property(program_space_limit, program_size(Size0)),
	assertz(program_space_limit:test),
	abolish(program_space_limit:test/0),
	module_property(program_space_limit, program_size(Size)).
test(assert, [cleanup(abolish(program_space_limit:test/0))]) :-
	set_module(program_space_limit:program_space(1000)),
	module_property(program_space_limit, program_space(Space)),
	assertion(Space == 1000),
	assertz(program_space_limit:test).
test(overflow, [ cleanup(abolish(program_space_limit:test/1)),
		 error(resource_error(program_space))
	       ]) :-
	set_module(program_space_limit:program_space(1000)),
	forall(between(1, 100, X),
	       assertz(program_space_limit:test(X))).
test(repeat, [ cleanup(abolish(program_space_limit:test/1))
	     ]) :-
	set_module(program_space_limit:program_space(100000)),
	module_property(program_space_limit, program_size(Size0)),
	forall(between(1, 100, _),
	       ( forall(between(1, 100, X),
			assertz(program_space_limit:test(X))),
		 retractall(program_space_limit:test(_)),
		 module_property(program_space_limit, program_size(Size)),
		 assertion(Size0 == Size),
		 Size0 == Size		% no warning if optimized away
	       )).

:- end_tests(program_space).
