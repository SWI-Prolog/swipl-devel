/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2015, University of Amsterdam
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

:- module(test_scan_options,
	  [ test_scan_options/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(terms)).
:- use_module(library(apply)).

/** <module> Test option processing

This test suite deals with  built-in   option  processing. Note that, in
default mode, SWI-Prolog option processing ignores unknown options.
*/

test_scan_options :-
	run_tests([ scan_options,
		    dict_option
		  ]).

:- begin_tests(scan_options, []).

test(process, End == 1) :-
	numbervars(x(_,X,X), 0, End, [singletons(true)]).
test(implicit_true, End == 1) :-
	numbervars(x(_,X,X), 0, End, [singletons]).
test(no_option, End == 2) :-
	numbervars(x(_,X,X), 0, End, [unlikely(true)]).
test(bad_value_type, error(type_error(bool, 42))) :-
	numbervars(x(_,X,X), 0, _, [singletons(42)]).
test(bad_type, error(domain_error(numbervar_option, unlikely))) :-
	numbervars(x(_,X,X), 0, _, [unlikely]).
test(bad_type, error(domain_error(numbervar_option, f(a,b)))) :-
	numbervars(x(_,X,X), 0, _, [f(a,b)]).
test(bad_type, error(domain_error(numbervar_option, 1.3))) :-
	numbervars(x(_,X,X), 0, _, [1.3]).
test(instantiation, error(instantiation_error)) :-
	numbervars(x(_,X,X), 0, _, [_]).
test(instantiation, error(instantiation_error)) :-
	numbervars(x(_,X,X), 0, _, [singletons(true)|_]).
test(instantiation, error(type_error(list,1))) :-
	numbervars(x(_,X,X), 0, _, [singletons(true)|1]).

:- end_tests(scan_options).

:- begin_tests(dict_option, []).

test(process, End == 1) :-
	numbervars(x(_,X,X), 0, End, _{singletons:true}).
test(bad_value_type, error(type_error(bool, 42))) :-
	numbervars(x(_,X,X), 0, _, _{singletons:42}).
test(no_option, End == 2) :-
	numbervars(x(_,X,X), 0, End, _{unlikely:true}).

:- end_tests(dict_option).
