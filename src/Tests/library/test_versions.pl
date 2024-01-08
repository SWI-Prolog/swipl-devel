/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_versions,
          [ test_versions/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(prolog_versions)).

test_versions :-
    run_tests([ cmp_versions
              ]).

:- begin_tests(cmp_versions).

test(eq) :-
    cmp_versions(=, '9.1', '9.1.2').
test(eq, fail) :-
    cmp_versions(=, '9.1.1', '9.1.2').
test(eq, fail) :-
    cmp_versions(=, '9.1.2-12', '9.1.2-13').
test(lt, fail) :-
    cmp_versions(<, '9.1', '9.1.2').
test(gt, fail) :-
    cmp_versions(>, '9.1', '9.1.2').
test(lt) :-
    cmp_versions(<, '9.1.1', '9.1.2').
test(lt) :-
    cmp_versions(<, '9.1.1-23-g955d063c0', '9.1.2').
test(lt) :-
    cmp_versions(<, '9.1.2-22-g955d063ca', '9.1.2-23-g955d063c0').

:- end_tests(cmp_versions).

