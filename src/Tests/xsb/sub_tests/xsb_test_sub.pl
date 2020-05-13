/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
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

:- module(xsb_test_sub,
          [ xsb_test_sub/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

/** <module> Run XSB call subsumption tests
*/

xsb_test_sub :-
    run_tests([ xsb_sub
              ]).

term_expansion(sub_test(Test),
               (   test(Test) :-
                       run_sub_test(Test)
               )).

run_sub_test(Test) :-
    setup_call_cleanup(
        set_prolog_flag(table_subsumptive, true),
        xsb_test(sub_tests,Test,go),
        set_prolog_flag(table_subsumptive, false)).

:- begin_tests(xsb_sub, [sto(rational_trees)]).

sub_test(lrtc1).
sub_test(lrtc2).
sub_test(lrtc3).
sub_test(lrtc4).
sub_test(lrtc5).
sub_test(lrtc6).
sub_test(lrtc7).
sub_test(lrtc8).

sub_test(rrtc1).
sub_test(rrtc2).
sub_test(rrtc3).
sub_test(rrtc4).
sub_test(rrtc5).
sub_test(rrtc6).
sub_test(rrtc7).
sub_test(rrtc8).

sub_test(drtc1).
sub_test(drtc2).
sub_test(drtc3).
sub_test(drtc4).
sub_test(drtc5).
sub_test(drtc6).
sub_test(drtc7).
sub_test(drtc8).

sub_test(sg1).
sub_test(sg2).
sub_test(sg3).
sub_test(sg4).
sub_test(sg5).
sub_test(sg6).
sub_test(sg7).
sub_test(sg8).

sub_test(genome1).
sub_test(genome2).
sub_test(genome3).

sub_test(test_answer_abstraction).

/* These these are (still) too expensive
sub_test(floratest).
sub_test(decker).
sub_test(pilegaard).
*/

:- end_tests(xsb_sub).

