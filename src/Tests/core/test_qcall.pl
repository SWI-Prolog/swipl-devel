/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2012-2014, University of Amsterdam
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

:- module(test_qcall,
	  [ test_qcall/0
	  ]).
:- use_module(library(plunit)).

test_qcall :-
	run_tests([ qualified_calls
		  ]).

:- op(900, xfx, @).

:- module_transparent qm:qm/1.

qm:qm(C) :-
	context_module(C).

m(qm).
me(1=qm).
m2(qm2).
g(A,qm(A)).

:- begin_tests(qualified_calls).

test(colon, C == qm) :-
	qm:qm(C).
test(colonc, C == qm) :-
	qm:(qm(C),true).
test(colonv, C == qm) :-
	m(M),
	M:qm(C).
test(coloncv, C == qm) :-
	m(M),
	M:(true,qm(C)).
test(at, C == qm2) :-
	qm:qm(C)@qm2.
test(vat, C == qm2) :-
	m(M),
	M:qm(C)@qm2.
test(atv, C == C) :-
	m2(QC),
	qm:qm(C)@QC.
test(atvv, C == C) :-
	m(M),
	m2(QC),
	M:qm(C)@QC.
test(atvvv, C == C) :-
	m(M),
	m2(QC),
	g(C,G),
	M:G@QC.
test(atvvv, C == C) :-
	m(M),
	m2(QC),
	g(C,G),
	M:(G,true)@QC.
test(localv, M-C == qm-qm) :-
	call((m(M),M:qm(C))).
test(localv, M-C == qm-qm) :-
	call((me(1=M),M:qm(C))).
test(local_argvar, M-C == qm-qm) :-
	call((m(M),M:(qm(C),qm(C)))).
test(local_argvar, M-C == qm-qm) :-
	call((m(M),M:(call(qm(C)),qm(C)))).

:- end_tests(qualified_calls).
