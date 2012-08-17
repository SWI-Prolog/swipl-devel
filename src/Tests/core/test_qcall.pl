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
test(local_argvar, M-C == qm-qm) :-
	call((m(M),M:(qm(C),qm(C)))).
test(local_argvar, M-C == qm-qm) :-
	call((m(M),M:(call(qm(C)),qm(C)))).

:- end_tests(qualified_calls).
