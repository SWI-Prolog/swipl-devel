/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_w3c_test,
	  [ process_manifest/0,
	    process_manifest/1,
	    run_tests/0,
	    run_failed/0,
	    edit_failed/0
	  ]).
:- use_module(rdf).			% our RDF parser
:- use_module(rdf_nt).			% read .nt files

:- dynamic
	rdf/3.

ns(test,
   'http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema/').

local('http://www.w3.org/2000/10/rdf-tests/rdfcore/',
      'W3Ctests/').

process_manifest :-
	process_manifest('W3Ctests/Manifest.rdf').
process_manifest(Manifest) :-
	retractall(rdf(_,_,_)),
	load_rdf(Manifest, Triples),
	assert_triples(Triples).

assert_triples([]).
assert_triples([rdf(S, P, O)|T]) :-
	canonise(S, Subject),
	canonise(P, Predicate),
	canonise(O, Object),
	assert(rdf(Subject, Predicate, Object)),
	assert_triples(T).

canonise(NS:Name, N:Name) :-
	ns(N, NS), !.
canonise(Absolute, N:Name) :-
	atom(Absolute),
	ns(N, NS),
	atom_concat(NS, Name, Absolute), !.
canonise(X, X).
	

:- dynamic
	test_result/2.

run_tests :-
	retractall(test_result(_, _)),
	(   rdf(About, rdf:type, test:Type),
	    test_type(Type),
	    run_test(About, true),
	    fail
	;   true
	), !,
	report_results.

test_type('PositiveParserTest').
%test_type('NegativeParserTest').

report_results :-
	findall(X, test_result(X, true), Positive),
	findall(X, test_result(X, false), Negative),
	length(Positive, P),
	length(Negative, N),
	format('~N~n~w tests succeeded, ~w tests failed~n', [P, N]).


run_test(Test, Assert) :-
	rdf(Test, test:inputDocument, In),
	local_file(In, InFile),
	exists_file(InFile),
	rdf(Test, test:outputDocument, Out),
	local_file(Out, NTFile),
	load_rdf(InFile, RDF),
	load_rdf_nt(NTFile, NT),
	(   compare_triples(RDF, NT)
	->  store(Assert, test_result(Test, true))
	;   store(Assert, test_result(Test, false)),
	    format('~N*** Test failed: ~w. Our triples~n', [Test]),
	    pp(RDF),
	    format('~N*** Normative output~n'),
	    pp(NT)
	).

store(true, Term) :- !,
	assert(Term).
store(_, _).


local_file(URL, File) :-
	local(URLPrefix, FilePrefix),
	atom_concat(URLPrefix, Base, URL), !,
	atom_concat(FilePrefix, Base, File).

		 /*******************************
		 *	    DEBUG TESTS		*
		 *******************************/

run_failed :-
	test_result(Test, false), !,
	run_test(Test, false).

edit_failed :-
	test_result(Test, false), !,
	rdf(Test, test:inputDocument, In),
	local_file(In, InFile),
	edit(file(InFile)).



		 /*******************************
		 *	     COMPARING		*
		 *******************************/

compare_triples(A, B) :-
	compare_list(A, B, [], _).

compare_list([], [], S, S).
compare_list([H1|T1], In2, S0, S) :-
	select(H2, In2, T2),
	compare_triple(H1, H2, S0, S1), !,
	compare_list(T1, T2, S1, S).

compare_triple(rdf(Subj1,P1,O1), rdf(Subj2, P2, O2), S0, S) :-
	compare_field(Subj1, Subj2, S0, S1),
	compare_field(P1, P2, S1, S2),
	compare_field(O1, O2, S2, S).

compare_field(X, X, S, S) :- !.
compare_field(rdf:Name, Atom, S, S) :-
	rdf_parser:rdf_name_space(NS),
	atom_concat(NS, Name, Atom), !.
compare_field(NS:Name, Atom, S, S) :-
	atom_concat(NS, Name, Atom), !.
compare_field(X, node(Id), S, S) :-
	memberchk(X=Id, S), !.
compare_field(X, node(Id), S, [X=Id|S]) :-
	\+ memberchk(X=_, S),
	atom(X),
	sub_atom(X, 0, _, _, 'Description__'),
	format('Assume ~w = ~w~n', [X, node(Id)]).

