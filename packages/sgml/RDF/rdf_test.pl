/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_test,
	  [ suite/1,			% +Test-number
	    test_dir/1,			% +Directory
	    test_file/1,		% +File
	    time_file/1,		% +File
	    passed/1,			% +Test-numberOrFile
	    test/0,			% run whole suite
	    show_ok/1			% +Test
	  ]).

:- use_module(library(sgml)).
:- use_module(rdf_parser).
:- use_module(rdf_triple).
:- use_module(rdf).
:- use_module(pretty_print).

:- set_prolog_flag(rdf_container, true).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test file for the SWI-Prolog RDF parser.  Toplevel predicates:

	# test/0		
	Run all tests from the `suite' directory and validate the
	the result if the correct result is stored in a .ok file.

	# suite(N)
	Run test on suite/t<N>.rdf, showing RDF, intermediate
	representation and triples on the console.

	# passed(N)
	Parse suite/t<N>.rdf and save the result in suite/t<N>.ok
	
The intention is to write  tests,  use   suite/1  to  make sure they are
parsed correctly and then run passed/1 to   save  the correct answer, so
running test/0 can validate all results.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

suite(N) :-
	concat_atom(['suite/t', N, '.rdf'], File),
	test_file(File).

test_file(File) :-
	rdf_reset_ids,
	format('************* Test ~w ***~n', [File]),
	cat(File),
	load_structure(File,
		       [ RDFElement ],
		       [ dialect(xmlns),
			 space(sgml)
		       ]),
	rdf_start_file([], Cleanup),
	xml_to_plrdf(RDFElement, RDF, []),
	rdf_end_file(Cleanup),
	format('============= Prolog term ==============~n', []),
	pretty_print(RDF),
	rdf_triples(RDF, Triples),
	format('============= Triples ==================~n', []),
	write_triples(Triples).

time_file(File) :-
	time(load_rdf(File, Triples)),
	length(Triples, Len),
	format('Created ~w triples~n', [Len]).

passed(Id) :-
	integer(Id), !,
	concat_atom(['suite/t', Id, '.rdf'], File),
	passed(File).
passed(File) :-
	rdf_reset_ids,
	ok_file(File, OkFile),
	load_rdf(File, Triples),
	open(OkFile, write, Fd),
	save_triples(Triples, Fd),
	close(Fd),
	length(Triples, N),
	format('Saved ~d triples to ~w~n', [N, OkFile]).

:- dynamic failed/1.

test :-
	test(load_rdf),
	test(process_rdf).

test(How) :-
	retractall(failed(_)),
	test_dir(suite, How),
	findall(F, failed(F), Failed),
	(   Failed == []
	->  true
	;   length(Failed, N),
	    format('ERROR: ~w tests failed~n', [N]),
	    fail
	).


test_dir(Dir) :-
	test_dir(Dir, load_rdf).

test_dir(Dir, How) :-
	format('Tests from "~w" [~w]: ', [Dir, How]),
	atom_concat(Dir, '/*.rdf', Pattern),
	expand_file_name(Pattern, TestFiles),
	maplist(test(How), TestFiles),
	format(' done~n').

test(How, File) :-
	format('.'), flush_output,
	rdf_reset_ids,
	ok_file(File, OkFile),
	(   call(How, File, Triples)
	->  (   catch(open(OkFile, read, Fd), _, fail)
	    ->  (   read_triples(Fd, OkTriples),
		    close(Fd),
		    compare_triples(Triples, OkTriples, _Subst)
		->  true
		;   assert(failed(File)),
		    format('~N~w: WRONG ANSWER~n', [File])
		)
	    ;	format('~N~w: (no .ok file)~n', [File])
	    )
	;   assert(failed(File)),
	    format('~N~w: PARSE FAILED~n', [File])
	).

ok_file(File, OkFile) :-
	file_base_name(File, BaseFile),
	file_name_extension(Base, _, BaseFile),
	file_directory_name(File, Dir),
	concat_atom([Dir, /, ok, /, Base, '.ok'], OkFile).


save_triples([], _).
save_triples([H|T], Fd) :-
	format(Fd, '~q.~n', [H]),
	save_triples(T, Fd).

read_triples(Fd, Terms) :-
	read(Fd, T0),
	read_triples(T0, Fd, Terms).

read_triples(end_of_file, _, []) :- !.
read_triples(rdf(S0,P0,O0), Fd, [rdf(S,P,O)|R]) :-
	global_ref(S0, S),
	global_ref(P0, P),
	global_obj(O0, O),
	read(Fd, T1),
	read_triples(T1, Fd, R).

global_ref(rdf:Local, Global) :-
	rdf_name_space(NS), !,
	atom_concat(NS, Local, Global).
global_ref(NS:Local, Global) :- !,
	atom_concat(NS, Local, Global).
global_ref(URI, URI).

global_obj(literal(X), literal(X)) :- !.
global_obj(Local, Global) :-
	global_ref(Local, Global).


write_triples([]) :- !.
write_triples([H|T]) :- !,
	write_triple(H),
	write_triples(T).

write_triple(Triple) :-
	is_rdf_triple(Triple), !,
	Triple = rdf(S,P,O),
	format('{~p, ~p, ~p}~n', [S,P,O]).
write_triple(Triple) :-
	format('@@@@@ Bad Triple: ~p~n', [Triple]),
	fail.

cat(File) :-
	open(File, read, Fd),
	copy_stream_data(Fd, user_output),
	close(Fd).

:- dynamic triple/1.

process_rdf(File, Triples) :-
	retractall(triple(_)),
	process_rdf(File, assert_triples, []),
	findall(T, retract(triple(T)), Triples).

assert_triples([], _).
assert_triples([H|T], Loc) :-
	assert(triple(H)),
	assert_triples(T, Loc).


		 /*******************************
		 *	      VALIDATE		*
		 *******************************/

is_rdf_triple(rdf(Subject, Predicate, Object)) :-
	is_subject(Subject),
	is_predicate(Predicate),
	is_object(Object).

is_subject(0) :- !, fail.		% Variables
is_subject(URI) :- is_uri(URI), !.
is_subject(each(URI)) :- is_uri(URI), !.
is_subject(prefix(Pattern)) :-
	atom(Pattern), !.

is_predicate(0) :- !, fail.
is_predicate(rdf:RdfPred) :- !,
is_rdf_predicate(RdfPred).
is_predicate(NS:Pred) :- !,
	atom(NS),
	atom(Pred).
is_predicate(Pred) :-
	atom(Pred).

is_object(0) :- !,
	fail.
is_object(literal(XML)) :- !,
	is_xml(XML).
is_object(rdf:RdfType) :- !,
	is_rdf_type(RdfType).
is_object(URI) :-
	is_uri(URI).

is_object(Subject) :-
	is_subject(Subject), !.
is_object(Pred) :-
	is_predicate(Pred), !.

is_uri(URI) :- atom(URI).

is_xml(_XML). % for now

is_rdf_predicate(RdfPred) :- atom(RdfPred).

is_rdf_type(RdfType) :- atom(RdfType).

		 /*******************************
		 *	       UTIL		*
		 *******************************/

%	find_rdf(+XMLTerm, -RDFTerm)
%
%	If the document contains an embedded RDF term, return it, else
%	return the whole document.  The latter is a bit dubious, but good
%	for the purpose of this test-file

find_rdf(Term, RDFTerm) :-
	RDFTerm = element(NS:'RDF', _, _),
	term_member(RDFTerm, Term), !,
	(   rdf_name_space(NS)
	->  true
	;   assert(rdf_parser:rdf_name_space(NS)),
	    assert(new_rdf_namespace(NS))
	).
find_rdf(Term, Term).

term_member(X, X).
term_member(X, Compound) :-
	compound(Compound),
	arg(_, Compound, Arg),
	term_member(X, Arg).

		 /*******************************
		 *	     COMPARING		*
		 *******************************/

%	compare_triples(+PlRDF, +NTRDF, -Substitions)
%
%	Compare two models and if they are equal, return a list of
%	PlID = NTID, mapping NodeID elements.


compare_triples(A, B, Substitutions) :-
	compare_list(A, B, [], Substitutions), !.

compare_list([], [], S, S).
compare_list([H1|T1], In2, S0, S) :-
	select(H2, In2, T2),
	compare_triple(H1, H2, S0, S1),
	compare_list(T1, T2, S1, S).

compare_triple(rdf(Subj1,P1,O1), rdf(Subj2, P2, O2), S0, S) :-
	compare_field(Subj1, Subj2, S0, S1),
	compare_field(P1, P2, S1, S2),
	compare_field(O1, O2, S2, S).

compare_field(X, X, S, S) :- !.
compare_field(literal(X), xml(X), S, S) :- !. % TBD
compare_field(rdf:Name, Atom, S, S) :-
	atom(Atom),
	rdf_parser:rdf_name_space(NS),
	atom_concat(NS, Name, Atom), !.
compare_field(NS:Name, Atom, S, S) :-
	atom(Atom),
	atom_concat(NS, Name, Atom), !.
compare_field(X, Id, S, S) :-
	memberchk(X=Id, S), !.
compare_field(X, Y, S, [X=Y|S]) :-
	\+ memberchk(X=_, S),
	node_id(X),
	node_id(Y),
	format('Assume ~w = ~w~n', [X, Y]).

node_id(node(_)) :- !.
node_id(X) :-
	atom(X),
	generated_prefix(Prefix),
	sub_atom(X, 0, _, _, Prefix), !.

generated_prefix('Bag__').
generated_prefix('Seq__').
generated_prefix('Alt__').
generated_prefix('Description__').
generated_prefix('Statement__').

		 /*******************************
		 *	    SHOW DIAGRAM	*
		 *******************************/

show_ok(Test) :-
	ok_file(Test, File),
	open(File, read, Fd),
	read_triples(Fd, OkTriples),
	close(Fd),
	new(D, rdf_diagram(string('Ok for %s', File))),
	send(D, triples, OkTriples),
	send(D, open).
