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
	    test/0
	  ]).

:- use_module(library(sgml)).
:- use_module(rdf_parser).
:- use_module(rdf_triple).
:- use_module(rdf).
:- use_module(pretty_print).

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
		       XMLTerm,
		       [ dialect(xmlns),
			 space(remove)
		       ]),
	find_rdf(XMLTerm, RDFElement),
	xml_to_plrdf(RDFElement, [], RDF),
	format('============= Prolog term ==============~n', []),
	pretty_print(RDF),
	rdf_triples(RDF, Triples),
	format('============= Triples ==================~n', []),
	write_triples(Triples).

time_file(File) :-
	time(rdf_parse(File, Triples)),
	length(Triples, Len),
	format('Created ~w triples~n', [Len]).

rdf_parse(File, Triples) :-
	load_structure(File,
		       XMLTerm,
		       [ dialect(xmlns),
			 space(sgml)
		       ]),
	find_rdf(XMLTerm, RDFElement),
	xml_to_plrdf(RDFElement, [], RDF),
	rdf_triples(RDF, Triples).

passed(Id) :-
	integer(Id), !,
	concat_atom(['suite/t', Id, '.rdf'], File),
	passed(File).
passed(File) :-
	rdf_reset_ids,
	ok_file(File, OkFile),
	rdf_parse(File, Triples),
	open(OkFile, write, Fd),
	save_triples(Triples, Fd),
	close(Fd),
	length(Triples, N),
	format('Saved ~d triples to ~w~n', [N, OkFile]).

test :-
	test_dir(suite).

test_dir(Dir) :-
	atom_concat(Dir, '/*.rdf', Pattern),
	expand_file_name(Pattern, TestFiles),
	checklist(test, TestFiles).

test(File) :-
	format('Testing ~w ... ', [File]), flush_output,
	rdf_reset_ids,
	ok_file(File, OkFile),
	(   rdf_parse(File, Triples)
	->  (   catch(open(OkFile, read, Fd), _, fail)
	    ->  (   read_triples(Fd, OkTriples),
		    close(Fd),
		    sort(Triples, Sorted),
		    sort(OkTriples, Sorted)
		->  format('ok~n')
		;   format('WRONG ANSWER~n', [])
		)
	    ;	format('(no .ok file)~n', [])
	    )
	;   format('PARSE FAILED~n', [])
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
read_triples(T0, Fd, [T0|R]) :-
	read(Fd, T1),
	read_triples(T1, Fd, R).

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

