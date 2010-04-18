/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_rdf,
	  [ suite/1,			% +Test-number
	    test_dir/1,			% +Directory
	    test_file/1,		% +File
	    time_file/1,		% +File
	    passed/1,			% +Test-numberOrFile
	    test_rdf/0,			% run whole suite
	    show_ok/1			% +Test
	  ]).

:- multifile
	user:file_search_path/2.

user:file_search_path(library, .).
user:file_search_path(library, '../sgml').
user:file_search_path(library, '../clib').
user:file_search_path(library, '..').
user:file_search_path(foreign, '../sgml').
user:file_search_path(foreign, '../clib').
user:file_search_path(foreign, '../semweb').

:- use_module(library(sgml)).
:- use_module(library(semweb/rdf_compare)).
:- use_module(library(rdf_parser)).
:- use_module(library(rdf_triple)).
:- use_module(library(rdf)).
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
	atomic_list_concat(['suite/t', N, '.rdf'], File),
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
	make_rdf_state([base_uri('http://test.org/test/')], State, _),
	xml_to_plrdf(RDFElement, RDF, State),
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
	atomic_list_concat(['suite/t', Id, '.rdf'], File),
	passed(File).
passed(File) :-
	rdf_reset_ids,
	ok_file(File, OkFile),
	load_rdf(File, Triples),
	open(OkFile, write, Fd, [encoding(utf8)]),
	save_triples(Triples, Fd),
	close(Fd),
	length(Triples, N),
	format('Saved ~d triples to ~w~n', [N, OkFile]).

:- dynamic failed/1.

test_rdf :-
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
	->  (   catch(open(OkFile, read, Fd, [encoding(utf8)]), _, fail)
	    ->  (   read_triples(Fd, OkTriples),
		    close(Fd),
		    rdf_equal_graphs(Triples, OkTriples, _Subst)
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
	atomic_list_concat([Dir, /, ok, /, Base, '.ok'], OkFile).


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
	format('{~q, ~q, ~q}~n', [S,P,O]).
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
		 *	    SHOW DIAGRAM	*
		 *******************************/

show_ok(Test) :-
	ok_file(Test, File),
	open(File, read, Fd, [encoding(utf8)]),
	read_triples(Fd, OkTriples),
	close(Fd),
	new(D, rdf_diagram(string('Ok for %s', File))),
	send(D, triples, OkTriples),
	send(D, open).
