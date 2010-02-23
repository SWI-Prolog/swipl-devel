/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_turtle,
	  [ test_turtle/0,
	    test_turtle/1		% +Test
	  ]).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../RDF')).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(foreign, '../clib')).

:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_compare)).
:- use_module(library(rdf_ntriples)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(aggregate)).

:- dynamic
	error/1,
	passed/1,
	this_dir/1.

:- retractall(this_dir(_)),
   prolog_load_context(directory, Dir),
   asserta(this_dir(Dir)).


test_turtle :-
	retractall(error(_)),
	retractall(passed(_)),
	this_dir(Dir),
	atom_concat(Dir, '/Tests/Turtle', TestDir),
	test_dir(TestDir),
	(   error(_)
	->  fail
	;   aggregate_all(count, passed(_), Passed),
	    aggregate_all(count, blocked(_), Blocked),
	    format('~NAll ~D Turtle tests passed (~D blocked)~n',
		   [Passed, Blocked])
	).

test_turtle(File) :-
	this_dir(Here),
	atomic_list_concat([Here, '/Tests/Turtle/', File], FullFile),
	test_file(FullFile).

%%	blocked(?Test)
%
%	True if Test is blocked.  Currently blocked:
%
%	    $ test-29.ttl :
%	    URI test.  Contains ...%&...  Should or shouldn't we
%	    do %-decoding!?  Surely there are datasets our there
%	    that expect us to do so.
%
%	    $ test-28.ttl :
%	    Test numbers.  I don't understand this test and I don't
%	    understand the *three* files: test-28.ttl, test-28.out
%	    and test-28.out.ttl.

blocked('test-28.ttl').
blocked('test-29.ttl').


%:- debug(test_turtle).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handle the test-cases provided with the Turtle language definition.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test_dir(Dir) :-
	atom_concat(Dir, '/*.ttl', Pattern),
	expand_file_name(Pattern, Files),
	maplist(test_file, Files).

test_file(File) :-
	file_base_name(File, BaseName),
	blocked(BaseName), !,
	print_message(informational, test_turtle(blocked, BaseName)).
test_file(File) :-
	file_base_name(File, Base),
	atom_concat(bad, _, Base), !,
	file_base_name(File, BaseName),
	debug(test_turtle, 'Negative test ~w ...', [BaseName]),
	catch(load_turtle(File, _Triples), E, true),
	(   nonvar(E)
	->  test_passed(BaseName)
	;   print_message(error, test_turtle(false, BaseName))
	).
test_file(File) :-
	file_base_name(File, BaseName),
	file_name_extension(Base, ttl, File),
	file_name_extension(Base, out, OkFile),
	exists_file(OkFile), !,
	debug(test_turtle, 'Test ~w ...', [BaseName]),
	load_turtle(File, Triples),
	load_rdf_ntriples(OkFile, OkTriples0),
	maplist(canonical_triple, OkTriples0, OkTriples),
	sort(Triples, Turtle),
	sort(OkTriples, OK),
	(   rdf_equal_graphs(OK, Turtle, _)
	->  test_passed(BaseName)
	;   print_message(error, test_turtle(false, BaseName)),
	    (	debugging(test_turtle)
	    ->	report_diff(OK, Turtle)
	    ;	true
	    )
	).
test_file(_).				% not a test

load_turtle(File, Triples) :-
	file_base_name(File, Base),
	atom_concat('http://www.w3.org/2001/sw/DataAccess/df1/tests/',
		    Base,
		    BaseURI),
	rdf_read_turtle(File, Triples,
			[ base_uri(BaseURI),
			  anon_prefix(node(_)),
			  on_error(error)
			]).

canonical_triple(rdf(S0, P0, O0),
		 rdf(S,  P,  O)) :-
	canonical_node(S0, S),
	canonical_node(P0, P),
	canonical_node(O0, O).

canonical_node(node(GenId), node(N)) :-
	atom_concat(genid, AN, GenId), !,
	atom_number(AN, N).
canonical_node(Node, Node).

report_diff(OK, Result) :-
	rdf_equal_graphs(OK, Result, _), !.
report_diff(OK, Result) :-
	subtract(OK, Result, Missing),
	subtract(Result, OK, TooMany),
	(   Missing \== []
	->  length(Missing, NM),
	    format('**************** ~D Omitted results:~n', [NM]),
	    write_list(Missing)
	;   true
	),
	(   TooMany \== []
	->  length(TooMany, TM),
	    format('**************** ~D Overcomplete results:~n', [TM]),
	    write_list(TooMany)
	;   true
	).

write_list([]).
write_list([H|T]) :-
	(   H =.. [row|Cols]
	->  write_cols(Cols),
	    format(' .~n')
	;   H = rdf(S,P,O)
	->  write_cell(S), put(' '),
	    write_cell(P), put(' '),
	    write_cell(O), write(' .\n')
	;   format('~p~n', [H])
	),
	write_list(T).


write_cols([]).
write_cols([H|T]) :-
	write_cell(H),
	(   T == []
	->  true
	;   put(' '),
	    write_cols(T)
	).

write_cell(literal(X)) :- !,
	format('"~w"', [X]).
write_cell(R) :-
	atom(R),
	rdf_global_id(NS:Id, R), !,
	format('<~w:~w>', [NS, Id]).
write_cell('$null$') :- !,
	write('NULL').
write_cell(R) :-
	atom(R), !,
	format('<!~w>', [R]).
write_cell(X) :-
	format('~p', [X]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

test_passed(Test) :-
	print_message(informational, test_turtle(true, Test)),
	(   current_prolog_flag(verbose, silent)
	->  put_char(user_error, '.')
	;   true
	).

:- multifile
	prolog:message//1.

prolog:message(test_turtle(true, Test)) -->
	{ assert(passed(Test)) },
	[ 'Turtle test ~q: ~tpassed~42|'-[Test] ].
prolog:message(test_turtle(false, Test)) -->
	{ assert(error(Test)) },
	[ 'Turtle test ~q: ~tFAILED~42|'-[Test], nl,
	  'Re-run with "?- debug(test_turtle)." to see more details'-[]
	].
prolog:message(test_turtle(blocked, Test)) -->
	[ 'Turtle test ~q: ~t(blocked)~42|'-[Test] ].
