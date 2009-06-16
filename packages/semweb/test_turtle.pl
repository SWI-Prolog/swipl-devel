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
	  [ test_turtle/0
	  ]).
:- asserta(user:file_search_path(library, '..')).

:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(rdf_ntriples)).
:- use_module(library(apply)).
:- use_module(library('semweb/rdf_db')).

test_turtle :-
	test_dir('Tests/Turtle').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handle the test-cases provided with the Turtle language definition.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test_dir(Dir) :-
	atom_concat(Dir, '/*.ttl', Pattern),
	expand_file_name(Pattern, Files),
	maplist(test_file, Files).

test_file(File) :-
	file_base_name(File, Base),
	atom_concat(bad, _, Base), !,
	file_base_name(File, BaseName),
	format('Negative test ~w ...', [BaseName]), flush_output,
	catch(load_turtle(File, _Triples), E, true),
	(   nonvar(E)
	->  format('ok~n')
	;   format(' SHOULD FAIL~n')
	).
test_file(File) :-
	file_base_name(File, BaseName),
	format('Test ~w ...', [BaseName]), flush_output,
	load_turtle(File, Triples),
	file_name_extension(Base, ttl, File),
	file_name_extension(Base, out, OkFile),
	load_rdf_ntriples(OkFile, OkTriples0),
	maplist(canonical_triple, OkTriples0, OkTriples),
	sort(Triples, Turtle),
	sort(OkTriples, OK),
	report_diff(OK, Turtle),
	format(' done~n').

load_turtle(File, Triples) :-
	file_base_name(File, Base),
	atom_concat('http://www.redland.opensource.ac.uk/raptor/tests/turtle/',
		    Base,
		    BaseURI),
	rdf_load_turtle(File, Triples,
			[ base_uri(BaseURI),
			  anon_prefix(node(_))
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
	compare_triples(OK, Result, _), !.
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
		 *	      COMPARE		*
		 *******************************/

%%	compare_triples(+PlRDF, +NTRDF, -Substitions)
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
compare_field(X, Id, S, S) :-
	memberchk(X=Id, S), !.
compare_field(X, Y, S, [X=Y|S]) :-
	\+ memberchk(X=_, S),
	node_id(X),
	node_id(Y),
	debug(rdf_compare, 'Assume ~w = ~w~n', [X, Y]).

node_id(node(_)) :- !.
node_id(X) :-
	atom(X),
	sub_atom(X, 0, _, _, '__').
