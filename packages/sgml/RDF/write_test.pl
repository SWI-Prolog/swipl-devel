/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(test_rdf_write,
	  [ run_tests/0,
            run_tests/1
	  ]).

:- asserta(user:file_search_path(foreign, '..')).
:- asserta(user:file_search_path(foreign, '../../semweb')).
:- asserta(user:file_search_path(library, '../..')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../../plunit')).

:- use_module(library(plunit)).
:- use_module(library(rdf_write)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf).


		 /*******************************
		 *	    ROUND TRIP		*
		 *******************************/

test_graph(Triples) :-
	tmp_file(rdf, Tmp),
	open(Tmp, write, Out, [encoding(utf8)]),
	rdf_write_xml(Out, Triples),
	close(Out),
	load_rdf(Tmp, ReadTriples),
	delete_file(Tmp),
	compare_triples(Triples, ReadTriples, _).
	

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
	rdf_is_bnode(X),
	rdf_is_bnode(Y),
	debug(bnode, 'Assume ~w = ~w~n', [X, Y]).


		 /*******************************
		 *	      TESTS		*
		 *******************************/

:- begin_tests(rdf_write).

test(1, true) :-
	test_graph([ rdf(s, p, o)
		   ]).
test(anon_s, true) :-
	test_graph([ rdf('__s', p, o)
		   ]).
test(anon_o, true) :-
	test_graph([ rdf(s, p, '__o')
		   ]).
test(anon_loop, blocked('NodeID map must check for cycles')) :-
	test_graph([ rdf('__r1', p1, '__r2'),
		     rdf('__r2', p1, '__r1')
		   ]).
test(anon_loop, true) :-
	test_graph([ rdf('__r1', p1, '__r2'),
		     rdf('__r1', p2, '__r2'),
		     rdf('__r2', p1, '__r1'),
		     rdf('__r2', p2, '__r1')
		   ]).
test(anon_reuse, true) :-
	test_graph([ rdf('__s1', p1, '__o1'),
		     rdf('__s2', p1, '__o1')
		   ]).
test(anon_reuse, true) :-
	test_graph([ rdf('__s1', p1, '__o1'),
		     rdf('__s2', p1, '__o1'),
		     rdf('__o1', name, literal(foo))
		   ]).
test(literal, true) :-
	test_graph([ rdf(s, p, literal(hello))
		   ]).
test(lang, true) :-
	test_graph([ rdf(s, p, literal(lang(en, hello)))
		   ]).
test(type, true) :-
	test_graph([ rdf(s, p, literal(type(t, hello)))
		   ]).

:- end_tests(rdf_write).


