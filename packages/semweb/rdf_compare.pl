/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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

:- module(rdf_compare,
	  [ rdf_equal_graphs/3		% +Graph1, +Graph2, -Substitutions
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(apply)).
:- use_module(library(debug)).


/** <module> Compare RDF graphs

This library provides predicates that compare   RDF  graphs. The current
version only provides one predicate:   rdf_equal_graphs/3  verifies that
two graphs are identical after proper labeling of the blank nodes.

Future versions of this library may   contain  more advanced operations,
such as diffing two graphs.
*/


%%	rdf_equal_graphs(+GraphA, +GraphB, -Substition) is semidet.
%
%	True if GraphA  and  GraphB  are   the  same  under  Substition.
%	Substition is a list of BNodeA = BNodeB, where BNodeA is a blank
%	node that appears in GraphA and  BNodeB   is  a  blank node that
%	appears in GraphB.
%
%	@param GraphA is a list of rdf(S,P,O) terms
%	@param GraphB is a list of rdf(S,P,O) terms
%	@param Substition is a list if NodeA = NodeB terms.
%	@tbd	The current implementation is rather naive.  After
%		dealing with the subgraphs that contain no bnodes,
%		it performs a fully non-deterministic substitution.

rdf_equal_graphs(A, B, Substitutions) :-
	sort(A, SA),
	sort(B, SB),
	partition(contains_bnodes, SA, VA, GA),
	partition(contains_bnodes, SB, VB, GB),
	GA == GB,
	compare_list(VA, VB, [], Substitutions), !.

contains_bnodes(rdf(S,P,O)) :-
	(   node_id(S)
	;   node_id(P)
	;   node_id(O)
	), !.

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
	rdf_is_bnode(X).

