/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

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

:- module(sparql_client,
	  [ sparql_query/3,		% +Query, -Row, +Options
	    sparql_set_server/1,	% +Options
	    sparql_read_xml_result/2	% +Stream, -Result
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(rdf)).
:- use_module(library(option)).

/** <module> SPARQL client library

This module provides a SPARQL client.  For example:

    ==
    ?- sparql_query('select * where { ?x rdfs:label "Amsterdam" }', Row,
		    [ host('dbpedia.org'), path('/sparql')]).

    Row = row('http://www.ontologyportal.org/WordNet#WN30-108949737') ;
    false.
    ==

Or, querying a local server using an =ASK= query:

    ==
    ?- sparql_query('ask { owl:Class rdfs:label "Class" }', Row,
		    [ host('localhost'), port(3020), path('/sparql/')]).
    Row = true.
    ==
*/


%%	sparql_query(+Query, -Result, +Options) is nondet.
%
%	Execute a SPARQL query on an HTTP   SPARQL endpoint. Query is an
%	atom that denotes  the  query.  Result   is  unified  to  a term
%	rdf(S,P,O) for =CONSTRUCT= and =DESCRIBE=  queries, row(...) for
%	=SELECT= queries and  =true=  or   =false=  for  =ASK=  queries.
%	Options are
%
%	    * host(+Host)
%	    * port(+Port)
%	    * path(+Path)
%	    The above three options set the location of the server.
%	    * search(+ListOfParams)
%	    Provide additional query parameters, such as the graph.
%	    * variable_names(-ListOfNames)
%	    Unifies ListOfNames with a list of atoms that describe the
%	    names of the variables in a =SELECT= query.
%
%	Remaining options are passed to   http_open/3.  The defaults for
%	Host, Port and Path can be   set  using sparql_set_server/1. The
%	initial default for port is 80 and path is =|/sparql/|=.

sparql_query(Query, Row, Options) :-
	sparql_param(host(Host), Options,  Options1),
	sparql_param(port(Port), Options1, Options2),
	sparql_param(path(Path), Options2, Options3),
	select_option(search(Extra), Options3, Options4, []),
	select_option(variable_names(VarNames), Options4, Options5, _),
	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(Path),
		    search([ query = Query
			   | Extra
			   ])
		  | Options5
		  ], In,
		  [ header(content_type, ContentType)
		  ]),
	plain_content_type(ContentType, CleanType),
	read_reply(CleanType, In, VarNames, Row).

read_reply('application/rdf+xml', In, _, Row) :- !,
	call_cleanup(load_rdf(stream(In), RDF), close(In)),
	member(Row, RDF).
read_reply(MIME, In, VarNames, Row) :-
	sparql_result_mime(MIME), !,
	call_cleanup(sparql_read_xml_result(stream(In), Result),
		     close(In)),
	varnames(Result, VarNames),
	xml_result(Result, Row).
read_reply(Type, In, _, _) :-
	read_stream_to_codes(In, Codes),
	string_to_list(Reply, Codes),
	close(In),
	throw(error(domain_error(sparql_result_document, Type),
		    context(_, Reply))).

sparql_result_mime('application/sparql-results+xml').
sparql_result_mime('application/sparql-result+xml').


plain_content_type(Type, Plain) :-
	sub_atom(Type, B, _, _, (;)), !,
	sub_string(Type, 0, B, _, Main),
	normalize_space(atom(Plain), Main).
plain_content_type(Type, Type).

xml_result(ask(Bool), Result) :- !,
	Result = Bool.
xml_result(select(_VarNames, Rows), Result) :-
	member(Result, Rows).

varnames(ask(_), _).
varnames(select(VarTerm, _Rows), VarNames) :-
	VarTerm =.. [_|VarNames].


		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

:- dynamic
	sparql_setting/1.

sparql_setting(port(80)).
sparql_setting(path('/sparql/')).

sparql_param(Param, Options0, Options) :-
	select_option(Param, Options0, Options), !.
sparql_param(Param, Options, Options) :-
	sparql_setting(Param), !.
sparql_param(Param, Options, Options) :-
	functor(Param, Name, _),
	throw(error(existence_error(option, Name), _)).

%%	sparql_set_server(+OptionOrList)
%
%	Set sparql server default options.  Provided defaults are:
%	host, port and repository.  For example:
%
%	    ==
%		set_sparql_default([ host(localhost),
%				     port(8080)
%				     repository(world)
%				   ])
%	    ==

sparql_set_server([]) :- !.
sparql_set_server([H|T]) :- !,
	sparql_set_server(H),
	sparql_set_server(T).
sparql_set_server(Term) :-
	functor(Term, Name, Arity),
	functor(Unbound, Name, Arity),
	retractall(sparql_setting(Unbound)),
	assert(sparql_setting(Term)).


		 /*******************************
		 *	       RESULT		*
		 *******************************/

ns(sparql, 'http://www.w3.org/2005/sparql-results#').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read    the    SPARQL    XML    result     format    as    defined    in
http://www.w3.org/TR/rdf-sparql-XMLres/, version 6 April 2006.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	  MACRO HANDLING	*
		 *******************************/

%	substitute 'sparql' by the namespace   defined  above for better
%	readability of the remainder of the code.

term_subst(V, _, _, V) :-
	var(V), !.
term_subst(F, F, T, T) :- !.
term_subst(C, F, T, C2) :-
	compound(C), !,
	functor(C, Name, Arity),
	functor(C2, Name, Arity),
	term_subst(0, Arity, C, F, T, C2).
term_subst(T, _, _, T).

term_subst(A, A, _, _, _, _) :- !.
term_subst(I0, Arity, C0, F, T, C) :-
	I is I0 + 1,
	arg(I, C0, A0),
	term_subst(A0, F, T, A),
	arg(I, C, A),
	term_subst(I, Arity, C0, F, T, C).

term_expansion(T0, T) :-
	ns(sparql, NS),
	term_subst(T0, sparql, NS, T).


		 /*******************************
		 *	     READING		*
		 *******************************/

%%	sparql_read_xml_result(+Input, -Result)
%
%	Specs from http://www.w3.org/TR/rdf-sparql-XMLres/.  The returned
%	Result term is of the format:
%
%		* select(VarNames, Rows)
%		Where VarNames is a term v(Name, ...) and Rows is a
%		list of row(....) containing the column values in the
%		same order as the variable names.
%
%		* ask(Bool)
%		Where Bool is either =true= or =false=

sparql_read_xml_result(Input, Result) :-
	load_structure(Input, DOM,
		       [ dialect(xmlns),
			 space(remove)
		       ]),
	dom_to_result(DOM, Result).

dom_to_result(DOM, Result) :-
	(   sub_element(DOM, sparql:head, _HAtt, Content)
	->  variables(Content, Vars)
	;   Vars = []
	),
	(   Vars == [],
	    sub_element(DOM, sparql:boolean, _, [TrueFalse])
	->  Result = ask(TrueFalse)
	;   VarTerm =.. [v|Vars],
	    Result = select(VarTerm, Rows),
	    sub_element(DOM, sparql:results, _RAtt, RContent)
	->  rows(RContent, Vars, Rows)
	).

%%	variables(+DOM, -Varnames)
%
%	Deals with <variable name=Name>.  Head   also  may contain <link
%	href="..."/>. This points to additional   meta-data.  Not really
%	clear what we can do with that.

variables([], []).
variables([element(sparql:variable, Att, [])|T0], [Name|T]) :- !,
	memberchk(name=Name, Att),
	variables(T0, T).
variables([element(sparql:link, _, _)|T0], T) :-
	variables(T0, T).


rows([], _, []).
rows([R|T0], Vars, [Row|T]) :-
	row_values(Vars, R, Values),
	Row =.. [row|Values],
	rows(T0, Vars, T).

row_values([], _, []).
row_values([Var|VarT], DOM, [Value|ValueT]) :-
	(   sub_element(DOM, sparql:binding, Att, Content),
	    memberchk(name=Var, Att)
	->  value(Content, Value)
	;   Value = '$null$'
	),
	row_values(VarT, DOM, ValueT).

value([element(sparql:literal, Att, Content)], literal(Lit)) :- !,
	lit_value(Content, Value),
	(   memberchk(datatype=Type, Att)
	->  Lit = type(Type, Value)
	;   memberchk(xml:lang=Lang, Att)
	->  Lit = lang(Lang, Value)
	;   Lit = Value
	).
value([element(sparql:uri, [], [URI])], URI) :- !.
value([element(sparql:bnode, [], [NodeID])], URI) :- !,
	atom_concat('__', NodeID, URI).			% DUBIOUS
value([element(sparql:unbound, [], [])], '$null$').


lit_value([], '').
lit_value([Value], Value).


%%	sub_element(+DOM, +Name, -Atttribs, -Content)

sub_element(element(Name, Att, Content), Name, Att, Content).
sub_element(element(_, _, List), Name, Att, Content) :-
	sub_element(List, Name, Att, Content).
sub_element([H|T], Name, Att, Content) :-
	(   sub_element(H, Name, Att, Content)
	;   sub_element(T, Name, Att, Content)
	).

