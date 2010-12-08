/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam
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

:- module(rdf_db,
	  [ rdf_version/1,		% -Version

	    rdf/3,			% ?Subject, ?Predicate, ?Object
	    rdf/4,			% ?Subject, ?Predicate, ?Object, ?DB
	    rdf_has/3,			% ?Subject, +Pred, ?Obj
	    rdf_has/4,			% ?Subject, +Pred, ?Obj, -RealPred
	    rdf_reachable/3,		% ?Subject, +Pred, ?Object
	    rdf_reachable/5,		% ?Subject, +Pred, ?Object, +MaxD, ?D
	    rdf_subject/1,		% ?Subject

	    rdf_member_property/2,	% ?Property, ?Index

	    rdf_assert/3,		% +Subject, +Predicate, +Object
	    rdf_assert/4,		% +Subject, +Predicate, +Object, +DB
	    rdf_retractall/3,		% ?Subject, ?Predicate, ?Object
	    rdf_retractall/4,		% ?Subject, ?Predicate, ?Object, +DB
	    rdf_update/4,		% +Subject, +Predicate, +Object, +Act
	    rdf_update/5,		% +Subject, +Predicate, +Object, +Src, +Act
	    rdf_set_predicate/2,	% +Predicate, +Property
	    rdf_predicate_property/2,	% +Predicate, ?Property
	    rdf_current_predicate/1,	% -Predicate
	    rdf_current_literal/1,	% -Literal
	    rdf_transaction/1,		% :Goal
	    rdf_transaction/2,		% :Goal, +Id
	    rdf_active_transaction/1,	% ?Id

	    rdf_monitor/2,		% :Goal, +Options

	    rdf_save_db/1,		% +File
	    rdf_save_db/2,		% +File, +DB
	    rdf_load_db/1,		% +File
	    rdf_reset_db/0,

	    rdf_node/1,			% -Id
	    rdf_bnode/1,		% -Id
	    rdf_is_bnode/1,		% +Id

	    rdf_is_resource/1,		% +Term
	    rdf_is_literal/1,		% +Term

	    rdf_load/1,			% +File
	    rdf_load/2,			% +File, +Options
	    rdf_save/1,			% +File
	    rdf_save/2,			% +File, +Options
	    rdf_unload/1,		% +File

	    rdf_md5/2,			% +DB, -MD5
	    rdf_atom_md5/3,		% +Text, +Times, -MD5

	    rdf_graph_property/2,	% ?Graph, ?Property
	    rdf_graph/1,		% ?Graph
	    rdf_source/1,		% ?File
	    rdf_source/2,		% ?DB, ?SourceURL
	    rdf_make/0,			% Reload modified databases

	    rdf_source_location/2,	% +Subject, -Source
	    rdf_statistics/1,		% -Key
	    rdf_generation/1,		% -Generation
	    rdf_estimate_complexity/4,	% +S,+P,+O,-Count

	    rdf_save_subject/3,		% +Stream, +Subject, +DB
	    rdf_save_header/2,		% +Out, +Options
	    rdf_save_footer/1,		% +Out

	    rdf_equal/2,		% ?Resource, ?Resource
	    lang_equal/2,		% +Lang1, +Lang2
	    lang_matches/2,		% +Lang, +Pattern

	    rdf_current_ns/2,		% ?Alias, ?URI
	    rdf_register_ns/2,		% +Alias, +URI
	    rdf_register_ns/3,		% +Alias, +URI, +Options
	    rdf_global_id/2,		% ?NS:Name, ?Global
	    rdf_global_object/2,	% ?Object, ?NSExpandedObject
	    rdf_global_term/2,		% Term, WithExpandedNS

	    rdf_match_label/3,		% +How, +String, +Label
	    rdf_split_url/3,		% ?Base, ?Local, ?URL
	    rdf_url_namespace/2,	% +URL, ?Base
	    rdf_quote_uri/2,		% +URI, -Quoted

	    rdf_debug/1,		% Set verbosity

	    rdf_new_literal_map/1,	% -Handle
	    rdf_destroy_literal_map/1,	% +Handle
	    rdf_reset_literal_map/1,	% +Handle
	    rdf_insert_literal_map/3,	% +Handle, +Key, +Literal
	    rdf_insert_literal_map/4,	% +Handle, +Key, +Literal, -NewKeys
	    rdf_delete_literal_map/3,	% +Handle, +Key, +Literal
	    rdf_delete_literal_map/2,	% +Handle, +Key
	    rdf_find_literal_map/3,	% +Handle, +KeyList, -Literals
	    rdf_keys_in_literal_map/3,	% +Handle, +Spec, -Keys
	    rdf_statistics_literal_map/2, % +Handle, +Name(-Arg...)

	    rdf_graph_prefixes/2,	% ?Graph, -Prefixes
	    rdf_graph_prefixes/3,	% ?Graph, -Prefixes, :Filter

	    (rdf_meta)/1,		% +Heads
	    op(1150, fx, (rdf_meta))
	  ]).
:- use_module(library(rdf)).
:- use_module(library(lists)).
:- use_module(library(shlib)).
:- use_module(library(gensym)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(option)).
:- use_module(library(nb_set)).
:- use_module(library(error)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(rdf_cache).

:- use_foreign_library(foreign(rdf_db)).

:- meta_predicate
	rdf_transaction(0),
	rdf_transaction(0, +),
	rdf_monitor(1, +),
	rdf_save(+, :),
	rdf_load(+, :).

:- multifile
	ns/2,
	rdf_meta_specification/3.	% UnboundHead, Module, Head
:- dynamic
	ns/2,			% ID, URL
	rdf_source/5.		% DB, SourceURL, ModTimeAtLoad, Triples, MD5
:- volatile
	rdf_source/5.
:- discontiguous
	term_expansion/2.

/** <module> Core RDF database

@see Documentation for semweb package
*/

		 /*******************************
		 *	     NAMESPACES		*
		 *******************************/

%%	rdf_current_ns(?Alias, ?URI) is nondet.
%
%	Query  predefined  namespaces  and    namespaces   defined  with
%	rdf_register_ns/2.

rdf_current_ns(Alias, URI) :-
	ns(Alias, URI).

%%	ns(?Alias, ?URI) is nondet.
%
%	Dynamic  predicate  that  maintains   the  registered  namespace
%	aliases.

ns(rdf,	    'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
ns(rdfs,    'http://www.w3.org/2000/01/rdf-schema#').
ns(owl,	    'http://www.w3.org/2002/07/owl#').
ns(xsd,	    'http://www.w3.org/2001/XMLSchema#').
ns(dc,	    'http://purl.org/dc/elements/1.1/').
ns(dcterms, 'http://purl.org/dc/terms/').
ns(eor,	    'http://dublincore.org/2000/03/13/eor#').
ns(skos,    'http://www.w3.org/2004/02/skos/core#').
ns(serql,   'http://www.openrdf.org/schema/serql#').

%%	rdf_register_ns(+Alias, +URI) is det.
%%	rdf_register_ns(+Alias, +URI, +Options) is det.
%
%	Register Alias as an abbreviateion for URI. Options:
%
%		* force(Boolean)
%		If =true=, Replace existing namespace alias. Please note
%		that replacing a namespace is dangerous as namespaces
%		affect preprocessing. Make sure all code that depends on
%		a namespace is compiled after changing the registration.
%
%		* keep(Boolean)
%		If =true= and Alias is already defined, keep the
%		original message and succeed silently.
%
%	Without options, an attempt  to  redefine   an  alias  raises  a
%	permission error.

rdf_register_ns(Alias, URI) :-
	rdf_register_ns(Alias, URI, []).

rdf_register_ns(Alias, URI, _) :-
	ns(Alias, URI), !.
rdf_register_ns(Alias, URI, Options) :-
	ns(Alias, _),
	(   option(force(Force), Options, false),
	    Force == true
	->  retractall(ns(Alias, _)),
	    assert(ns(Alias, URI))
	;   option(keep(Keep), Options, false),
	    Keep == true
	->  true
	;   throw(error(permission_error(register, namespace, Alias),
			context(_, 'Already defined')))
	).
rdf_register_ns(Alias, URI, _) :-
	assert(ns(Alias, URI)).

%%	register_file_ns(+Map:list(pair)) is det.
%
%	Register a namespace as encounted in   the  namespace list of an
%	RDF document. We only register if  both the abbreviation and URL
%	are not already known. Is there a   better  way? This code could
%	also do checks on the consistency   of  RDF and other well-known
%	namespaces.
%
%	@tbd	Better error handling

register_file_ns([]) :- !.
register_file_ns([Decl|T]) :- !,
	register_file_ns(Decl),
	register_file_ns(T).
register_file_ns([]=_) :- !.		% xmlns= (overall default)
register_file_ns(NS=URL) :- !,		% compatibility
	register_file_ns(NS-URL).
register_file_ns(NS-URL) :-
	(   ns(NS, URL)
	->  true
	;   ns(NS, _)
	->  true			% redefined abbreviation
	;   ns(_, URL)
	->  true			% redefined URL
	;   rdf_register_ns(NS, URL)
	).


%%	rdf_global_id(?Id, ?GlobalId) is det.
%
%	Convert between NS:Local and global atomic identifier.
%	To be completed.

rdf_global_id(NS:Local, Global) :-
	global(NS, Local, Global), !.
rdf_global_id(Global, Global).


%%	rdf_global_object(+Object, -GlobalObject) is semidet.
%%	rdf_global_object(-Object, +GlobalObject) is semidet.
%
%	Same as rdf_global_id/2,  but  intended   for  dealing  with the
%	object part of a  triple,  in   particular  the  type  for typed
%	literals.
%
%	@error	existence_error(rdf_namespace, NS)

rdf_global_object(NS:Local, Global) :-
	global(NS, Local, Global), !.
rdf_global_object(literal(type(NS:Local, Value)),
		  literal(type(Global, Value))) :-
	global(NS, Local, Global), !.
rdf_global_object(Global, Global).

global(NS, Local, Global) :-
	(   atom(Global)
	->  ns(NS, Full),
	    atom_concat(Full, Local, Global)
	;   atom(NS), atom(Local)
	->  (   ns(NS, Full)
	    *->	atom_concat(Full, Local, Global)
	    ;	current_prolog_flag(xref, true)
	    ->	Global = NS:Local
	    ;	existence_error(rdf_namespace, NS)
	    )
	).


%%	rdf_global_term(+TermIn, -GlobalTerm) is det.
%
%	Does rdf_global_id/2 on all terms NS:Local by recursively analysing
%	the term.

rdf_global_term(Var, Var) :-
	var(Var), !.
rdf_global_term(NS:Local, Global) :-
	atom(NS), atom(Local), ns(NS, Full), !,
	atom_concat(Full, Local, Global).
rdf_global_term([H0|T0], [H|T]) :- !,
	rdf_global_term(H0, H),
	rdf_global_term(T0, T).
rdf_global_term(Term0, Term) :-
	compound(Term0), !,
	Term0 =.. [H|L0],
	rdf_global_term(L0, L),
	Term =.. [H|L].
rdf_global_term(Term, Term).


		 /*******************************
		 *	      EXPANSION		*
		 *******************************/

:- multifile
	system:term_expansion/2,
	system:goal_expansion/2.

system:term_expansion((:- rdf_meta(Heads)), Clauses) :-
	prolog_load_context(module, M),
	mk_clauses(Heads, M, Clauses).

mk_clauses((A,B), M, [H|T]) :- !,
	mk_clause(A, M, H),
	mk_clauses(B, M, T).
mk_clauses(A, M, [C]) :-
	mk_clause(A, M, C).

mk_clause(Head0, M0, rdf_db:rdf_meta_specification(Unbound, Module, Head)) :-
	strip_module(M0:Head0, Module, Head),
	valid_rdf_meta_head(Head),
	functor(Head, Name, Arity),
	functor(Unbound, Name, Arity).

valid_rdf_meta_head(Head) :-
	callable(Head), !,
	Head =.. [_|Args],
	valid_args(Args).
valid_rdf_meta_head(Head) :-
	throw(error(type_error(callable, Head), _)).

valid_args([]).
valid_args([H|T]) :-
	valid_arg(H), !,
	valid_args(T).

valid_arg(:).				% meta argument
valid_arg(+).				% non-var
valid_arg(-).				% var
valid_arg(?).				% either var or non-var
valid_arg(@).				% not modified
valid_arg(r).				% RDF resource
valid_arg(o).				% RDF object
valid_arg(t).				% term with RDF resources
valid_arg(A) :-
	throw(error(type_error(rdf_meta_argument, A), _)).

%%	rdf_meta(+Heads)
%
%	This   directive   is   expanded   using   term-expansion.   The
%	implementation just throws an error in   case  it is called with
%	the wrong context.

rdf_meta(Heads) :-
	throw(error(context_error(nodirective, rdf_meta(Heads)), _)).


system:goal_expansion(G, Expanded) :-
	rdf_meta_specification(G, _, _), !,
	prolog_load_context(module, LM),
	(   rdf_meta_specification(G, Module, Spec),
	    right_module(LM, G, Module)
	->  rdf_expand(G, Spec, Expanded)
	;   debugging(rdf_meta),
	    sub_term(G, NS:Local),
	    atom(NS), atom(Local)
	->  print_message(warning, rdf_meta(not_expanded(LM:G))),
	    fail
	),
	rdf_expand(G, Spec, Expanded).

system:term_expansion(Fact, Expanded) :-
	rdf_meta_specification(Fact, Module, Spec),
	prolog_load_context(module, Module), !,
	rdf_expand(Fact, Spec, Expanded).
system:term_expansion((Head :- Body), (Expanded :- Body)) :-
	rdf_meta_specification(Head, Module, Spec),
	prolog_load_context(module, Module), !,
	rdf_expand(Head, Spec, Expanded).


right_module(M, _, M) :- !.
right_module(LM, G, M) :-
	predicate_property(LM:G, imported_from(M)).

rdf_expand(G, Spec, Expanded) :-
	functor(G, Name, Arity),
	functor(Expanded, Name, Arity),
	rdf_expand_args(0, Arity, G, Spec, Expanded).

rdf_expand_args(Arity, Arity, _, _, _) :- !.
rdf_expand_args(I0, Arity, Goal, Spec, Expanded) :-
	I is I0 + 1,
	arg(I, Goal, GA),
	arg(I, Spec, SA),
	arg(I, Expanded, EA),
	rdf_expand_arg(SA, GA, EA),
	rdf_expand_args(I, Arity, Goal, Spec, Expanded).

rdf_expand_arg(r, A, E) :- !,
	mk_global(A, E).
rdf_expand_arg(o, A, E) :- !,
	rdf_global_object(A, E).
rdf_expand_arg(t, A, E) :- !,
	rdf_global_term(A, E).
rdf_expand_arg(:, A, E) :- !,
	expand_goal(A, E).
rdf_expand_arg(_, A, A).

%%	mk_global(+Src, -Resource)
%
%	Realised rdf_global_id(+, -), but adds compiletime checking,
%	notably to see whether a namespace is not yet defined.

mk_global(X, X) :-
	var(X), !.
mk_global(X, X) :-
	atom(X), !.
mk_global(NS:Local, Global) :-
	must_be(atom, NS),
	must_be(atom, Local),
	(   ns(NS, Full)
	->  atom_concat(Full, Local, Global)
	;   current_prolog_flag(xref, true)
	->  Global = NS:Local
	;   existence_error(namespace, NS)
	).

:- rdf_meta
	rdf(r,r,o),
	rdf_has(r,r,o,r),
	rdf_has(r,r,o),
	rdf_assert(r,r,o),
	rdf_retractall(r,r,o),
	rdf(r,r,o,?),
	rdf_assert(r,r,o,+),
	rdf_retractall(r,r,o,?),
	rdf_reachable(r,r,r),
	rdf_reachable(r,r,r,+,?),
	rdf_update(r,r,o,t),
	rdf_update(r,r,o,+,t),
	rdf_equal(r,r),
	rdf_source_location(r,-),
	rdf_subject(r),
	rdf_set_predicate(r, t),
	rdf_predicate_property(r, -),
	rdf_estimate_complexity(r,r,r,-).

%%	rdf_equal(?Resource1, ?Resource2)
%
%	Simple equality test to exploit goal-expansion

rdf_equal(Resource, Resource).

%%	lang_equal(+Lang1, +Lang2) is semidet.
%
%	True if two RFC language specifiers denote the same language
%
%	@see lang_matches/2.

lang_equal(Lang, Lang) :- !.
lang_equal(Lang1, Lang2) :-
	downcase_atom(Lang1, LangCannon),
	downcase_atom(Lang2, LangCannon).


%%	rdf_has(?Subject, +Predicate, ?Object)
%
%	Succeeds if the triple rdf(Subject, Predicate, Object) is true
%	exploiting the rdfs:subPropertyOf predicate.

rdf_has(Subject, Predicate, Object) :-
	rdf_has(Subject, Predicate, Object, _).


		 /*******************************
		 *	    COLLECTIONS		*
		 *******************************/

%%	rdf_member_property(?Prop, ?Index)
%
%	Deal with the rdf:_1, ... properties.

term_expansion(member_prefix(x),
	       member_prefix(Prefix)) :-
	rdf_db:ns(rdf, NS),
	atom_concat(NS, '_', Prefix).
member_prefix(x).

rdf_member_property(P, N) :-
	integer(N), !,
	member_prefix(Prefix),
	atom_concat(Prefix, N, P).
rdf_member_property(P, N) :-
	member_prefix(Prefix),
	atom_concat(Prefix, Sub, P),
	atom_number(Sub, N).


		 /*******************************
		 *	ANONYMOUS SUBJECTS	*
		 *******************************/

%%	rdf_node(-Id)
%
%	Generate a unique blank node identifier for a subject.
%
%	@deprecated	New code should use rdf_bnode/1.

rdf_node(Resource) :-
	rdf_bnode(Resource).

%%	rdf_bnode(-Id)
%
%	Generate a unique anonymous identifier for a subject.

rdf_bnode(Value) :-
	repeat,
	gensym('__bnode', Value),
	\+ rdf_subject(Value),
	\+ rdf(_, _, Value),
	\+ rdf(_, Value, _), !.



		 /*******************************
		 *	       TYPES		*
		 *******************************/

%%	rdf_is_bnode(+Id)
%
%	Tests if a resource is a blank node (i.e. is an anonymous
%	resource).
%
%	@see rdf_bnode/1.

rdf_is_bnode(Id) :-
	atom(Id),
	sub_atom(Id, 0, _, _, '__').

%%	rdf_is_resource(@Term) is semidet.
%
%	True if Term is an RDF  resource.   Note  that  this is merely a
%	type-test; it does not mean  this   resource  is involved in any
%	triple.  Blank nodes are also considered resources.
%
%	@see rdf_is_bnode/1

rdf_is_resource(Term) :-
	atom(Term).

%%	rdf_is_literal(@Term) is semidet.
%
%	True if Term is an RDF literal object. Currently only checks for
%	groundness and the literal functor.

rdf_is_literal(literal(Value)) :-
	ground(Value).


		 /*******************************
		 *	      SOURCE		*
		 *******************************/

%%	rdf_source_location(+Subject, -File:Line)
%
%	Return the source-locations for triples for this subject.

rdf_source_location(Subject, Source) :-
	findall(Source, rdf(Subject, _, _, Source), Sources),
	sort(Sources, Unique),
	member(Source, Unique).


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

%%	rdf_statistics(?KeyValue) is nondet.
%
%	Obtain statistics on the RDF database.
%
%	@param KeyValue	Term of the form Key(Value).

rdf_statistics(sources(Count)) :-
	predicate_property(rdf_source(_,_,_,_,_), number_of_clauses(Count)).
rdf_statistics(subjects(Count)) :-
	rdf_statistics_(subjects(Count)).
rdf_statistics(properties(Count)) :-
	rdf_statistics_(predicates(Count)).
rdf_statistics(triples(Count)) :-
	rdf_statistics_(triples(Count)).
rdf_statistics(gc(Count, Time)) :-
	rdf_statistics_(gc(Count, Time)).
rdf_statistics(rehash(Count, Time)) :-
	rdf_statistics_(rehash(Count, Time)).
rdf_statistics(core(Bytes)) :-
	rdf_statistics_(core(Bytes)).
rdf_statistics(lookup(Index, Count)) :-
	functor(Indexed, indexed, 16),
	rdf_statistics_(Indexed),
	index(Index, I),
	Arg is I + 1,
	arg(Arg, Indexed, Count),
	Count \== 0.
rdf_statistics(searched_nodes(Count)) :-
	rdf_statistics_(searched_nodes(Count)).
rdf_statistics(literals(Count)) :-
	rdf_statistics_(literals(Count)).
rdf_statistics(triples_by_file(File, Count)) :-
	(   var(File)
	->  rdf_graph(File),
	    rdf_statistics_(triples(File, Count))
	;   rdf_statistics_(triples(File, Count))
	).
rdf_statistics(duplicates(Count)) :-
	rdf_statistics_(duplicates(Count)).

index(rdf(-,-,-,-), 0).
index(rdf(+,-,-,-), 1).
index(rdf(-,+,-,-), 2).
index(rdf(+,+,-,-), 3).
index(rdf(-,-,+,-), 4).
index(rdf(+,-,+,-), 5).
index(rdf(-,+,+,-), 6).
index(rdf(+,+,+,-), 7).

index(rdf(-,-,-,+), 8).
index(rdf(+,-,-,+), 9).
index(rdf(-,+,-,+), 10).
index(rdf(+,+,-,+), 11).
index(rdf(-,-,+,+), 12).
index(rdf(+,-,+,+), 13).
index(rdf(-,+,+,+), 14).
index(rdf(+,+,+,+), 15).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

%%	rdf_current_predicate(?Predicate)
%
%	True if Predicate is a currently defined predicate.

rdf_current_predicate(P) :-
	var(P), !,
	rdf_current_predicates(All),
	member(P, All),
	rdf_predicate_property_(P, triples(N)),
	N > 0.
rdf_current_predicate(P) :-
	rdf_predicate_property_(P, triples(N)),
	N > 0.

rdf_current_predicate(P, DB) :-
	rdf_current_predicates(All),
	member(P, All),
	once(rdf(_,P,_,DB:_)).

%%	rdf_predicate_property(?Predicate, ?Property)
%
%	Enumerate predicates and their properties


rdf_predicate_property(P, Prop) :-
	var(P), !,
	rdf_current_predicates(All),
	member(P, All),
	rdf_predicate_property_(P, Prop).
rdf_predicate_property(P, Prop) :-
	rdf_predicate_property_(P, Prop).


		 /*******************************
		 *	    TRANSACTION		*
		 *******************************/

%%	rdf_transaction(:Goal) is semidet.
%%	rdf_transaction(:Goal, +Id) is semidet.
%
%	Backward compatibility

rdf_transaction(Goal) :-
	rdf_transaction_(Goal, user).
rdf_transaction(Goal, Id) :-
	(   nonvar(Id),
	    Id = log(_, DB)
	->  must_be(atom, DB)
	;   true
	),
	rdf_transaction_(Goal, Id).

%%	rdf_active_transaction(?Id) is nondet.
%
%	True if Id is the identifier of a currently open transaction. If
%	Id  is  not  instantiated,    backtracking   yields  transaction
%	identifiers starting with  the   innermost  nested  transaction.
%	Transaction identifier terms are not copied,  need not be ground
%	and can be instantiated during the transaction.

rdf_active_transaction(Id) :-
	rdf_active_transactions_(List),
	member(Id, List).

%%	rdf_monitor(:Goal, +Options)
%
%	Call Goal if specified actions occur on the database.

rdf_monitor(Goal, Options) :-
	monitor_mask(Options, 0xffff, Mask),
	rdf_monitor_(Goal, Mask).

monitor_mask([], Mask, Mask).
monitor_mask([H|T], Mask0, Mask) :-
	update_mask(H, Mask0, Mask1),
	monitor_mask(T, Mask1, Mask).

update_mask(-X, Mask0, Mask) :- !,
	monitor_mask(X, M),
	Mask is Mask0 /\ \M.
update_mask(+X, Mask0, Mask) :- !,
	monitor_mask(X, M),
	Mask is Mask0 \/ M.
update_mask(X, Mask0, Mask) :-
	monitor_mask(X, M),
	Mask is Mask0 \/ M.

%%	monitor_mask(Name, Mask)
%
%	Mask bit for the monitor events.  Note that this must be kept
%	consistent with the enum broadcast_id defined in rdf_db.c

					% C-defined broadcasts
monitor_mask(assert,	   0x0001).
monitor_mask(assert(load), 0x0002).
monitor_mask(retract,	   0x0004).
monitor_mask(update,	   0x0008).
monitor_mask(new_literal,  0x0010).
monitor_mask(old_literal,  0x0020).
monitor_mask(transaction,  0x0040).
monitor_mask(load,	   0x0080).
monitor_mask(rehash,	   0x0100).
					% prolog defined broadcasts
monitor_mask(parse,	   0x1000).
monitor_mask(reset,	   0x2000).
monitor_mask(unload,	   0x1000).
					% mask for all
monitor_mask(all,	   0xffff).

%rdf_broadcast(Term, MaskName) :-
%%	monitor_mask(MaskName, Mask),
%%	rdf_broadcast_(Term, Mask).


		 /*******************************
		 *    QUICK BINARY LOAD/SAVE	*
		 *******************************/

%%	rdf_save_db(+File) is det.
%%	rdf_save_db(+File, +DB) is det.
%
%	Save triples into File in a   quick-to-load binary format. If DB
%	is supplied only triples flagged to originate from that database
%	are  added.  Files  created  this  way    can  be  loaded  using
%	rdf_load_db/1.

rdf_save_db(File) :-
	open(File, write, Out, [type(binary)]),
	set_stream(Out, record_position(false)),
	call_cleanup(rdf_save_db_(Out, _), close(Out)).


rdf_save_db(File, DB) :-
	open(File, write, Out, [type(binary)]),
	set_stream(Out, record_position(false)),
	call_cleanup(rdf_save_db_(Out, DB), close(Out)).


%%	rdf_load_db_no_admin(+File, +Id, -Graphs) is det.
%
%	Load triples from a  .trp  file   without  updating  the  source
%	administration. Id is  handled  to   monitor  action.  Graphs is
%	either an atom, indicating a single loaded   graph  or a list of
%	graph-names encountered in File.

rdf_load_db_no_admin(File, Id, Graphs) :-
	open(File, read, In, [type(binary)]),
	set_stream(In, record_position(false)),
	call_cleanup(rdf_load_db_(In, Id, Graphs), close(In)).


%%	check_loaded_cache(+DB, +Graphs, +Modified) is det.
%
%	Verify the loaded cache file and optionally fix the modification
%	time (new versions save this along with the snapshot).
%
%	@tbd	What to do if there is a cache mismatch? Delete the loaded
%		graphs and fail?

check_loaded_cache(DB, DB, _Modified) :- !.
check_loaded_cache(DB, Graphs, _) :-
	print_message(warning, rdf(inconsistent_cache(DB, Graphs))).


%%	rdf_load_db(+File) is det.
%
%	Load triples from a file created using rdf_save_db/2 and update
%	the file administration.

rdf_load_db(File) :-
	uri_file_name(URL, File),
	rdf_load_db_no_admin(File, URL, Graphs),
	(   (   is_list(Graphs)
	    ->	member(DB, Graphs)
	    ;	DB = Graphs
	    ),
	    rdf_md5(DB, MD5),
	    rdf_statistics_(triples(DB, Triples)),
	    rdf_graph_source_(DB, SourceURL, Modified),
	    retractall(rdf_source(DB, _, _, _, _)),
	    assert(rdf_source(DB, SourceURL, Modified, Triples, MD5)),
	    fail
	;   true
	).


		 /*******************************
		 *	    LOADING RDF		*
		 *******************************/

:- multifile
	rdf_open_hook/8,
	rdf_open_decode/4,		% +Encoding, +File, -Stream, -Cleanup
	rdf_load_stream/3,		% +Format, +Stream, +Options
	rdf_file_type/2,		% ?Extension, ?Format
	rdf_storage_encoding/2,		% ?Extension, ?Encoding
	url_protocol/1.			% ?Protocol

%%	rdf_load(+FileOrList) is det.
%%	rdf_load(+FileOrList, +Options) is det.
%
%	Load RDF file.  Options provides additional processing options.
%	Currently defined options are:
%
%	    * blank_nodes(+ShareMode)
%	    How to handle equivalent blank nodes.  If =share= (default),
%	    equivalent blank nodes are shared in the same resource.
%
%	    * base_uri(+URI)
%	    URI that is used for rdf:about="" and other RDF constructs
%	    that are relative to the base uri.  Default is the source
%	    URL.
%
%	    * graph(?Graph)
%	    Named graph in which to load the data.  It is *not* allowed
%	    to load two sources into the same named graph.  If Graph is
%	    unbound, it is unified to the graph into which the data is
%	    loaded.
%
%	    * db(?Graph)
%	    Deprecated.  New code must use graph(Graph).
%
%	    * if(Condition)
%	    When to load the file. One of =true=, =changed= (default) or
%	    =not_loaded=.
%
%	    * modified(-Modified)
%	    Unify Modified with one of =not_modified=, cached(File),
%	    last_modified(Stamp) or =unknown=.
%
%	    * cache(Bool)
%	    If =false=, do not use or create a cache file.
%
%	    * register_namespaces(Bool)
%	    If =true= (default =false=), register xmlns= namespace
%	    declarations as ns/2 namespaces if there is no conflict.
%
%	Other options are forwarded to process_rdf/3.

rdf_load(Spec) :-
	rdf_load(Spec, []).

rdf_load([], _) :- !.
rdf_load([H|T], Options) :- !,
	rdf_load(H, Options),
	rdf_load(T, Options).
rdf_load(Spec, M:Options) :-
	must_be(list, Options),
	statistics(cputime, T0),
	rdf_open_input(Spec, In, Cleanup, SourceURL, Graph, Modified,
		       Format, Options),
	return_modified(Modified, Options),
	(   Modified == not_modified
	->  Action = none
	;   Modified = cached(CacheFile)
	->  do_unload(Graph),
	    catch(rdf_load_db_no_admin(CacheFile, cache(Graph), Graphs), _, fail),
	    check_loaded_cache(Graph, Graphs, Modified),
	    rdf_statistics_(triples(Graph, Triples)),
	    Action = load
	;   option(base_uri(BaseURI), Options, Graph),
	    (	var(BaseURI)
	    ->	BaseURI = SourceURL
	    ;	true
	    ),
	    once(phrase(derived_options(Options, NSList), Extra)),
	    merge_options([ base_uri(BaseURI),
			    graph(Graph),
			    format(Format)
			  | Extra
			  ], Options, RDFOptions),
	    do_unload(Graph),
	    graph_modified(Modified, ModifiedStamp),
	    rdf_set_graph_source(Graph, SourceURL, ModifiedStamp),
	    call_cleanup(rdf_load_stream(Format, In, M:RDFOptions),
			 Cleanup),
	    save_cache(Graph, SourceURL, Options),
	    register_file_ns(NSList),
	    rdf_statistics_(triples(Graph, Triples)),
	    rdf_md5(Graph, MD5),
	    assert(rdf_source(Graph, SourceURL, Modified, Triples, MD5)),
	    format_action(Format, Action)
	),
	report_loaded(Action, SourceURL, Graph, Triples, T0, Options).

format_action(triples, load) :- !.
format_action(_, parsed).

save_cache(Graph, SourceURL, Options) :-
	option(cache(true), Options, true),
	rdf_cache_file(SourceURL, write, CacheFile), !,
	catch(save_cache(Graph, CacheFile), E,
	      print_message(warning, E)).
save_cache(_, _, _).

derived_options([], _) -->
	[].
derived_options([H|T], NSList) -->
	(   {   H == register_namespaces(true)
	    ;   H == (register_namespaces = true)
	    }
	->  [ namespaces(NSList) ]
	;   []
	),
	derived_options(T, NSList).

graph_modified(last_modified(Stamp), Stamp).
graph_modified(unknown, Stamp) :-
	get_time(Stamp).

return_modified(Modified, Options) :-
	option(modified(M0), Options), !,
	M0 = Modified.
return_modified(_, _).


		 /*******************************
		 *	  INPUT HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This section deals with pluggable input sources.  The task of the input
layer is

    * Decide on the graph-name
    * Decide on the source-location
    * Decide whether loading is needed (if-modified)
    * Decide on the serialization in the input

The protocol must ensure minimal  overhead,   in  particular for network
protocols. E.g. for HTTP we want to make a single call on the server and
use If-modified-since to verify that we need not reloading this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	rdf_open_input(+Spec, -Stream, -Cleanup,
%%		       -Source, -Graph, -Modified, -Format,
%%		       +Options)
%
%	Open an input source.
%
%	Options processed:
%
%	    * graph(Graph)
%	    * db(Graph)
%	    * if(Condition)
%	    * cache(Cache)
%	    * format(Format)
%
%	@param	Modified is one of =not_modified=, last_modified(Time),
%		cached(CacheFile) or =unknown=

rdf_open_input(Spec, Stream, Cleanup,
	       SourceURL, Graph, Modified, Format,
	       Options) :-
	source_url(Spec, Protocol, SourceURL),
	load_graph(SourceURL, Graph, Options),
	option(if(If), Options, changed),
	(   If == true
	->  true
	;   rdf_graph_source_(Graph, SourceURL, HaveModified)
	->  true
	;   option(cache(true), Options, true),
	    rdf_cache_file(SourceURL, read, CacheFile)
	->  time_file(CacheFile, HaveModified)
	;   true
	),
	option(format(Format), Options, _),
	open_input_if_modified(Protocol, SourceURL, HaveModified,
			       Stream, Cleanup, Modified0, Format, Options),
	(   Modified0 == not_modified
	->  (   nonvar(CacheFile)
	    ->	Modified = cached(CacheFile)
	    ;	Modified = not_modified
	    )
	;   Modified = Modified0
	).


%%	source_url(+Spec, -Class, -SourceURL) is det.
%
%	Determine class and url of the source.  Class is one of
%
%	    * stream(Stream)
%	    * file
%	    * a url-protocol (e.g., =http=)

source_url(stream(In), stream(In), SourceURL) :- !,
	(   stream_property(In, file_name(File))
	->  to_url(File, SourceURL)
	;   gensym('stream://', SourceURL)
	).
source_url(Stream, Class, SourceURL) :-
	is_stream(Stream), !,
	source_url(stream(Stream), Class, SourceURL).
source_url(Spec, Protocol, SourceURL) :-
	compound(Spec), !,
	source_file(Spec, Protocol, SourceURL).
source_url(FileURL, Protocol, SourceURL) :-		% or return FileURL?
	uri_file_name(FileURL, File), !,
	source_file(File, Protocol, SourceURL).
source_url(SourceURL, Protocol, SourceURL) :-
	is_url(SourceURL, Protocol), !.
source_url(File, Protocol, SourceURL) :-
	source_file(File, Protocol, SourceURL).

source_file(Spec, file(SExt), SourceURL) :-
	findall(Ext, valid_extension(Ext), Exts),
	absolute_file_name(Spec, File, [access(read), extensions(Exts)]),
	storage_extension(Plain, SExt, File),
	uri_file_name(SourceURL, Plain).

to_url(URL, URL) :-
	is_url(URL, _Protocol), !.
to_url(File, URL) :-
	absolute_file_name(File, Path),
	uri_file_name(URL, Path).

storage_extension(Plain, SExt, File) :-
	file_name_extension(Plain, SExt, File),
	SExt \== '',
	rdf_storage_encoding(SExt, _), !.
storage_extension(File, '', File).

%%	rdf_input(URL, Source, _BaseURI) is semidet.
%
%	@deprecated Only exists to make old ClioPatria rdf_library.pl
%	work

rdf_input(Spec, Source, _BaseURI) :-
	source_url(Spec, Class, SourceURL),
	compat_input(Class, SourceURL, Source).

compat_input(file(Ext), SourceURL, file(Path)) :-
	uri_file_name(SourceURL, File),
	file_name_extension(File, Ext, Path).
compat_input(http, SourceURL, url(http, SourceURL)).

%%	load_graph(+SourceURL, -Graph, +Options) is det.
%
%	Graph is the graph into which we load the data.  Processes
%	the graph(?Graph) option.

load_graph(Source, Graph, Options) :-
	(   option(graph(Graph), Options)
	->  true
	;   option(db(Graph), Options)
	), !,
	(   ground(Graph)
	->  true
	;   load_graph(Source, Graph)
	).
load_graph(Source, Graph, _) :-
	load_graph(Source, Graph).

load_graph(SourceURL, BaseURI) :-
	file_name_extension(BaseURI, Ext, SourceURL),
	rdf_storage_encoding(Ext, _), !.
load_graph(SourceURL, SourceURL).


open_input_if_modified(stream(In), SourceURL, _, In, true,
		       unknown, Format, _) :- !,
	(   var(Format)
	->  guess_format(SourceURL, Format)
	;   true
	).
open_input_if_modified(file(SExt), SourceURL, HaveModified, Stream, Cleanup,
		       Modified, Format, _) :- !,
	uri_file_name(SourceURL, File0),
	file_name_extension(File0, SExt, File),
	time_file(File, LastModified),
	(   nonvar(HaveModified),
	    HaveModified >= LastModified
	->  Modified = not_modified,
	    Cleanup = true
	;   storage_open(SExt, File, Stream, Cleanup),
	    Modified = last_modified(LastModified),
	    (	var(Format)
	    ->	guess_format(File0, Format)
	    ;	true
	    )
	).
open_input_if_modified(file, SourceURL, HaveModified, Stream, Cleanup,
		       Modified, Format, Options) :- !,
	open_input_if_modified(file(''), SourceURL, HaveModified,
			       Stream, Cleanup,
			       Modified, Format, Options).
open_input_if_modified(Protocol, SourceURL, HaveModified, Stream, Cleanup,
		       Modified, Format, Options) :-
	rdf_open_hook(Protocol, SourceURL, HaveModified, Stream, Cleanup,
		      Modified, Format, Options).

guess_format(File, Format) :-
	file_name_extension(_, Ext, File),
	(   rdf_file_type(Ext, Format)
	->  true
	;   Format = xml,
	    print_message(warning, rdf(guess_format(Ext)))
	).

%%	storage_open(+Extension, +File, -Stream, -Cleanup)
%
%	Open the low-level storage. Note  that   the  file  is opened as
%	binary. This is the same  as   for  HTTP  resources. The correct
%	encoding will be set by the XML parser or the Turtle parser.

storage_open('', File, Stream, close(Stream)) :- !,
	open(File, read, Stream, [type(binary)]).
storage_open(Ext, File, Stream, Cleanup) :-
	rdf_storage_encoding(Ext, Encoding),
	rdf_open_decode(Encoding, File, Stream, Cleanup).

valid_extension(Ext) :-
	rdf_file_type(Ext, _).
valid_extension(Ext) :-
	rdf_storage_encoding(Ext, _).

%%	is_url(+Term, -Protocol) is semidet.
%
%	True if Term is an atom denoting a URL of the given Protocol.
%	We only support a limited set of protocols as defined by the
%	extensible predicate url_protocol/1.

is_url(URL, Protocol) :-
	atom(URL),
	sub_atom(URL, B, _, _, :), !,
	sub_atom(URL, 0, B, _, RawProtocol),
	downcase_atom(RawProtocol, Protocol),
	url_protocol(Protocol).

url_protocol(file).			% built-in

%%	rdf_file_type(+Extension, -Format) is semidet.
%
%	True if Format  is  the  format   belonging  to  the  given file
%	extension.  This predicate is multifile and can thus be extended
%	by plugins.

rdf_file_type(xml,   xml).
rdf_file_type(rdf,   xml).
rdf_file_type(rdfs,  xml).
rdf_file_type(owl,   xml).
rdf_file_type(htm,   xhtml).
rdf_file_type(html,  xhtml).
rdf_file_type(xhtml, xhtml).
rdf_file_type(trp,   triples).


%%	rdf_file_encoding(+Extension, -Format) is semidet.
%
%	True if Format describes the storage encoding of file.

rdf_storage_encoding('', plain).


%%	rdf_load_stream(+Format, +Stream, :Options)
%
%	Load RDF data from Stream.
%
%	@tbd	Handle mime-types?

rdf_load_stream(xml, Stream, Options) :- !,
	graph(Options, Graph),
	rdf_transaction(process_rdf(Stream, assert_triples, Options),
			parse(Graph)).
rdf_load_stream(xhtml, Stream, M:Options) :- !,
	graph(Options, Graph),
	rdf_transaction(process_rdf(Stream, assert_triples,
				    M:[embedded(true)|Options]),
			parse(Graph)).
rdf_load_stream(triples, Stream, Options) :- !,
	graph(Options, Graph),
	rdf_load_db_(Stream, Graph, _Graphs).


%%	report_loaded(+Action, +Source, +DB, +Triples, +StartCPU, +Options)

report_loaded(none, _, _, _, _, _) :- !.
report_loaded(Action, Source, DB, Triples, T0, Options) :-
	statistics(cputime, T1),
	Time is T1 - T0,
	(   option(silent(true), Options)
	->  Level = silent
	;   Level = informational
	),
	print_message(Level,
		      rdf(loaded(Action, Source, DB, Triples, Time))).


%%	rdf_unload(+Spec) is det.
%
%	Remove the triples loaded from the specified source and remove
%	the source from the database.

rdf_unload(Graph) :-
	atom(Graph),
	rdf_statistics_(triples(Graph, Triples)),
	Triples > 0, !,
	do_unload(Graph).
rdf_unload(Spec) :-
	source_url(Spec, _Protocol, SourceURL),
	rdf_graph_source_(Graph, SourceURL, _), !,
	do_unload(Graph).
rdf_unload(_).

do_unload(DB) :-
	rdf_transaction(rdf_retractall(_,_,_,DB),
			unload(DB)),
	retractall(rdf_source(DB, _, _, _, _)),
	rdf_unset_graph_source(DB).


%%	rdf_graph(+DB) is semidet.
%%	rdf_graph(-DB) is nondet.
%
%	True if DB is a current named graph with at least one triple.

rdf_graph(DB) :-
	atom(DB), !,
	rdf_statistics_(triples(DB, Triples)),
	Triples > 0.
rdf_graph(DB) :-
	rdf_graphs_(Sources),
	member(DB, Sources),
	rdf_statistics_(triples(DB, Triples)),
	Triples > 0.

%%	rdf_source(?Graph, ?SourceURL) is nondet.
%
%	True if named Graph is loaded from SourceURL.
%
%	@deprecated Use rdf_graph_property(Graph, source(SourceURL)).

rdf_source(Graph, SourceURL) :-
	rdf_graph(Graph),
	rdf_graph_source_(Graph, SourceURL, _Modified).

%%	rdf_source(?Source)
%
%	True if Source is a loaded source.
%
%	@deprecated	Use rdf_graph/1 or rdf_source/2.

rdf_source(SourceURL) :-
	rdf_source(_Graph, SourceURL).

%%	rdf_make
%
%	Reload all loaded files that have been modified since the last
%	time they were loaded.

rdf_make :-
	findall(Source-Graph, modified_graph(Source, Graph), Modified),
	forall(member(Source-Graph, Modified),
	       catch(rdf_load(Source, [graph(Graph), if(changed)]), E,
		     print_message(error, E))).

modified_graph(SourceURL, Graph) :-
	rdf_graph(Graph),
	rdf_graph_source_(Graph, SourceURL, Modified),
	\+ sub_atom(SourceURL, 0, _, _, 'stream://'),
	Modified > 0.

%%	rdf_graph_property(?Graph, ?Property) is nondet.
%
%	True when Property is a property of Graph.  Defined properties
%	are:
%
%	    * hash(Hash)
%	    Hash is the (MD5-)hash for the content of Graph.
%	    * source(Source)
%	    The graph is loaded from the Source (a URL)
%	    * source_last_modified(?Time)
%	    Time is the last-modified timestamp of Source at the moment
%	    that the graph was loaded from Source.
%	    * triples(Count)
%	    True when Count is the number of triples in Graph.

rdf_graph_property(Graph, Property) :-
	rdf_graph(Graph),
	rdf_graph_property_(Property, Graph).

rdf_graph_property_(hash(Hash), Graph) :-
	rdf_md5(Graph, Hash).
rdf_graph_property_(source(URL), Graph) :-
	rdf_graph_source_(Graph, URL, _).
rdf_graph_property_(source_last_modified(Time), Graph) :-
	rdf_graph_source_(Graph, _, Time),
	Time > 0.0.
rdf_graph_property_(triples(Count), Graph) :-
	rdf_statistics_(triples(Graph, Count)).


%%	save_cache(+DB, +Cache) is det.
%
%	Save triples belonging to DB in the file Cache.

save_cache(DB, Cache) :-
	catch(open(Cache, write, CacheStream, [type(binary)]), _, fail), !,
	call_cleanup(rdf_save_db_(CacheStream, DB),
		     close(CacheStream)).

%%	assert_triples(+Triples, +Source)
%
%	Assert a list of triples into the database. Foir security
%	reasons we check we aren't inserting anything but nice RDF
%	triples.

assert_triples([], _).
assert_triples([rdf(S,P,O)|T], DB) :- !,
	rdf_assert(S, P, O, DB),
	assert_triples(T, DB).
assert_triples([H|_], _) :-
	throw(error(type_error(rdf_triple, H), _)).


		 /*******************************
		 *	       RESET		*
		 *******************************/

%%	rdf_reset_db
%
%	Remove all triples from the RDF database and reset all its
%	statistics.

rdf_reset_db :-
	retractall(rdf_source(_,_,_,_,_)),
	rdf_transaction(rdf_reset_db_, reset).


		 /*******************************
		 *	     SAVE RDF		*
		 *******************************/

%%	rdf_save(+Out) is det.
%%	rdf_save(+Out, :Options) is det.
%
%	Write RDF data as RDF/XML. Options is a list of one or more of
%	the following options:
%
%		* graph(+Graph)
%		Save only triples associated to the given named Graph.
%
%		* db(+DB)
%		Deprecated synonym for graph(DB).
%
%		* anon(Bool)
%		If false (default true) do not save blank nodes that do
%		not appear (indirectly) as object of a named resource.
%
%		* base_uri(URI)
%		BaseURI used. If present, all URIs that can be
%		represented relative to this base are written using
%		their shorthand.  See also =write_xml_base= option
%
%		* write_xml_base(Bool)
%		If =false=, do _not_ include the =|xml:base|=
%		declaration that is written normally when using the
%		=base_uri= option.
%
%		* convert_typed_literal(:Convertor)
%		Call Convertor(-Type, -Content, +RDFObject), providing
%		the opposite for the convert_typed_literal option of
%		the RDF parser.
%
%		* encoding(Encoding)
%		Encoding for the output.  Either utf8 or iso_latin_1
%
%		* document_language(+Lang)
%		Initial xml:lang saved with rdf:RDF element
%
%		* sorted(+Boolean)
%		If =true= (default =false=), emit subjects sorted on
%		the full URI.  Useful to make file comparison easier.
%
%	@param Out	Location to save the data.  This can also be a
%			file-url (=|file://path|=) or a stream wrapped
%			in a term stream(Out).

:- thread_local
	named_anon/2.			% +Resource, -Id

rdf_save(File) :-
	rdf_save2(File, []).

rdf_save(Spec, M:Options0) :-
	is_list(Options0), !,
	meta_options(save_meta_option, M:Options0, Options),
	to_file(Spec, File),
	rdf_save2(File, Options).
rdf_save(Spec, _:DB) :-
	atom(DB), !,			% backward compatibility
	to_file(Spec, File),
	rdf_save2(File, [graph(DB)]).

save_meta_option(convert_typed_literal).

to_file(URL, File) :-
	atom(URL),
	uri_file_name(URL, File), !.
to_file(File, File).

rdf_save2(File, Options) :-
	option(encoding(Encoding), Options, utf8),
	valid_encoding(Encoding),
	open_output(File, Encoding, Out, Close),
	flag(rdf_db_saved_subjects, OSavedSubjects, 0),
	flag(rdf_db_saved_triples, OSavedTriples, 0),
	call_cleanup(rdf_do_save(Out, Options),
		     Reason,
		     cleanup_save(Reason,
				  File,
				  OSavedSubjects,
				  OSavedTriples,
				  Close)).

open_output(stream(Out), Encoding, Out,
	    set_stream(Out, encoding(Old))) :- !,
	stream_property(Out, encoding(Old)),
	set_stream(Out, encoding(Encoding)).
open_output(File, Encoding, Out,
	    close(Out)) :-
	open(File, write, Out, [encoding(Encoding)]).

valid_encoding(Enc) :-
	(   xml_encoding_name(Enc, _)
	->  true
	;   throw(error(domain_error(encoding, Enc), _))
	).


cleanup_save(Reason,
	     File,
	     OSavedSubjects,
	     OSavedTriples,
	     Close) :-
	call(Close),
	flag(rdf_db_saved_subjects, SavedSubjects, OSavedSubjects),
	flag(rdf_db_saved_triples, SavedTriples, OSavedTriples),
	retractall(named_anon(_, _)),
	(   Reason == exit
	->  print_message(informational,
			  rdf(saved(File, SavedSubjects, SavedTriples)))
	;   format(user_error, 'Reason = ~w~n', [Reason])
	).

rdf_do_save(Out, Options0) :-
	rdf_save_header(Out, Options0, Options),
	(   option(sorted(true), Options, false)
	->  setof(Subject, rdf_subject(Subject, Options), Subjects),
	    forall(member(Subject, Subjects),
		   rdf_save_non_anon_subject(Out, Subject, Options))
	;   forall(rdf_subject(Subject, Options),
		   rdf_save_non_anon_subject(Out, Subject, Options))
	),
	rdf_save_footer(Out), !.	% dubious cut; without the
					% cleanup handlers isn't called!?

rdf_subject(Subject, Options) :-
	graph(Options, DB),
	var(DB), !,
	rdf_subject(Subject).
rdf_subject(Subject, Options) :-
	graph(Options, DB),
	rdf_subject(Subject),
	(   rdf(Subject, _, _, DB:_)
	->  true
	).

graph(Options0, DB) :-
	strip_module(Options0, _, Options),
	(   memberchk(graph(DB0), Options)
	->  DB = DB0
	;   memberchk(db(DB0), Options)
	->  DB = DB0
	;   true			% leave unbound
	).


%%	rdf_save_header(+Fd, +Options)
%
%	Save XML document header, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_save_header(Out, Options) :-
	rdf_save_header(Out, Options, _).

rdf_save_header(Out, Options, OptionsOut) :-
	is_list(Options), !,
	stream_property(Out, encoding(Enc)),
	xml_encoding(Enc, Encoding),
	format(Out, '<?xml version=\'1.0\' encoding=\'~w\'?>~n', [Encoding]),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	header_namespaces(Options, NSIdList),
	nsmap(NSIdList, NsMap),
	append(Options, [nsmap(NsMap)], OptionsOut),
	forall(member(Id=URI, NsMap),
	       (   rdf_quote_uri(URI, QURI),
		   xml_quote_attribute(QURI, NSText0, Enc),
		   xml_escape_parameter_entity(NSText0, NSText),
		   format(Out, '~N    <!ENTITY ~w \'~w\'>', [Id, NSText])
	       )),
	format(Out, '~N]>~n~n', []),
	format(Out, '<rdf:RDF', []),
	(   member(Id, NSIdList),
	    format(Out, '~N    xmlns:~w="&~w;"~n', [Id, Id]),
	    fail
	;   true
	),
	(   option(base_uri(Base), Options),
	    option(write_xml_base(true), Options, true)
	->  rdf_quote_uri(Base, QBase),
	    xml_quote_attribute(QBase, BaseText, Enc),
	    format(Out, '~N    xml:base="~w"~n', [BaseText])
	;   true
	),
	(   memberchk(document_language(Lang), Options)
	->  format(Out, '~N    xml:lang="~w"', [Lang])
	;   true
	),
	format(Out, '>~n', []).
rdf_save_header(Out, FileRef, OptionsOut) :-	% compatibility
	atom(FileRef),
	rdf_save_header(Out, [graph(FileRef)], OptionsOut).

xml_encoding(Enc, Encoding) :-
	(   xml_encoding_name(Enc, Encoding)
	->  true
	;   throw(error(domain_error(rdf_encoding, Enc), _))
	).

xml_encoding_name(ascii,       'US-ASCII').
xml_encoding_name(iso_latin_1, 'ISO-8859-1').
xml_encoding_name(utf8,        'UTF-8').

%%	nsmap(+NSIds, -Map:list(id=uri)) is det.
%
%	Create a namespace-map that is compatible to xml_write/2
%	for dealing with XML-Literals

nsmap([], []).
nsmap([Id|T0], [Id=URI|T]) :-
	ns(Id, URI),
	nsmap(T0, T).

%%	xml_escape_parameter_entity(+In, -Out) is det.
%
%	Escape % as &#37; for entity declarations.

xml_escape_parameter_entity(In, Out) :-
	sub_atom(In, _, _, _, '%'), !,
	atom_codes(In, Codes),
	phrase(escape_parent(Codes), OutCodes),
	atom_codes(Out, OutCodes).
xml_escape_parameter_entity(In, In).

escape_parent([]) --> [].
escape_parent([H|T]) -->
	(   { H == 37 }
	->  "&#37;"
	;   [H]
	),
	escape_parent(T).


%%	header_namespaces(Options, -List)
%
%	Get namespaces we will define as entities

header_namespaces(Options, List) :-
	memberchk(namespaces(NSL0), Options), !,
	sort([rdf,rdfs|NSL0], List).
header_namespaces(Options, List) :-
	graph(Options, DB),
	used_namespace_entities(List, DB).

%%	rdf_graph_prefixes(?Graph, -List:ord_set) is det.
%%	rdf_graph_prefixes(?Graph, -List:ord_set, :Options) is det.
%
%	List is a sorted list of  prefixes (namepaces) in Graph. Options
%	defined are:
%
%	    * filter(:Filter)
%	    optional Filter argument is used to filter the results. It
%	    is called with 3 additional arguments:
%
%	        ==
%	        call(Filter, Where, Prefix, URI)
%	        ==
%
%	    The Where argument gives the location of the prefix ans is
%	    one of =subject=, =predicate=, =object= or =type=. The
%	    Prefix argument is the potentionally new prefix and URI is
%	    the full URI that is being processed.
%
%	    * expand(:Goal)
%	    Hook to generate the graph.  Called using
%
%	        ==
%	        call(Goal,S,P,O,Graph)
%	        ==
%
%	    * min_count(+Count)
%	    Only include prefixes that appear at least N times.  Default
%	    is 1. Declared prefixes are always returned if found at
%	    least one time.


:- thread_local
	graph_prefix/3.
:- meta_predicate
	rdf_graph_prefixes(?, -, :).

rdf_graph_prefixes(Graph, List) :-
	rdf_graph_prefixes(Graph, List, []).

rdf_graph_prefixes(Graph, List, M:QOptions) :-
	is_list(QOptions), !,
	meta_options(is_meta, M:QOptions, Options),
	option(filter(Filter), Options, true),
	option(expand(Expand), Options, rdf_db),
	option(min_count(MinCount), Options, 1),
	call_cleanup(prefixes(Expand, Graph, Prefixes, Filter, MinCount),
		     retractall(graph_prefix(_,_,_))),
	sort(Prefixes, List).
rdf_graph_prefixes(Graph, List, M:Filter) :-
	rdf_graph_prefixes(Graph, List, M:[filter(Filter)]).

is_meta(filter).
is_meta(expand).


prefixes(Expand, Graph, Prefixes, Filter, MinCount) :-
	(   call(Expand, S, P, O, Graph),
	    add_ns(subject, Filter, S, MinCount, s(S)),
	    add_ns(predicate, Filter, P, MinCount, sp(S,P)),
	    add_ns_obj(Filter, O, MinCount, spo(S,P,O)),
	    fail
	;   true
	),
	findall(Prefix, graph_prefix(Prefix, MinCount, _), Prefixes).

add_ns(Where, Filter, S, MinCount, Context) :-
	\+ rdf_is_bnode(S),
	iri_xml_namespace(S, Full),
	Full \== '', !,
	(   graph_prefix(Full, MinCount, _)
	->  true
	;   Filter == true
	->  add_ns(Full, Context)
	;   call(Filter, Where, Full, S)
	->  add_ns(Full, Context)
	;   true
	).
add_ns(_, _, _, _, _).

add_ns(Full, Context) :-
	graph_prefix(Full, _, Contexts),
	memberchk(Context, Contexts), !.
add_ns(Full, Context) :-
	retract(graph_prefix(Full, C0, Contexts)), !,
	C1 is C0+1,
	asserta(graph_prefix(Full, C1, [Context|Contexts])).
add_ns(Full, _) :-
	ns(_, Full), !,
	asserta(graph_prefix(Full, _, _)).
add_ns(Full, Context) :-
	asserta(graph_prefix(Full, 1, [Context])).


add_ns_obj(Filter, O, MinCount, Context) :-
	atom(O), !,
	add_ns(object, Filter, O, MinCount, Context).
add_ns_obj(Filter, literal(type(Type, _)), MinCount, _) :-
	atom(Type), !,
	add_ns(type, Filter, Type, MinCount, t(Type)).
add_ns_obj(_, _, _, _).


%%	used_namespace_entities(-List, ?Graph) is det.
%
%	Return the namespace aliases that are actually used in Graph. In
%	addition, this predicate creates ns<N>   aliases  for namespaces
%	used in predicates because RDF/XML cannot write predicates other
%	than as an XML name.

used_namespace_entities(List, Graph) :-
	decl_used_predicate_ns(Graph),
	used_namespaces(List, Graph).

used_namespaces(List, DB) :-
	rdf_graph_prefixes(DB, FullList),
	ns_abbreviations(FullList, List0),
	sort([rdf|List0], List).

ns_abbreviations([], []).
ns_abbreviations([H0|T0], [H|T]) :-
	ns(H, H0), !,
	ns_abbreviations(T0, T).
ns_abbreviations([_|T0], T) :-
	ns_abbreviations(T0, T).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For every URL used as a predicate  we   *MUST*  define a namespace as we
cannot use names holding /, :, etc. as XML identifiers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
	predicate_ns/2.

decl_used_predicate_ns(DB) :-
	retractall(predicate_ns(_,_)),
	(   rdf_current_predicate(P, DB),
	    decl_predicate_ns(P),
	    fail
	;   true
	).

decl_predicate_ns(Pred) :-
	predicate_ns(Pred, _), !.
decl_predicate_ns(Pred) :-
	rdf_global_id(NS:Local, Pred),
	xml_name(Local), !,
	assert(predicate_ns(Pred, NS)).
decl_predicate_ns(Pred) :-
	atom_codes(Pred, Codes),
	append(NSCodes, LocalCodes, Codes),
	xml_codes(LocalCodes), !,
	(   NSCodes \== []
	->  atom_codes(NS, NSCodes),
	    (   ns(Id, NS)
	    ->	assert(predicate_ns(Pred, Id))
	    ;	between(1, infinite, N),
		atom_concat(ns, N, Id),
		\+ ns(Id, _)
	    ->  rdf_register_ns(Id, NS),
		print_message(informational,
			      rdf(using_namespace(Id, NS)))
	    ),
	    assert(predicate_ns(Pred, Id))
	;   assert(predicate_ns(Pred, -)) % no namespace used
	).

xml_codes([]).
xml_codes([H|T]) :-
	xml_code(H),
	xml_codes(T).

xml_code(X) :-
	code_type(X, csym), !.
xml_code(0'-).				% Match 0'-


%%	rdf_save_footer(Out:stream) is det.
%
%	Finish XML generation and write the document footer.

rdf_save_footer(Out) :-
	retractall(named_anon(_, _)),
	format(Out, '</rdf:RDF>~n', []).

%%	rdf_save_non_anon_subject(+Out, +Subject, +Options)
%
%	Save an object.  Anonymous objects not saved if anon(false)
%	is present in the Options list.

rdf_save_non_anon_subject(_Out, Subject, Options) :-
	rdf_is_bnode(Subject),
	(   memberchk(anon(false), Options)
	;   graph(Options, DB),
	    rdf_db(_, _, Subject, DB)
	), !.
rdf_save_non_anon_subject(Out, Subject, Options) :-
	rdf_save_subject(Out, Subject, Options),
	flag(rdf_db_saved_subjects, X, X+1).


%%	rdf_save_subject(+Out, +Subject:resource, +Options) is det.
%
%	Save the triples associated to Subject to Out.

rdf_save_subject(Out, Subject, Options) :-
	is_list(Options), !,
	option(base_uri(BaseURI), Options, '-'),
	(   rdf_save_subject(Out, Subject, BaseURI, 0, Options)
	->  format(Out, '~n', [])
	;   throw(error(rdf_save_failed(Subject), 'Internal error'))
	).
rdf_save_subject(Out, Subject, DB) :-
	(   var(DB)
	->  rdf_save_subject(Out, Subject, [])
	;   rdf_save_subject(Out, Subject, [graph(DB)])
	).


%%	rdf_save_subject(+Out:stream, +Subject:resource, +BaseURI,
%%			 +Indent:int, +Options) is det.
%
%	Save properties of Subject.
%
%	@param Indent	Current indentation

rdf_save_subject(Out, Subject, BaseURI, Indent, Options) :-
	graph(Options, DB),
	findall(Pred=Object, rdf_db(Subject, Pred, Object, DB), Atts0),
	sort(Atts0, Atts),		% remove duplicates
	length(Atts, L),
	(   length(Atts0, L0),
	    Del is L0-L,
	    Del > 0
	->  print_message(informational,
			  rdf(save_removed_duplicates(Del, Subject)))
	;   true
	),
	rdf_save_subject(Out, Subject, BaseURI, Atts, Indent, Options),
	flag(rdf_db_saved_triples, X, X+L).

rdf_db(Subject, Pred, Object, DB) :-
	var(DB), !,
	rdf(Subject, Pred, Object).
rdf_db(Subject, Pred, Object, DB) :-
	rdf(Subject, Pred, Object, DB:_).

%%	rdf_save_subject(+Out:stream, +Subject:resource, +BaseURI,
%%			 +Atts:list(Pred=Obj), +Indent:int, +Options) is det.
%
%	Save triples defined by Atts on Subject.

rdf_save_subject(Out, Subject, BaseURI, Atts, Indent, Options) :-
	rdf_equal(rdf:type, RdfType),
	select(RdfType=Type, Atts, Atts1),
	\+ rdf_is_bnode(Type),
	rdf_id(Type, BaseURI, TypeId),
	xml_is_name(TypeId), !,
	format(Out, '~*|<', [Indent]),
	rdf_write_id(Out, TypeId),
	save_about(Out, BaseURI, Subject),
	save_attributes(Atts1, BaseURI, Out, TypeId, Indent, Options).
rdf_save_subject(Out, Subject, BaseURI, Atts, Indent, Options) :-
	format(Out, '~*|<rdf:Description', [Indent]),
	save_about(Out, BaseURI, Subject),
	save_attributes(Atts, BaseURI, Out, rdf:'Description', Indent, Options).

xml_is_name(_NS:Atom) :- !,
	xml_name(Atom).
xml_is_name(Atom) :-
	xml_name(Atom).

%%	save_about(+Out, +BaseURI, +Subject) is det.
%
%	Save the rdf:about. If Subject is a  blank node, save the nodeID
%	if any.

save_about(Out, _, Subject) :-
	rdf_is_bnode(Subject), !,
	(   named_anon(Subject, NodeID)
	->  format(Out, ' rdf:nodeID="~w"', [NodeID])
	;   true
	).
save_about(Out, BaseURI, Subject) :-
	stream_property(Out, encoding(Encoding)),
	rdf_value(Subject, BaseURI, QSubject, Encoding),
	format(Out, ' rdf:about="~w"', [QSubject]).

%%	save_attributes(+List, +BaseURI, +Stream, Element)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Atts, BaseURI, Out, Element, Indent, Options) :-
	split_attributes(Atts, InTag, InBody),
	SubIndent is Indent + 2,
	save_attributes2(InTag, BaseURI, tag, Out, SubIndent, Options),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, BaseURI, body, Out, SubIndent, Options),
	    format(Out, '~N~*|</', [Indent]),
	    rdf_write_id(Out, Element),
	    format(Out, '>~n', [])
	).

%%	split_attributes(+Attributes, -HeadAttrs, -BodyAttr)
%
%	Split attribute (Name=Value) list into attributes for the head
%	and body. Attributes can only be in the head if they are literal
%	and appear only one time in the attribute list.

split_attributes(Atts, HeadAttr, BodyAttr) :-
	duplicate_attributes(Atts, Dupls, Singles),
	simple_literal_attributes(Singles, HeadAttr, Rest),
	append(Dupls, Rest, BodyAttr).

%%	duplicate_attributes(+Attrs, -Duplicates, -Singles)
%
%	Extract attributes that appear more than onces as we cannot
%	dublicate an attribute in the head according to the XML rules.

duplicate_attributes([], [], []).
duplicate_attributes([H|T], Dupls, Singles) :-
	H = (Name=_),
	named_attributes(Name, T, D, R),
	D \== [],
	append([H|D], Dupls2, Dupls), !,
	duplicate_attributes(R, Dupls2, Singles).
duplicate_attributes([H|T], Dupls2, [H|Singles]) :-
	duplicate_attributes(T, Dupls2, Singles).

named_attributes(_, [], [], []) :- !.
named_attributes(Name, [H|T], D, R) :-
	(   H = (Name=_)
	->  D = [H|DT],
	    named_attributes(Name, T, DT, R)
	;   R = [H|RT],
	    named_attributes(Name, T, D, RT)
	).

%%	simple_literal_attributes(+Attributes, -Inline, -Body)
%
%	Split attributes for (literal) attributes to be used in the
%	begin-tag and ones that have to go into the body of the description.

simple_literal_attributes([], [], []).
simple_literal_attributes([H|TA], [H|TI], B) :-
	in_tag_attribute(H), !,
	simple_literal_attributes(TA, TI, B).
simple_literal_attributes([H|TA], I, [H|TB]) :-
	simple_literal_attributes(TA, I, TB).

in_tag_attribute(_=literal(Text)) :-
	atom(Text),			% may not have lang qualifier
	atom_length(Text, Len),
	Len < 60.

%%	save_attributes(+List, +BaseURI, +TagOrBody, +Stream)
%
%	Save a list of attributes.

save_attributes2([], _, _, _, _, _).
save_attributes2([H|T], BaseURI, Where, Out, Indent, Options) :-
	save_attribute(Where, H, BaseURI, Out, Indent, Options),
	save_attributes2(T, BaseURI, Where, Out, Indent, Options).

save_attribute(tag, Name=literal(Value), BaseURI, Out, Indent, _DB) :-
	AttIndent is Indent + 2,
	rdf_id(Name, BaseURI, NameText),
	stream_property(Out, encoding(Encoding)),
	xml_quote_attribute(Value, QVal, Encoding),
	format(Out, '~N~*|', [AttIndent]),
	rdf_write_id(Out, NameText),
	format(Out, '="~w"', [QVal]).
save_attribute(body, Name=literal(Literal0), BaseURI, Out, Indent, Options) :- !,
	rdf_id(Name, BaseURI, NameText),
	(   memberchk(convert_typed_literal(Converter), Options),
	    call(Converter, Type, Content, Literal0)
	->  Literal = type(Type, Content)
	;   Literal = Literal0
	),
	save_body_literal(Literal, NameText, BaseURI, Out, Indent, Options).
save_attribute(body, Name=Value, BaseURI, Out, Indent, Options) :-
	rdf_is_bnode(Value), !,
	rdf_id(Name, BaseURI, NameText),
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	(   named_anon(Value, NodeID)
	->  format(Out, ' rdf:nodeID="~w"/>', [NodeID])
	;   (   rdf(S1, Name, Value),
	        rdf(S2, P2, Value),
		(S1 \== S2 ; Name \== P2)
	    ->  predicate_property(named_anon(_,_), number_of_clauses(N)),
		atom_concat('bn', N, NodeID),
		assert(named_anon(Value, NodeID))
	    ;	true
	    ),
	    SubIndent is Indent + 2,
	    (   rdf_collection(Value)
	    ->  save_about(Out, BaseURI, Value),
		format(Out, ' rdf:parseType="Collection">~n', []),
		rdf_save_list(Out, Value, BaseURI, SubIndent, Options)
	    ;   format(Out, '>~n', []),
		rdf_save_subject(Out, Value, BaseURI, SubIndent, Options)
	    ),
	    format(Out, '~N~*|</', [Indent]),
	    rdf_write_id(Out, NameText),
	    format(Out, '>~n', [])
	).
save_attribute(body, Name=Value, BaseURI, Out, Indent, _DB) :-
	stream_property(Out, encoding(Encoding)),
	rdf_value(Value, BaseURI, QVal, Encoding),
	rdf_id(Name, BaseURI, NameText),
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	format(Out, ' rdf:resource="~w"/>', [QVal]).

%%	save_body_literal(+Literal, +NameText, +BaseURI,
%%			  +Out, +Indent, +Options).

save_body_literal(lang(Lang, Value),
		  NameText, BaseURI, Out, Indent, Options) :- !,
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	(   memberchk(document_language(Lang), Options)
	->  write(Out, '>')
	;   rdf_id(Lang, BaseURI, LangText),
	    format(Out, ' xml:lang="~w">', [LangText])
	),
	save_attribute_value(Value, Out, Indent),
	write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_body_literal(type(Type, DOM),
		  NameText, _BaseURI, Out, Indent, Options) :-
	rdf_equal(Type, rdf:'XMLLiteral'), !,
	save_xml_literal(DOM, NameText, Out, Indent, Options).
save_body_literal(type(Type, Value),
		  NameText, BaseURI, Out, Indent, _) :- !,
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	stream_property(Out, encoding(Encoding)),
	rdf_value(Type, BaseURI, QVal, Encoding),
	format(Out, ' rdf:datatype="~w">', [QVal]),
	save_attribute_value(Value, Out, Indent),
	write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_body_literal(Literal,
		  NameText, _, Out, Indent, _) :-
	atomic(Literal), !,
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	write(Out, '>'),
	save_attribute_value(Literal, Out, Indent),
	write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_body_literal(DOM,
		  NameText, BaseURI, Out, Indent, Options) :-
	rdf_equal(Type, rdf:'XMLLiteral'),
	save_body_literal(type(Type, DOM),
			  NameText, BaseURI, Out, Indent, Options).

save_attribute_value(Value, Out, _) :-	% strings
	atom(Value), !,
	stream_property(Out, encoding(Encoding)),
	xml_quote_cdata(Value, QVal, Encoding),
	write(Out, QVal).
save_attribute_value(Value, Out, _) :-	% numbers
	number(Value), !,
	writeq(Out, Value).		% quoted: preserve floats
save_attribute_value(Value, _Out, _) :-
	throw(error(save_attribute_value(Value), _)).

%%	save_xml_literal(+DOM, +Attr, +Out, +Indent, +Options) is det.
%
%	Save an XMLLiteral value. We already emitted
%
%		==
%		<prop parseType="literal"
%		==
%
%	but  not  the  terminating  =|>|=.  We  need  to  establish  the
%	namespaces used in the DOM. The   namespaces in the rdf document
%	are in the nsmap-option of Options.

save_xml_literal(DOM, Attr, Out, Indent, Options) :-
	xml_is_dom(DOM), !,
	memberchk(nsmap(NsMap), Options),
	id_to_atom(Attr, Atom),
	xml_write(Out,
		  element(Atom, ['rdf:parseType'='Literal'], DOM),
		  [ header(false),
		    indent(Indent),
		    nsmap(NsMap)
		  ]).
save_xml_literal(NoDOM, _, _, _, _) :-
	must_be(xml_dom, NoDOM).

id_to_atom(NS:Local, Atom) :- !,
	atomic_list_concat([NS,Local], :, Atom).
id_to_atom(ID, ID).


%%	rdf_collection(+URI) is semidet.
%
%	True  if  URI  represents  an  RDF    list  that  fits  the  RDF
%	parseType=collection syntax. This means it is   a linked list of
%	bnode-cells with a rdf:first that is   a  resource, optionally a
%	rdf:type that is an rdf:list and the list ends in an rdf:nil.

:- rdf_meta
	rdf_collection(r),
	collection_p(r,r).

rdf_collection(rdf:nil) :- !.
rdf_collection(Cell) :-
	rdf_is_bnode(Cell),
	findall(F, rdf(Cell, rdf:first, F), [_]),
	findall(F, rdf(Cell, rdf:rest, F), [Rest]),
	forall(rdf(Cell, P, V),
	       collection_p(P, V)),
	rdf_collection(Rest).

collection_p(rdf:first, V) :- atom(V).
collection_p(rdf:rest, _).
collection_p(rdf:type, rdf:'List').


%%	rdf_save_list(+Out, +List, +BaseURI, +Indent, +Options)

rdf_save_list(_, List, _, _, _) :-
	rdf_equal(List, rdf:nil), !.
rdf_save_list(Out, List, BaseURI, Indent, Options) :-
	rdf_has(List, rdf:first, First),
	(   rdf_is_bnode(First)
	->  nl(Out),
	    rdf_save_subject(Out, First, BaseURI, Indent, Options)
	;   stream_property(Out, encoding(Encoding)),
	    rdf_value(First, BaseURI, QVal, Encoding),
	    format(Out, '~N~*|<rdf:Description rdf:about="~w"/>',
		   [Indent, QVal])
	),
	flag(rdf_db_saved_triples, X, X+3),
	(   rdf_has(List, rdf:rest, List2),
	    \+ rdf_equal(List2, rdf:nil)
	->  rdf_save_list(Out, List2, BaseURI, Indent, Options)
	;   true
	).


%%	rdf_id(+Resource, +BaseURI, -NSLocal)
%
%	Generate a NS:Local  name  for   Resource  given  the  indicated
%	default namespace. This call is used for elements.

rdf_id(Id, BaseURI, Local) :-
	assertion(atom(BaseURI)),
	atom_concat(BaseURI, Local, Id),
	sub_atom(Local, 0, 1, _, #), !.
rdf_id(Id, _, NS:Local) :-
	iri_xml_namespace(Id, Full, Local),
	ns(NS, Full), !.
rdf_id(Id, _, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_id(Id, _, Id).


%%	rdf_write_id(+Out, +NSLocal) is det.
%
%	Write an identifier. We cannot use native write on it as both NS
%	and Local can be operators.

rdf_write_id(Out, NS:Local) :- !,
	format(Out, '~w:~w', [NS, Local]).
rdf_write_id(Out, Atom) :-
	write(Out, Atom).

%%	rdf_value(+Resource, +BaseURI, -Text, +Encoding)
%
%	According  to  "6.4  RDF  URI  References"  of  the  RDF  Syntax
%	specification, a URI reference is  UNICODE string not containing
%	control sequences, represented as  UTF-8   and  then  as escaped
%	US-ASCII.

rdf_value(Base, Base, '', _) :- !.
rdf_value(V, Base, Text, Encoding) :-
	atom_concat(Base, Local, V),
	sub_atom(Local, 0, _, _, #), !,
	rdf_quote_uri(Local, Q0),
	xml_quote_attribute(Q0, Text, Encoding).
rdf_value(V, _, Text, Encoding) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	rdf_quote_uri(Local, QLocal0),
	xml_quote_attribute(QLocal0, QLocal, Encoding),
	atomic_list_concat(['&', NS, (';'), QLocal], Text).
rdf_value(V, _, Q, Encoding) :-
	rdf_quote_uri(V, Q0),
	xml_quote_attribute(Q0, Q, Encoding).


		 /*******************************
		 *	DEPRECATED MATERIAL	*
		 *******************************/

%%	rdf_split_url(+Prefix, +Local, -URL) is det.
%%	rdf_split_url(-Prefix, -Local, +URL) is det.
%
%	Split/join a URL.  This functionality is moved to library(sgml).
%
%	@deprecated Use iri_xml_namespace/3. Note that the argument
%	order is iri_xml_namespace(+IRI, -Namespace, -Localname).

rdf_split_url(Prefix, Local, URL) :-
	atomic(URL), !,
	iri_xml_namespace(URL, Prefix, Local).
rdf_split_url(Prefix, Local, URL) :-
	atom_concat(Prefix, Local, URL).

%%	rdf_url_namespace(+URL, -Namespace)
%
%	Namespace is the namespace of URL.
%
%	@deprecated Use iri_xml_namespace/2

rdf_url_namespace(URL, Prefix) :-
	iri_xml_namespace(URL, Prefix).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(loaded(How, What, BaseURI, Triples, Time))) -->
	how(How),
	source(What),
	into(What, BaseURI),
	in_time(Triples, Time).
prolog:message(rdf(save_removed_duplicates(N, Subject))) -->
	[ 'Removed ~d duplicate triples about "~p"'-[N,Subject] ].
prolog:message(rdf(saved(File, SavedSubjects, SavedTriples))) -->
	[ 'Saved ~D triples about ~D subjects into ~p'-
	  [SavedTriples, SavedSubjects, File]
	].
prolog:message(rdf(using_namespace(Id, NS))) -->
	[ 'Using namespace id ~w for ~w'-[Id, NS] ].
prolog:message(rdf(inconsistent_cache(DB, Graphs))) -->
	[ 'RDF cache file for ~w contains the following graphs'-[DB], nl,
	  '~t~8|~p'-[Graphs]
	].
prolog:message(rdf(guess_format(Ext))) -->
	[ 'Unknown file-extension: ~w.  Assuming RDF/XML'-[Ext] ].
prolog:message(rdf_meta(not_expanded(G))) -->
	[ 'rdf_meta: ~p is not expanded'-[G] ].

how(load)   --> [ 'Loaded' ].
how(parsed) --> [ 'Parsed' ].

source(SourceURL) -->
	{ uri_file_name(SourceURL, File), !,
	  file_base_name(File, Base)	% TBD: relative file?
	},
	[ ' "~p"'-[Base] ].
source(SourceURL) -->
	[ ' "~p"'-[SourceURL] ].

into(_, _) --> [].			% TBD

in_time(Triples, ParseTime) -->
	[ ' in ~2f sec; ~D triples'-[ParseTime, Triples]
	].
