/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

	    rdf_save_db/1,		% +File
	    rdf_save_db/2,		% +File, +DB
	    rdf_load_db/1,		% +File
	    rdf_reset_db/0,

	    rdf_node/1,			% -Id
	    rdf_bnode/1,		% -Id
	    rdf_is_bnode/1,		% +Id

	    rdf_load/1,			% +File
	    rdf_load/2,			% +File, +Options
	    rdf_save/1,			% +File
	    rdf_save/2,			% +File, +DB
	    rdf_unload/1,		% +File

	    rdf_md5/2,			% +DB, -MD5
	    rdf_atom_md5/3,		% +Text, +Times, -MD5

	    rdf_source/1,		% ?File
	    rdf_make/0,			% Reload modified databases

	    rdf_source_location/2,	% +Subject, -Source
	    rdf_statistics/1,		% -Key
	    rdf_generation/1,		% -Generation
	    rdf_estimate_complexity/4,	% +S,+P,+O,-Count

	    rdf_save_subject/3,		% +Stream, +Subject, +DB
	    rdf_save_header/2,		% +Out, +DB
	    rdf_save_footer/1,		% +Out

	    rdf_equal/2,		% ?Resource, ?Resource

	    rdf_register_ns/2,		% +Alias, +URI
	    rdf_global_id/2,		% ?NS:Name, ?Global
	    rdf_global_object/2,	% ?Object, ?NSExpandedObject
	    rdf_global_term/2,		% Term, WithExpandedNS

	    rdf_match_label/3,		% +How, +String, +Label
	    rdf_split_url/3,		% ?Base, ?Local, ?URL

	    rdf_debug/1			% Set verbosity
	  ]).
:- use_module(library(rdf)).
:- use_module(library(lists)).
:- use_module(library(shlib)).
:- use_module(library(gensym)).

:- initialization
   load_foreign_library(foreign(rdf_db)).

:- multifile
	ns/2.
:- dynamic
	ns/2,				% ID, URL
	rdf_source/4.			% File, ModTimeAtLoad, Triples, MD5
:- volatile
	rdf_source/4.


		 /*******************************
		 *	     NAMESPACES		*
		 *******************************/

ns(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
ns(owl,  'http://www.w3.org/2002/07/owl#').
ns(xsd,  'http://www.w3.org/2001/XMLSchema#').
ns(dc,   'http://purl.org/dc/elements/1.1/').
ns(eor,  'http://dublincore.org/2000/03/13/eor#').
ns(serql,'http://www.openrdf.org/schema/serql#').

%	rdf_register_ns(+Alias, +URI)
%
%	Register a namespace.  What to do if the Alias already
%	exists?  Throw a permission error?  Use both URI's as synonyms?

rdf_register_ns(Alias, URI) :-
	ns(Alias, URI), !.
rdf_register_ns(Alias, _) :-
	ns(Alias, _),
	throw(error(permission_error(register, namespace, Alias),
		    context(_, 'Already defined'))).
rdf_register_ns(Alias, URI) :-
	assert(ns(Alias, URI)).


%	rdf_global_id(?Id, ?GlobalId)
%
%	Convert between NS:Local and global atomic identifier.
%	To be completed.

rdf_global_id(NS:Local, Global) :-
	global(NS, Local, Global), !.
rdf_global_id(Global, Global).


%	rdf_global_object(?Object, ?GlobalObject)
%	
%	Same as rdf_global_id/2,  but  intended   for  dealing  with the
%	object part of a  triple,  in   particular  the  type  for typed
%	literals.

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
	    *-> atom_concat(Full, Local, Global)
	    ;   atom_concat(NS, Local, Global)
	    )
	).



%	rdf_global_term(+TermIn, -GlobalTerm)
%	
%	Does rdf_global_id/2 on all terms NS:Local by recursively analysing
%	the term.

rdf_global_term(Var, Var) :-
	var(Var), !.
rdf_global_term(NS:Local, Global) :-
	rdf_global_id(NS:Local, Global).
rdf_global_term([H0|T0], [H|T]) :- !,
	rdf_global_term(H0, H),
	rdf_global_term(T0, T).
rdf_global_term(Term0, Term) :-
	compound(Term0), !,
	Term0 =.. [H|L0],
	rdf_global_term(L0, L),
	Term =.. [H|L].
rdf_global_term(Term, Term).


%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdf(Subj0, Pred0, Obj0),
		    rdf(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_has(Subj0, Pred0, Obj0, RP0),
		    rdf_has(Subj, Pred, Obj, RP)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj),
	rdf_global_id(RP0, RP).
user:goal_expansion(rdf_has(Subj0, Pred0, Obj0),
		    rdf_has(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_assert(Subj0, Pred0, Obj0),
		    rdf_assert(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_retractall(Subj0, Pred0, Obj0),
		    rdf_retractall(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf(Subj0, Pred0, Obj0, PayLoad),
		    rdf(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_reachable(Subj0, Pred0, Obj0),
		    rdf_reachable(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_assert(Subj0, Pred0, Obj0, PayLoad),
		    rdf_assert(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_retractall(Subj0, Pred0, Obj0, PayLoad),
		    rdf_retractall(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).
user:goal_expansion(rdf_update(Subj0, Pred0, Obj0, Action0),
		    rdf_update(Subj, Pred, Obj, Action)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj),
	rdf_global_term(Action0, Action).
user:goal_expansion(rdf_equal(SubjA0, SubjB0),
		    rdf_equal(SubjA, SubjB)) :-
	rdf_global_id(SubjA0, SubjA),
	rdf_global_id(SubjB0, SubjB).
user:goal_expansion(rdf_source_location(Subj0, Source),
		    rdf_source_location(Subj, Source)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(rdf_subject(Subj0),
		    rdf_subject(Subj)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(rdf_set_predicate(P0, Prop),
		    rdf_set_predicate(P, Prop)) :-
	rdf_global_id(P0, P).
user:goal_expansion(rdf_predicate_property(P0, Prop),
		    rdf_predicate_property(P, Prop)) :-
	rdf_global_id(P0, P).
user:goal_expansion(rdf_estimate_complexity(Subj0, Pred0, Obj0, C),
		    rdf_estimate_complexity(Subj, Pred, Obj, C)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_object(Obj0, Obj).


%	rdf_equal(?Resource1, ?Resource2)
%	
%	Simple equality test to exploit goal-expansion

rdf_equal(Resource, Resource).


%	rdf_has(?Subject, +Predicate, ?Object)
%	
%	Succeeds if the triple rdf(Subject, Predicate, Object) is true
%	exploiting the rdfs:subPropertyOf predicate.

rdf_has(Subject, Predicate, Object) :-
	rdf_has(Subject, Predicate, Object, _).


		 /*******************************
		 *	    COLLECTIONS		*
		 *******************************/

%	rdf_member_property(?Prop, ?Index)
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

%	rdf_node(-Id)
%
%	Generate a unique identifier for a subject.  Obsolete.  New
%	code should use rdf_bnode/1.

rdf_node(Value) :-
	repeat,
	gensym('_:', Value),
	\+ rdf_subject(Value),
	\+ rdf(_, _, Value),
	\+ rdf(_, Value, _), !.


%	rdf_bnode(-Id)
%
%	Generate a unique anonymous identifier for a subject.

rdf_bnode(Value) :-
	repeat,
	gensym('__bnode', Value),
	\+ rdf_subject(Value),
	\+ rdf(_, _, Value),
	\+ rdf(_, Value, _), !.


%	rdf_is_bnode(+Id)
%	
%	Tests if a resource is a blank node (i.e. is an anonymous
%	resource).

rdf_is_bnode(Id) :-
	atom(Id),
	sub_atom(Id, 0, _, _, '__').


		 /*******************************
		 *	      SOURCE		*
		 *******************************/

%	rdf_source_location(+Subject, -File:Line)
%	
%	Return the source-locations for triples for this subject.

rdf_source_location(Subject, Source) :-
	findall(Source, rdf(Subject, _, _, Source), Sources),
	sort(Sources, Unique),
	member(Source, Unique).


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

%	rdf_statistics(+Key(-Value))
%	
%	Obtain some statistics

rdf_statistics(sources(Count)) :-
	predicate_property(rdf_source(_,_,_,_), number_of_clauses(Count)).
rdf_statistics(subjects(Count)) :-
	rdf_statistics_(subjects(Count)).
rdf_statistics(properties(Count)) :-
	rdf_statistics_(predicates(Count)).
rdf_statistics(triples(Count)) :-
	rdf_statistics_(triples(Count)).
%rdf_statistics(triples_by_property(Rel0, Count)) :-
%	rdf_global_id(Rel0, Rel),
%	triples_on_relation(Rel, Count).
rdf_statistics(lookup(Index, Count)) :-
	functor(Indexed, indexed, 8),
	rdf_statistics_(Indexed),
	index(Index, I),
	Arg is I + 1,
	arg(Arg, Indexed, Count),
	Count \== 0.
rdf_statistics(searched_nodes(Count)) :-
	rdf_statistics_(searched_nodes(Count)).
rdf_statistics(triples_by_file(File, Count)) :-
	(   var(File)
	->  rdf_source(File),
	    rdf_statistics_(triples(File, Count))
	;   rdf_statistics_(triples(File, Count))
	).
rdf_statistics(duplicates(Count)) :-
	rdf_statistics_(duplicates(Count)).

index(rdf(-,-,-), 0).
index(rdf(+,-,-), 1).
index(rdf(-,+,-), 2).
index(rdf(+,+,-), 3).
index(rdf(-,-,+), 4).
index(rdf(+,-,+), 5).
index(rdf(-,+,+), 6).
index(rdf(+,+,+), 7).

triples_on_relation(Rel, Count) :-
	count_solutions(rdf(_,Rel,_), Count).

%	count_solutions(+Goal, -Count)
%	
%	Count the number of times Goal succeeds.

count_solutions(Goal, Count) :-
	flag(rdf_db_count_solutions, Old, 0),
	(   catch(Goal, E, (flag(rdf_db_count_solutions, _, Old),
			    throw(E))),
	    flag(rdf_db_count_solutions, C, C+1),
	    fail
	;   flag(rdf_db_count_solutions, C, Old)
	),
	Count = C.


		 /*******************************
		 *    QUICK BINARY LOAD/SAVE	*
		 *******************************/

%	rdf_save_db(+File, [+DB])
%	
%	Save triples into File in a   quick-to-load binary format. If DB
%	is supplied only triples flagged to originate from that database
%	are added.

rdf_save_db(File) :-
	open(File, write, Out, [type(binary)]),
	call_cleanup(rdf_save_db_(Out, _), close(Out)).


rdf_save_db(File, DB) :-
	open(File, write, Out, [type(binary)]),
	call_cleanup(rdf_save_db_(Out, DB), close(Out)).


rdf_load_db_no_admin(File) :-
	open(File, read, Out, [type(binary)]),
	call_cleanup(rdf_load_db_(Out), close(Out)).


rdf_load_db(File) :-
	rdf_load_db_no_admin(File),
	rdf_sources_(Sources),
	(   member(Src, Sources),
	    rdf_md5(Src, MD5),
	    rdf_statistics_(triples(Src, Triples)),
	    retractall(rdf_source(Src, _, _, _)),
	    assert(rdf_source(Src, 0, Triples, MD5)),
	    fail
	;   true
	).


		 /*******************************
		 *	    LOADING RDF		*
		 *******************************/

%	rdf_load(+File, +Options)
%
%	Load RDF file.  Options provides additional processing options.
%	Currently defined options are:
%	
%	    result(-Action, -Triples, -MD5)
%	    	Return action taken (load, reload, none) and number
%	    	of triples loaded from the file as well as the MD5
%	    	digest.
%	    	
%	Other options are forwarded to process_rdf/3.

rdf_load(Spec) :-
	rdf_load(Spec, []).

rdf_load(Spec, Options0) :-
	statistics(cputime, CpuOld),
	(   select(result(Action, Triples, MD5), Options0, Options1)
	->  true
	;   Options1 = Options0
	),
	(   memberchk(blank_nodes(_), Options1)
	->  Options2 = Options1
	;   Options2 = [ blank_nodes(share)|Options1 ]
	),
	(   (   Spec = '$stream'(_)
	    ->	In = Spec
	    ;	Spec = stream(In)
	    )
	->  (   memberchk(base_uri(BaseURI), Options2)
	    ->	Options = Options2
	    ;	gensym('stream://', BaseURI),
		Options = [base_uri(BaseURI)|Options2]
	    ),
	    process_rdf(In, assert_triples, Options),
	    rdf_statistics_(triples(BaseURI, Triples)),
	    rdf_md5(BaseURI, MD5),
	    assert(rdf_source(BaseURI, Modified, Triples, MD5)),
	    Source = BaseURI,
	    Load = parsed(ParseTime),
	    Action = load
	;   Source = Spec,
	    Options = Options2,
	    absolute_file_name(Spec,
			       [ access(read),
				 extensions([rdf,rdfs,owl,''])
			       ], File),
	    time_file(File, Modified),
	    (	rdf_source(File, WhenLoaded, _, _)
	    ->	(   Modified > WhenLoaded
		->  rdf_retractall(_,_,_,File:_),
		    Action = reload
		;   Action = none
		)
	    ;	Action = load
	    ),
	    (	Action \== none
	    ->  atom_concat('file:', File, DefBaseURI),
		option(base_uri(BaseURI), Options1, DefBaseURI),
		retractall(rdf_source(File, _, _, _)),
		(   cache_file(File, Cache)
		->  (   time_file(Cache, CacheTime),
		        time_file(File, FileTime),
			CacheTime >= FileTime,
			catch(rdf_load_db_no_admin(Cache), _, fail)
		    ->  Load = cache(ParseTime)
		    ;   process_rdf(File, assert_triples, Options),
			Load = parsed(ParseTime),
			save_cache(File, Cache)
		    )
		;   process_rdf(File, assert_triples, Options),
		    Load = parsed(ParseTime)
		),
		rdf_statistics_(triples(File, Triples)),
		rdf_md5(File, MD5),
		assert(rdf_source(File, Modified, Triples, MD5))
	    ;	rdf_source(File, _Modified, Triples, MD5)
	    )
	),
	(   Action \== none
	->  statistics(cputime, CpuLoaded),
	    ParseTime is CpuLoaded - CpuOld,
	    print_message(informational,
			  rdf(loaded(Source, Triples, Load)))
	;   true
	).


%	rdf_unload(+Spec)
%	
%	Remove the triples loaded from the specified source and remove
%	the source from the database.

rdf_unload(Spec) :-
	(   Spec = '$stream'(_)
	->  throw(error(permission_error(rdf_db, unload, Spec), _))
	;   absolute_file_name(Spec,
			       [ access(read),
				 extensions([rdf,rdfs,owl,''])
			       ], File),
	    rdf_retractall(_,_,_,File:_),
	    retractall(rdf_source(File, _, _, _))
	).
	

%	rdf_source(?Source)
%	
%	Query the loaded sources

rdf_source(File) :-
	rdf_source(File, _, _, _).

%	rdf_make
%	
%	Reload all loaded files that have been modified since the last
%	time they were loaded.

rdf_make :-
	forall((rdf_source(File, Time, _Triples, _),
		Time \== 0),
	       rdf_load(File)).


%	cache_file(+Base, -CacheFile)
%	
%	Deduce the name of the file used to cache the triples.

cache_dir(CacheDir) :-
	(   current_prolog_flag(windows, true)
	->  CacheDir = '_cache'
	;   CacheDir = '.cache'
	).

cache_file(Base, Cache) :-
	file_directory_name(Base, BaseDir),
	file_base_name(Base, File),
	cache_dir(Dir),
	concat_atom([BaseDir, /, Dir], CacheDir),
	exists_directory(CacheDir),
	concat_atom([CacheDir, /, File, '.trp'], Cache).

%	save_cache(+File, +Cache)

save_cache(File, Cache) :-
	catch(open(Cache, write, CacheStream, [type(binary)]), _, fail), !,
	rdf_save_db_(CacheStream, File),
	close(CacheStream).

%	assert_triples(+Triples, +Source)
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

%	rdf_reset_db
%	
%	Remove all triples from the RDF database and reset all its
%	statistics.

rdf_reset_db :-
	rdf_reset_db_,
	retractall(rdf_source(_,_,_,_)).


		 /*******************************
		 *	     SAVE RDF		*
		 *******************************/

%	rdf_save(File, [+Options])
%
%	Save RDF data to file.  Options is a list of one or more of the
%	following options:
%	
%		# db(+DB)
%		Save only triples associated to the given DB
%		
%		# anon(Bool)
%		If false (default true) do not save blank nodes that do
%		not appear (indirectly) as object of a named resource.
%
%		# convert_typed_literal(:Convertor)
%		Call Convertor(-Type, -Content, +RDFObject), providing
%		the opposite for the convert_typed_literal option of
%		the RDF parser.

:- module_transparent
	rdf_save/2,
	meta_options/2.

rdf_save(File) :-
	rdf_save2(File, []).

rdf_save(File, Options0) :-
	is_list(Options0), !,
	meta_options(Options0, Options),
	rdf_save2(File, Options).
rdf_save(File, DB) :-
	atom(DB), !,			% backward compatibility
	rdf_save2(File, [db(DB)]).


rdf_save2(File, Options) :-
	open(File, write, Out),
	flag(rdf_db_saved_subjects, OSavedSubjects, 0),
	flag(rdf_db_saved_triples, OSavedTriples, 0),
	call_cleanup(rdf_do_save(Out, Options),
		     Reason,
		     cleanup_save(Reason,
				  File,
				  OSavedSubjects,
				  OSavedTriples,
				  Out)).


cleanup_save(Reason,
	     File,
	     OSavedSubjects,
	     OSavedTriples,
	     Out) :-
	close(Out),
	flag(rdf_db_saved_subjects, SavedSubjects, OSavedSubjects),
	flag(rdf_db_saved_triples, SavedTriples, OSavedTriples),
	(   Reason == exit
	->  print_message(informational,
			  rdf(saved(File, SavedSubjects, SavedTriples)))
	;   format(user_error, 'Reason = ~w~n', [Reason])
	).

rdf_do_save(Out, Options) :-
	rdf_save_header(Out, Options),
	forall(rdf_subject(Subject, Options),
	       rdf_save_non_anon_subject(Out, Subject, Options)),
	rdf_save_footer(Out), !.	% dubious cut; without the
					% cleanup handlers isn't called!?

rdf_subject(Subject, Options) :-
	db(Options, DB),
	var(DB), !,
	rdf_subject(Subject).
rdf_subject(Subject, Options) :-
	db(Options, DB),
	rdf_subject(Subject),
	(   rdf(Subject, _, _, DB:_)
	->  true
	).

db(Options, DB) :-
	(   memberchk(db(DB0), Options)
	->  DB = DB0
	;   true			% leave unbound
	).


%	meta_options(+OptionsIn, -OptionsOut)
%	
%	Do module qualification for options that are module sensitive.

meta_options([], []).
meta_options([Name=Value|T0], List) :-
	atom(Name), !,
	Opt =.. [Name, Value],
	meta_options([Opt|T0], List).
meta_options([H0|T0], [H|T]) :-
	(   H0 = convert_typed_literal(Handler)
	->  strip_module(Handler, M, P),
	    H = convert_typed_literal(M:P)
	;   H = H0
	),
	meta_options(T0, T).


%	rdf_save_header(+Fd, +Options)
%
%	Save XML document header, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_save_header(Out, Options) :-
	is_list(Options), !,
	db(Options, DB),
	format(Out, '<?xml version=\'1.0\' encoding=\'ISO-8859-1\'?>~n', []),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	used_namespaces(NSList, DB),
	(   member(Id, NSList),
	    ns(Id, NS),
	    format(Out, '~N    <!ENTITY ~w \'~w\'>', [Id, NS]),
	    fail
	;   true
	),
	format(Out, '~N]>~n~n', []),
	format(Out, '<rdf:RDF', []),
	(   member(Id, NSList),
	    format(Out, '~N    xmlns:~w="&~w;"~n', [Id, Id]),
	    fail
	;   true
	),
	format(Out, '>~n', []).
rdf_save_header(Out, FileRef) :-	% compatibility
	atom(FileRef),
	rdf_save_header(Out, [db(FileRef)]).
	

%	used_namespaces(-List)
%
%	Return the list of namespaces used in an RDF database.

used_namespaces(List, DB) :-
	decl_used_predicate_ns(DB),
	setof(NS, Full^ns(NS, Full), NS0),
	used_ns(NS0, List, DB).

used_ns([], [], _).
used_ns([H|T0], [H|T], DB) :-
	used_ns(H, DB), !,
	used_ns(T0, T, DB).
used_ns([_|T0], T, DB) :-
	used_ns(T0, T, DB).

used_ns(rdf, _) :- !.			% we need rdf:RDF
used_ns(NS, DB) :-
	ns(NS, Full),
	rdf_db(S,P,O,DB),
	(   sub_atom(S, 0, _, _, Full)
	;   sub_atom(P, 0, _, _, Full)
	;   atom(O),
	    sub_atom(O, 0, _, _, Full)
	), !.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For every URL used as a predicate  we   *MUST*  define a namespace as we
cannot use names holding /, :, etc. as XML identifiers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
	predicate_ns/2.

decl_used_predicate_ns(DB) :-
	retractall(predicate_ns(_,_)),
	(   rdf_db(_,P,_,DB),
	    decl_predicate_ns(P),
	    fail
	;   true
	).

decl_predicate_ns(Pred) :-
	predicate_ns(Pred, _), !.
decl_predicate_ns(Pred) :-
	rdf_global_id(NS:_Local, Pred),
	assert(predicate_ns(Pred, NS)), !.
decl_predicate_ns(Pred) :-
	atom_codes(Pred, Codes),
	append(NSCodes, LocalCodes, Codes),
	xml_codes(LocalCodes), !,
	(   NSCodes \== []
	->  atom_codes(NS, NSCodes),
	    (   between(1, 1000000, N),
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
xml_code(0'-).


rdf_save_footer(Out) :-
	format(Out, '</rdf:RDF>~n', []).

%	rdf_save_non_anon_subject(+Out, +Subject, +Options)
%	
%	Save an object.  Anonymous objects not saved if anon(false)
%	is present in the Options list.

rdf_save_non_anon_subject(_Out, Subject, Options) :-
	anonymous_subject(Subject),
	(   memberchk(anon(false), Options)
	;   db(Options, DB),
	    rdf_db(_, _, Subject, DB)
	), !.
rdf_save_non_anon_subject(Out, Subject, Options) :-
	db(Options, DB),
	rdf_save_subject(Out, Subject, DB),
	flag(rdf_db_saved_subjects, X, X+1).


rdf_save_subject(Out, Subject, Options) :-
	is_list(Options), !,
	(   rdf_save_subject(Out, Subject, -, 0, Options)
	->  format(Out, '~n', [])
	;   throw(error(rdf_save_failed(Subject), 'Internal error'))
	).
rdf_save_subject(Out, Subject, DB) :-
	(   var(DB)
	->  rdf_save_subject(Out, Subject, [])
	;   rdf_save_subject(Out, Subject, [db(DB)])
	).
		  

rdf_save_subject(Out, Subject, DefNS, Indent, Options) :-
	db(Options, DB),
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
	rdf_save_subject(Out, Subject, DefNS, Atts, Indent, Options),
	flag(rdf_db_saved_triples, X, X+L).

rdf_db(Subject, Pred, Object, DB) :-
	var(DB), !,
	rdf(Subject, Pred, Object).
rdf_db(Subject, Pred, Object, DB) :-
	rdf(Subject, Pred, Object, DB:_).

rdf_save_subject(Out, Subject, DefNS, Atts, Indent, Options) :-
	rdf_equal(rdf:type, RdfType),
	select(RdfType=Type, Atts, Atts1),
	rdf_id(Type, DefNS, TypeId),
	xml_is_name(TypeId), !,
	format(Out, '~*|<~w', [Indent, TypeId]),
	save_about(Out, Subject),
	save_attributes(Atts1, DefNS, Out, TypeId, Indent, Options).
rdf_save_subject(Out, Subject, _DefNS, Atts, Indent, Options) :-
	format(Out, '~*|<rdf:Description', [Indent]),
	save_about(Out, Subject),
	save_attributes(Atts, rdf, Out, rdf:'Description', Indent, Options).

xml_is_name(_NS:Atom) :- !,
	xml_name(Atom).
xml_is_name(Atom) :-
	xml_name(Atom).

save_about(_Out, Subject) :-
	anonymous_subject(Subject), !.
save_about(Out, Subject) :-
	rdf_value(Subject, QSubject),
	format(Out, ' rdf:about="~w"', [QSubject]).

%	save_attributes(+List, +DefNS, +Stream, Element)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Atts, DefNS, Out, Element, Indent, Options) :-
	split_attributes(Atts, InTag, InBody),
	SubIndent is Indent + 2,
	save_attributes2(InTag, DefNS, tag, Out, SubIndent, Options),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, _, body, Out, SubIndent, Options),
	    format(Out, '~N~*|</~w>~n', [Indent, Element])
	).

%	split_attributes(+Attributes, -HeadAttrs, -BodyAttr)
%	
%	Split attribute (Name=Value) list into attributes for the head
%	and body. Attributes can only be in the head if they are literal
%	and appear only one time in the attribute list.

split_attributes(Atts, HeadAttr, BodyAttr) :-
	duplicate_attributes(Atts, Dupls, Singles),
	simple_literal_attributes(Singles, HeadAttr, Rest),
	append(Dupls, Rest, BodyAttr).

%	duplicate_attributes(+Attrs, -Duplicates, -Singles)
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

%	simple_literal_attributes(+Attributes, -Inline, -Body)
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

%	save_attributes(+List, +DefNS, +TagOrBody, +Stream)
%
%	Save a list of attributes.

save_attributes2([], _, _, _, _, _).
save_attributes2([H|T], DefNS, Where, Out, Indent, Options) :-
	save_attribute(Where, H, DefNS, Out, Indent, Options),
	save_attributes2(T, DefNS, Where, Out, Indent, Options).

save_attribute(tag, Name=literal(Value), DefNS, Out, Indent, _DB) :-
	AttIndent is Indent + 2,
	rdf_att_id(Name, DefNS, NameText),
	xml_quote_attribute(Value, QVal),
	format(Out, '~N~*|~w="~w"', [AttIndent, NameText, QVal]).
save_attribute(body, Name=literal(Literal0), DefNS, Out, Indent, Options) :- !,
	rdf_id(Name, DefNS, NameText),
	(   memberchk(convert_typed_literal(Converter), Options),
	    call(Converter, Type, Content, Literal0)
	->  Literal = type(Type, Content)
	;   Literal = Literal0
	),
	(   Literal = lang(Lang, Value)
	->  rdf_id(Lang, DefNS, LangText),
	    format(Out, '~N~*|<~w xml:lang="~w">',
		   [Indent, NameText, LangText])
	;   Literal = type(Type, Value)
	->  rdf_id(Type, DefNS, TypeText),
	    format(Out, '~N~*|<~w rdf:dataType="~w">',
		   [Indent, NameText, TypeText])
	;   format(Out, '~N~*|<~w>', [Indent, NameText]),
	    Value = Literal
	),
	save_attribute_value(Value, Out),
	format(Out, '</~w>', [NameText]).
save_attribute(body, Name=Value, DefNS, Out, Indent, Options) :-
	anonymous_subject(Value), !,
	rdf_id(Name, DefNS, NameText),
	SubIndent is Indent + 2,
	(   rdf(Value, rdf:type, rdf:'List')
	->  format(Out, '~N~*|<~w rdf:parseType="Collection">~n',
		   [Indent, NameText]),
	    rdf_save_list(Out, Value, DefNS, SubIndent, Options)
	;   format(Out, '~N~*|<~w>~n',
		   [Indent, NameText]),
	    rdf_save_subject(Out, Value, DefNS, SubIndent, Options)
	),
	format(Out, '~N~*|</~w>~n', [Indent, NameText]).
save_attribute(body, Name=Value, DefNS, Out, Indent, _DB) :-
	rdf_value(Value, QVal),
	rdf_id(Name, DefNS, NameText),
	format(Out, '~N~*|<~w rdf:resource="~w"/>', [Indent, NameText, QVal]).

save_attribute_value(Value, Out) :-	% strings
	atom(Value), !,
	xml_quote_cdata(Value, QVal),
	write(Out, QVal).
save_attribute_value(Value, Out) :-	% numbers
	number(Value), !,
	writeq(Out, Value).		% quoted: preserve floats
save_attribute_value(Value, _Out) :-
	throw(error(save_attribute_value(Value), _)).

rdf_save_list(_, List, _, _, _) :-
	rdf_equal(List, rdf:nil), !.
rdf_save_list(Out, List, DefNS, Indent, Options) :-
	rdf_has(List, rdf:first, First),
	(   anonymous_subject(First)
	->  nl(Out),
	    rdf_save_subject(Out, First, DefNS, Indent, Options)
	;   rdf_value(First, QVal),
	    format(Out, '~N~*|<rdf:Description about="~w"/>',
		   [Indent, QVal])
	),
	flag(rdf_db_saved_triples, X, X+3),
	(   rdf_has(List, rdf:rest, List2),
	    \+ rdf_equal(List2, rdf:nil)
	->  rdf_save_list(Out, List2, DefNS, Indent, Options)
	;   true
	).

%	anonymous_subject(+Subject)
%	
%	Test if a resource is anonymous. This is highly dubious.
%	Probably we need to store this in the database.  The current
%	release of the RDF parser guarantees that all anonymous ids
%	start with __.

anonymous_subject(S) :-
	sub_atom(S, 0, _, _, '__'), !.

%	rdf_id(+Resource, +DefNS, -NSLocal)
%	
%	Generate a NS:Local name for Resource given the indicated
%	default namespace.  This call is used for elements.

rdf_id(Id, NS, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_id(Id, _, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_id(Id, _, Id).


rdf_att_id(Id, _, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_att_id(Id, _, Id).


rdf_value(V, Text) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	xml_quote_attribute(Local, QLocal),
	concat_atom(['&', NS, (';'), QLocal], Text).
rdf_value(V, V).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(loaded(Spec, Triples, parsed(ParseTime)))) -->
	[ 'Parsed' ],
	source(Spec),
	in_time(Triples, ParseTime).
prolog:message(rdf(loaded(Spec, Triples, cache(ParseTime)))) -->
	[ 'Loaded' ],
	source(Spec),
	in_time(Triples, ParseTime).
prolog:message(rdf(loaded(Spec, Triples, snapshot(ParseTime)))) -->
	[ 'Loaded snapshot' ],
	source(Spec),
	in_time(Triples, ParseTime).
prolog:message(rdf(save_removed_duplicates(N, Subject))) -->
	[ 'Removed ~d duplicate triples about "~p"'-[N,Subject] ].
prolog:message(rdf(saved(File, SavedSubjects, SavedTriples))) -->
	[ 'Saved ~D triples about ~D subjects into ~p'-
	  [SavedTriples, SavedSubjects, File]
	].
prolog:message(rdf(using_namespace(Id, NS))) -->
	[ 'Using namespace id ~w for ~w'-[Id, NS] ].

source(Spec) -->
	{ atom(Spec),
	  (   sub_atom(Spec, 0, _, _, 'stream://')
	  ->  Base = Spec
	  ;   file_base_name(Spec, Base)
	  )
	},
	[ ' "~w"'-[Base] ].
source(Spec) -->
	[ ' "~p"'-[Spec] ].

in_time(Triples, ParseTime) -->
	[ ' in ~2f sec; ~D triples'-[ParseTime, Triples]
	].
