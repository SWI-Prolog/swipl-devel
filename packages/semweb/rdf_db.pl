/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdf_db,
	  [ rdf/3,			% ?Subject, ?Predicate, ?Object
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

	    rdf_save_db/1,		% +File
	    rdf_save_db/2,		% +File, +DB
	    rdf_load_db/1,		% +File

	    rdf_node/1,			% -Id

	    rdf_load/1,			% +File
	    rdf_save/1,			% +File
	    rdf_save/2,			% +File, +DB

	    rdf_source/1,		% ?File
	    rdf_make/0,			% Reload modified databases

	    rdf_source_location/2,	% +Subject, -Source
	    rdf_statistics/1,		% -Key

	    rdf_save_subject/3,		% +Stream, +Subject, +DB
	    rdf_save_header/2,		% +Out, +DB
	    rdf_save_footer/1,		% +Out

	    rdf_equal/2,		% ?Resource, ?Resource

	    rdf_register_ns/2,		% +Alias, +URI
	    rdf_global_id/2,		% ?NS:Name, ?Global
	    rdf_global_term/2,		% Term, WithExpandedNS

	    rdf_match_label/3,		% +How, +String, +Label
	    rdf_split_url/3,		% ?Base, ?Local, ?URL

	    rdf_debug/1			% Set verbosity
	  ]).
:- use_module(library(rdf)).

:- initialization
   load_foreign_library(foreign(rdf_db)).

:- multifile
	ns/2.
:- dynamic
	ns/2,				% ID, URL
	rdf_source/2.			% File, ModTimeAtLoad


		 /*******************************
		 *	     NAMESPACES		*
		 *******************************/

ns(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
%ns(rdfs, 'http://www.w3.org/TR/1999/PR-rdf-schema-19990303#').
ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
ns(owl,  'http://www.w3.org/2002/07/owl#').
ns(xsd,  'http://www.w3.org/2000/10/XMLSchema#').
ns(dc,   'http://purl.org/dc/elements/1.1/').
ns(eor,  'http://dublincore.org/2000/03/13/eor#').
ns(tool, 'http://www.swi.psy.uva.nl/mia/tool#').

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

rdf_global_id(Global, Global) :-
	var(Global), !.
rdf_global_id(NS:Local, Global) :- !,
	(   ns(NS, Full)
	*-> atom_concat(Full, Local, Global)
	;   atom_concat(NS, Local, Global)
	).
rdf_global_id(Global, Global).


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
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_has(Subj0, Pred0, Obj0, RP0),
		    rdf_has(Subj, Pred, Obj, RP)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj),
	rdf_global_id(RP0, RP).
user:goal_expansion(rdf_has(Subj0, Pred0, Obj0),
		    rdf_has(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_assert(Subj0, Pred0, Obj0),
		    rdf_assert(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_retract(Subj0, Pred0, Obj0),
		    rdf_retract(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf(Subj0, Pred0, Obj0, PayLoad),
		    rdf(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_reachable(Subj0, Pred0, Obj0),
		    rdf_reachable(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_assert(Subj0, Pred0, Obj0, PayLoad),
		    rdf_assert(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_retract(Subj0, Pred0, Obj0, PayLoad),
		    rdf_retract(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
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
%	Generate a unique identifier for a subject.

rdf_node(Value) :-
	repeat,
	gensym('_:', Value),
	\+ rdf_subject(Value),
	\+ rdf(_, _, Value),
	\+ rdf(_, Value, _).


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
	predicate_property(rdf_source(_,_), number_of_clauses(Count)).
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
	rdf_source(File),
	count_solutions(rdf(_,_,_,File:_), Count).
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
	flag(count_solutions, Old, 0),
	(   catch(Goal, E, (flag(count_solutions, _, Old),
			    throw(E))),
	    flag(count_solutions, C, C+1),
	    fail
	;   flag(count_solutions, C, Old)
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


rdf_load_db(File) :-
	open(File, read, Out, [type(binary)]),
	call_cleanup(rdf_load_db_(Out), close(Out)).



		 /*******************************
		 *	    LOADING RDF		*
		 *******************************/

%	rdf_load(+File)
%
%	Load RDF file, associating each fact with File:Line

rdf_load(Spec) :-
	statistics(cputime, CpuOld),
	rdf_statistics(triples(N0)),
	(   Spec = '$stream'(_)
	->  process_rdf(Spec, [], assert_triples),
	    Load = parsed(ParseTime),
	    Action = load
	;   absolute_file_name(Spec,
			       [ access(read),
				 extensions([rdf,rdfs,owl,''])
			       ], File),
	    time_file(File, Modified),
	    (	rdf_source(File, WhenLoaded)
	    ->	(   Modified > WhenLoaded
		->  rdf_retractall(_,_,_,File:_),
		    Action = reload
		;   Action = none
		)
	    ;	Action = load
	    ),
	    (	Action \== none
	    ->	atom_concat('file:', File, BaseURI),
		retractall(rdf_source(File, _)),
		assert(rdf_source(File, Modified)),
		(   cache_file(File, Cache)
		->  (   time_file(Cache, CacheTime),
		        time_file(File, FileTime),
			CacheTime >= FileTime,
			catch(rdf_load_db(Cache), _, fail)
		    ->  Load = cache(ParseTime)
		    ;   process_rdf(File, BaseURI, assert_triples),
			Load = parsed(ParseTime),
			save_cache(File, Cache)
		    )
		;   process_rdf(File, BaseURI, assert_triples),
		    Load = parsed(ParseTime)
		)
	    ;	true
	    )
	),
	(   Action \== none
	->  rdf_statistics(triples(N1)),
	    statistics(cputime, CpuLoaded),
	    ParseTime is CpuLoaded - CpuOld,
	    N is N1 - N0,
	    print_message(informational,
			  rdf(loaded(Spec, N, Load)))
	;   true
	).


%	rdf_source(?Source)
%	
%	Query the loaded sources

rdf_source(File) :-
	rdf_source(File, _).

%	rdf_make
%	
%	Reload all loaded files that have been modified since the last
%	time they were loaded.

rdf_make :-
	forall(rdf_source(File, _Time),
	       rdf_load(File)).


%	cache_file(+Base, -CacheFile)
%	
%	Deduce the name of the file used to cache the triples.

cache_dir('_cache') :-
	current_prolog_flag(windows, true).
cache_dir('.cache').
	

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
	rdf_global_id(S, Subject),
	rdf_global_id(P, Predicate),
	rdf_global_id(O, Object),
	rdf_assert(Subject, Predicate, Object, DB),
	assert_triples(T, DB).
assert_triples([H|_], _) :-
	throw(error(type_error(rdf_triple, H), _)).

%	load_triples_from_stream(+PayLoad, +Stream)
%	
%	Read triples from a file and assert them into the database.

load_triples_from_stream(In) :-
	read(In, T0),
	load_triples_from_stream(T0, In).

load_triples_from_stream(end_of_file, _) :- !.
load_triples_from_stream(rdf(S,P,O,DB), In) :- !,
	rdf_assert(S, P, O, DB),
	read(In, T),
	load_triples_from_stream(T, In).
load_triples_from_stream(T, _) :-
	throw(error(type_error(rdf_triple, T), _)).


		 /*******************************
		 *	     SAVE RDF		*
		 *******************************/

%	rdf_save(File)
%
%	Save RDF data to file

rdf_save(File) :-
	rdf_save(File, _).

rdf_save(File, DB) :-
	open(File, write, Out),
	rdf_save_header(Out, DB),
	forall(rdf_subject(Subject, DB),
	       rdf_save_non_anon_subject(Out, Subject, DB)),
	rdf_save_footer(Out),
	close(Out).

%	rdf_save_header(+Fd, +DB)
%
%	Save XML documentheader, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_save_header(Out, DB) :-
	format(Out, '<?xml version=\'1.0\' encoding=\'ISO-8859-1\'?>~n', []),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	used_namespaces(NSList, DB),
	(   member(Id, NSList),
	    ens(Id, NS),
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

ens(e, '').
ens(Id, NS) :-
	ns(Id, NS).

%	used_namespaces(-List)
%
%	Return the list of namespaces used in an RDF database.

:- dynamic
	used_ns/1.

used_namespaces(List, DB) :-
	setof(NS, Full^ens(NS, Full), NS0),
	used_ns(NS0, List, DB).

used_ns([], [], _).
used_ns([H|T0], [H|T], DB) :-
	used_ns(H, DB), !,
	used_ns(T0, T, DB).
used_ns([_|T0], T, DB) :-
	used_ns(T0, T, DB).

used_ns(e, _) :- !.			% for now just assume it
used_ns(NS, DB) :-
	ns(NS, Full),
	rdf(S,P,O,DB),
	(   sub_atom(S, 0, _, _, Full)
	;   sub_atom(P, 0, _, _, Full)
	;   atom(O),
	    sub_atom(O, 0, _, _, Full)
	), !.


rdf_save_footer(Out) :-
	format(Out, '</rdf:RDF>~n', []).

rdf_save_non_anon_subject(_Out, Subject, _DB) :-
	anonymous_subject(Subject), !.
rdf_save_non_anon_subject(Out, Subject, DB) :-
	rdf_save_subject(Out, Subject, DB).


rdf_save_subject(Out, Subject, DB) :-
	rdf_save_subject(Out, Subject, rdf, 0, DB),
	format(Out, '~n', []).

rdf_save_subject(Out, Subject, DefNS, Indent, DB) :-
	setof(Pred=Object, rdf_db(Subject, Pred, Object, DB), Atts),
	rdf_save_subject(Out, Subject, DefNS, Atts, Indent, DB).

rdf_save_subject(Out, Subject, DefNS0, Atts, Indent, DB) :-
	rdf_global_id(rdf:type, RdfType),
	select(RdfType=Type, Atts, Atts1), !,
	rdf_local_id(Type, DefNS0, DefNS, TypeId),
	format(Out, '~*|<~w', [Indent, TypeId]),
	save_about(Out, Subject, Indent),
	save_attributes(Atts1, DefNS, Out, TypeId, Indent, DB).
rdf_save_subject(Out, Subject, _DefNS, Atts, Indent, DB) :-
	format(Out, '~*|<rdf:Description', [Indent]),
	save_about(Out, Subject, Indent),
	save_attributes(Atts, rdf, Out, rdf:'Description', Indent, DB).

save_about(_Out, Subject, Indent) :-
	Indent > 0,
	anonymous_subject(Subject), !.
save_about(Out, Subject, _) :-
	rdf_value(Subject, QSubject),
	format(Out, ' rdf:about="~w"', [QSubject]).

%	save_attributes(+List, +DefNS, +Stream, Element)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Atts, DefNS, Out, Element, Indent, DB) :-
	split_attributes(Atts, InTag, InBody),
	SubIndent is Indent + 2,
	save_attributes2(InTag, DefNS, tag, Out, SubIndent, DB),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, _, body, Out, SubIndent, DB),
	    format(Out, '~N~*|</~w>~n', [Indent, Element])
	).

%	split_attributes(+Attributes, -Inline, -Body)
%
%	Split attributes for (literal) attributes to be used in the
%	begin-tag and ones that have to go into the body of the description.

split_attributes([], [], []).
split_attributes([H|TA], [H|TI], B) :-
	in_tag_attribute(H), !,
	split_attributes(TA, TI, B).
split_attributes([H|TA], I, [H|TB]) :-
	split_attributes(TA, I, TB).

in_tag_attribute(_=literal(Text)) :-
	atom_length(Text, Len),
	Len < 60.

%	save_attributes(+List, +DefNS, +TagOrBody, +Stream)
%
%	Save a list of attributes.

save_attributes2([], _, _, _, _, _).
save_attributes2([H|T], DefNS, Where, Out, Indent, DB) :-
	save_attribute(Where, H, DefNS, Out, Indent, DB),
	save_attributes2(T, DefNS, Where, Out, Indent, DB).

save_attribute(tag, Name=literal(Value), DefNS, Out, Indent, _DB) :-
	rdf_id(Name, DefNS, NameText),
	xml_quote_attribute(Value, QVal),
	format(Out, '~N~*|~w="~w"', [Indent, NameText, QVal]).
save_attribute(body, Name=literal(Value), DefNS, Out, Indent, _DB) :- !,
	rdf_local_id(Name, DefNS, NameText),
	xml_quote_cdata(Value, QVal),
	format(Out, '~N~*|<~w>~w</~w>', [Indent, NameText, QVal, NameText]).
save_attribute(body, Name=Value, DefNS0, Out, Indent, DB) :-
	anonymous_subject(Value), !,
	rdf_local_id(Name, DefNS0, DefNS, NameText),
	format(Out, '~N~*|<~w>~n', [Indent, NameText]),
	SubIndent is Indent + 2,
	rdf_save_subject(Out, Value, DefNS, SubIndent, DB),
	format(Out, '~N~*|</~w>~n', [Indent, NameText]).
save_attribute(body, Name=Value, DefNS, Out, Indent, _DB) :-
	rdf_value(Value, QVal),
	rdf_local_id(Name, DefNS, NameText),
	format(Out, '~N~*|<~w rdf:resource="~w"/>', [Indent, NameText, QVal]).

anonymous_subject(S) :-
	sub_atom(S, 0, _, _, 'Description__').


rdf_id(Id, NS, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_id(Id, e, e:Id).


rdf_local_id(Id, NS, Local) :-
	ns(NS, Full),
	atom_concat(Full, Local, Id), !.
rdf_local_id(Id, _, Text) :-
	rdf_id(Id, _, Text).

rdf_local_id(Id, NS, NS, Local) :-
	ns(NS, Full),
	atom_concat(Full, Local, Id), !.
rdf_local_id(Id, _, NS, Text) :-
	rdf_id(Id, NS, Text).

rdf_value(V, Text) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	concat_atom(['&', NS, (';'), Local], Text).
rdf_value(V, V).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(loaded(Spec, Triples, parsed(ParseTime)))) -->
	{   atom(Spec)
	->  file_base_name(Spec, Base)
	;   Base = Spec
	},
	[ 'Parsed "~w" in ~2f sec; added ~D triples'-
	  [Base, ParseTime, Triples]
	].
prolog:message(rdf(loaded(Spec, Triples, cache(ParseTime)))) -->
	{   atom(Spec)
	->  file_base_name(Spec, Base)
	;   Base = Spec
	},
	[ 'Loaded "~w" in ~2f sec; added ~D triples'-
	  [Base, ParseTime, Triples]
	].

