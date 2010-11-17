/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

:- module(rdf_library,
	  [ rdf_attach_library/1,	% +Dir
	    rdf_load_library/1,		% +Ontology
	    rdf_load_library/2,		% +Ontology, +Options
	    rdf_list_library/0,
	    rdf_list_library/1,		% +Ontology
	    rdf_list_library/2,		% +Ontology, +Options
	    rdf_library_index/2		% ?Id, ?Facet
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_turtle')).
:- use_module(library(rdf)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(date)).
:- use_module(library(uri)).
:- use_module(library(http/http_open)).
:- use_module(library(thread)).

/** <module> RDF Library Manager

This module manages an ontology library. Such   a  library consists of a
directory with manifest files named =|Manifest.rdf|= or =|Manifest.ttl|=
(Turtle). The manifest files define ontologies  appearing in the library
as well as namespace mnemonics and dependencies.

The typical usage scenario is

==
?- rdf_attach_library('/some/directory').
?- rdf_load_library(my_ontology).
==

@tbd	Add caching info
@author Jan Wielemaker
*/

:- rdf_register_ns(lib, 'http://www.swi-prolog.org/rdf/library/').

:- dynamic
	manifest/2,			% Path, Time
	library_db/3.			% Name, URL, Facets

%	Force compile-time namespace expansion

:- rdf_meta
	edge(+, r,r,o).

		 /*******************************
		 *	      LOADING		*
		 *******************************/

%%	rdf_load_library(+Id) is det.
%%	rdf_load_library(+Id, +Options) is det.
%
%	Load ontologies from the  library.  A   library  must  first  be
%	attached using rdf_attach_library/1.  Defined Options are:
%
%		* import(Bool)
%		If =true= (default), also load ontologies that are
%		explicitely imported.
%
%		* base_uri(URI)
%		BaseURI used for loading RDF.  Local definitions in
%		ontologies overrule this option.
%
%		* claimed_source(URL)
%		URL from which we claim to have loaded the data.
%
%		* not_found(+Level)
%		The system does a pre-check for the existence of
%		all references RDF databases.  If Level is =error=
%		it reports missing databases as an error and fails.
%		If =warning= it prints them, but continues.  If
%		=silent=, no checks are preformed.  Default is =error=.
%
%		* concurrent(Threads)
%		Perform the load concurrently using N threads.  If not
%		specified, the number is determined by
%		guess_concurrency/2.
%
%		* load(+Bool)
%		If =false=, to all the preparation, but do not execute
%		the actual loading.  See also rdf_list_library/2.

rdf_load_library(Id) :-
	rdf_load_library(Id, []).

rdf_load_library(Id, Options) :-
	load_commands(Id, Options, Pairs),
	pairs_values(Pairs, Commands),
	list_to_set(Commands, Cmds2),
	delete_virtual(Cmds2, Cmds3),
	find_conflicts(Cmds3),
	check_existence(Cmds3, Cmds, Options),
	(   option(concurrent(Threads), Options)
	->  true
	;   guess_concurrency(Cmds, Threads)
	),
	length(Cmds, NSources),
	print_message(informational, rdf(loading(NSources, Threads))),
	(   option(load(true), Options, true)
	->  concurrent(Threads, Cmds, [])
	;   true
	).

delete_virtual([], []).
delete_virtual([virtual(_)|T0], T) :- !,
	delete_virtual(T0, T).
delete_virtual([H|T0], [H|T]) :-
	delete_virtual(T0, T).


%%	find_conflicts(+LoadCommands) is semidet.
%
%	Find possibly conflicting options for loading the same source

find_conflicts(Commands) :-
	no_source_with_different_options(Commands),
	no_sources_in_same_graph(Commands).

%%	no_source_with_different_options(+Commands) is semidet.
%
%	True if there are not multiple calls to load the same graph, but
%	with  different  load-options.  Prints  a    warning  and  fails
%	otherwise.

no_source_with_different_options(Commands) :-
	sort(Commands, Cmds),
	conflicts(Cmds, Conflicts),
	report_conflicts(Conflicts),
	Conflicts == [].

conflicts([], []).
conflicts([C1, C2|T0], [C1-C2|T]) :-
	conflict(C1, C2), !,
	conflicts([C2|T0], T).
conflicts([_|T0], T) :-
	conflicts(T0, T).

conflict(rdf_load(Src, Options1), rdf_load(Src, Options2)) :-
	sort(Options1, S1),
	sort(Options2, S2),
	S1 \== S2.

report_conflicts([]).
report_conflicts([C1-C2|T]) :-
	print_message(warning, rdf(load_conflict(C1,C2))),
	report_conflicts(T).

%%	no_sources_in_same_graph(+Commands) is semidet.
%
%	True if there are not two load   commands  referring to the same
%	graph.

no_sources_in_same_graph(Commands) :-
	map_list_to_pairs(command_graph, Commands, Keyed),
	keysort(Keyed, KeySorted),
	group_pairs_by_key(KeySorted, SourcesByGraph),
	(   member(Graph-Sources, SourcesByGraph),
	    Sources = [_,_|_]
	->  forall(( member(Graph-Sources, SourcesByGraph),
	             Sources = [_,_|_]
		   ),
		   print_message(error,
				 rdf(multiple_source_for_graph(Graph, Sources)))),
	    fail
	;   true
	).

command_graph(rdf_load(_, Options), Graph) :-
	option(graph(Graph), Options), !.
command_graph(rdf_load(URL, _), URL) :- !.
command_graph(_, _).			% Other command.  Each variable it its own key


%%	check_existence(+CommandsIn, -Commands, +Options) is det.
%
%	Report existence errors. Fail if at   least  one source does not
%	exist. and the not_found level is not =silent=.
%
%	@error existence_error(urls, ListOfUrls)

check_existence(CommandsIn, Commands, Options) :-
	option(not_found(Level), Options, error),
	must_be(oneof([error,warning,silent]), Level),
	(   Level == silent
	->  true
	;   missing_urls(CommandsIn, Commands, Missing),
	    (	Missing == []
	    ->	true
	    ;	Level == warning
	    ->	report_missing(Missing, Level)
	    ;	existence_error(urls, Missing)
	    )
	).


missing_urls([], [], []).
missing_urls([H|T0], Cmds, Missing) :-
	H = rdf_load(URL, _),
	(   catch(exists_url(URL), error(existence_error(_,_), _), fail)
	->  Cmds = [H|T],
	    missing_urls(T0, T, Missing)
	;   Missing = [URL|T],
	    missing_urls(T0, Cmds, T)
	).

report_missing([], _).
report_missing([H|T], Level) :-
	print_message(Level, error(existence_error(url, H), _)),
	report_missing(T, Level).

%%	guess_concurrency(+Commands, -Threads) is det.
%
%	How much concurrency to use? Set to   the  number of CPUs if all
%	input comes from  files  or  5   if  network  based  loading  is
%	demanded.

guess_concurrency(Commands, Threads) :-
	count_uris(Commands, FileURLs, OtherURLs),
	(   FileURLs > 0
	->  (   current_prolog_flag(cpu_count, CPUs)
	    ->  true
	    ;   CPUs = 1
	    ),
	    FileThreads is min(FileURLs, CPUs)
	;   FileThreads = 0
	),
	(   OtherURLs > 0
	->  OtherThreads is min(5, OtherURLs)
	;   OtherThreads = 0
	),
	Threads is FileThreads + OtherThreads.

count_uris([], 0, 0).
count_uris([rdf_load(URL, _)|T], F, NF) :-
	count_uris(T, F0, NF0),
	(   sub_atom(URL, 0, _, _, 'file://')
	->  F is F0 + 1,
	    NF = NF0
	;   NF is NF0 + 1,
	    F = F0
	).


%%	load_commands(+Id, +Options, -Pairs:list(Level-Command)) is det.
%
%	Commands are the RDF commands to execute for rdf_load_library/2.
%	Splitting  in  command  collection  and   execution  allows  for
%	concurrent execution as well  as   forward  checking of possible
%	problems.
%
%	@tbd	Fix poor style; avoid assert/retract.

:- thread_local
	command/2.

load_commands(Id, Options, Commands) :-
	retractall(command(_,_)),
	rdf_update_library_index,
	dry_load(Id, 1, Options),
	findall(Level-Cmd, retract(command(Level, Cmd)), Commands).

dry_load(Id, Level, Options) :-
	(   library(Id, File, Facets)
	->  merge_base_uri(Facets, Options, Options1),
	    merge_source(Facets, Options1, Options2),
	    merge_blanks(Facets, Options2, Options3),
	    (   \+ memberchk(virtual, Facets)
	    ->  load_options(Options3, File, RdfOptions),
		assert(command(Level, rdf_load(File, RdfOptions)))
	    ;	assert(command(Level, virtual(File)))
	    ),
	    (	option(import(true), Options, true)
	    ->	Level1 is Level + 1,
	        forall(member(imports(_, Import), Facets),
		       import(Import, Level1, Options3))
	    ;	true
	    )
	;   existence_error(ontology, Id)
	).

merge_base_uri(Facets, Options0, Options) :-
	(   option(base_uri(Base), Facets)
	->  delete(Options0, base_uri(_), Options1),
	    Options = [base_uri(Base)|Options1]
	;   Options = Options0
	).

merge_source(Facets, Options0, Options) :-
	(   option(claimed_source(Base), Facets)
	->  delete(Options0, claimed_source(_), Options1),
	    Options = [claimed_source(Base)|Options1]
	;   Options = Options0
	).

merge_blanks(Facets, Options0, Options) :-
	(   option(blank_nodes(Share), Facets)
	->  delete(Options0, blank_nodes(_), Options1),
	    Options = [blank_nodes(Share)|Options1]
	;   Options = Options0
	).

load_options(Options, File, RDFOptions) :-
	findall(O, load_option(Options, File, O), RDFOptions).

load_option(Options, File, graph(Source)) :-
	option(claimed_source(Source0), Options),
	(   sub_atom(Source0, _, _, 0, /)
	->  file_base_name(File, Base),
	    atom_concat(Source0, Base, Source)
	;   atom_concat(Source, #, Source0)
	->  true
	).
load_option(Options, File, base_uri(BaseURI)) :-
	option(base_uri(Base0), Options),
	sub_atom(/, _, _, 0, Base0),
	atom_concat(Base0, File, BaseURI).
load_option(Options, _File, blank_nodes(Share)) :-
	option(blank_nodes(Share), Options).

%%	import(+URL, +Level, +Options) is det.

import(Path, Level, Options) :-
	(   (   library(Id, Path, _)
	    ->	true
	    ;	manifest_for_path(Path, Manifest),
		catch(exists_url(Manifest), _, fail)
	    ->  process_manifest(Manifest),
		library(Id, Path, _)
	    )
	->  dry_load(Id, Level, Options)
	;   load_options(Options, Path, RdfOptions),
	    assert(command(Level, rdf_load(Path, RdfOptions)))
	).

manifest_for_path(URL, Manifest) :-
	file_directory_name(URL, Parent),
	manifest_file(Base),
	rdf_extension(Ext),
	atomic_list_concat([Parent, /, Base, '.', Ext], Manifest).

%%	rdf_list_library(+Id) is det.
%%	rdf_list_library(+Id, +Options) is det.
%
%	Print library dependency tree to the terminal.  Options include
%	options for rdf_load_library/2 and
%
%		* show_source(+Boolean)
%		If =true= (default), show location we are loading
%
%		* show_graph(+Boolean)
%		If =true= (default =false=), show name of graph
%
%		* show_virtual(+Boolean)
%		If =false= (default =true=), do not show virtual
%		repositories.
%
%		* indent(Atom)
%		Atom repeated for indentation levels

rdf_list_library(Id) :-
	rdf_list_library(Id, []).
rdf_list_library(Id, Options) :-
	load_commands(Id, Options, Commands),
	maplist(print_load(Options), Commands).

print_load(Options, _Level-virtual(_)) :-
	option(show_virtual(false), Options), !.
print_load(Options, Level-Command) :-
	option(indent(Indent), Options, '. '),
	forall(between(2, Level, _), format(Indent)),
	print_command(Command, Options),
	format('~N').

print_command(virtual(URL), _Options) :-
	format('<~w>', [URL]).
print_command(rdf_load(URL), Options) :-
	print_command(rdf_load(URL, []), Options).
print_command(rdf_load(URL, RDFOptions), Options) :-
	(   option(show_source(true), Options, true)
	->  format('~w', [URL]),
	    (   option(blank_nodes(noshare), RDFOptions)
	    ->  format(' <not shared>')
	    ;   true
	    ),
	    (   exists_url(URL)
	    ->  true
	    ;   format(' [NOT FOUND]')
	    )
	;   true
	),
	(   option(show_graph(true), Options, false),
	    option(graph(Base), RDFOptions)
	->  format('~N\tSource: ~w', [Base])
	;   true
	).

exists_url(URL) :-
	rdf_db:rdf_input(URL, Source, _BaseURI),
	existing_url_source(Source).

existing_url_source(file(Path)) :- !,
	access_file(Path, read).
existing_url_source(url(http, URL)) :- !,
	catch(http_open(URL, Stream, [ method(head) ]), _, fail),
	close(Stream).


%%	rdf_list_library
%
%	Prints known RDF library identifiers to current output.

rdf_list_library :-
	rdf_update_library_index,
	(   rdf_library_index(Id, title(Title)),
	    format('~w ~t~20|~w', [Id, Title]),
	    (	rdf_library_index(Id, version(Version))
	    ->	format(' (version ~w)', [Version])
	    ;	true
	    ),
	    nl,
	    fail
	;   true
	).


%%	rdf_library_index(?Id, ?Facet) is nondet.
%
%	Query the content of the library.  Defined facets are:
%
%		* source(URL)
%		Location from which to load the ontology
%
%		* title(Atom)
%		Title used for the ontology
%
%		* comment(Atom)
%		Additional comments for the ontology
%
%		* version(Atom)
%		Version information on the ontology
%
%		* imports(Type, URL)
%		URLs needed by this ontology. May succeed multiple
%		times.  Type is one of =ontology=, =schema= or =instances=.
%
%		* base_uri(BaseURI)
%		Base URI to use when loading documents. If BaseURI
%		ends in =|/|=, the actual filename is attached.
%
%		* claimed_source(Source)
%		URL from which we claim to have loaded the RDF. If
%		Source ends in =|/|=, the actual filename is
%		attached.
%
%		* blank_nodes(Share)
%		Defines how equivalent blank nodes are handled, where
%		Share is one of =share= or =noshare=.  Default is to
%		share.
%
%		* provides_ns(URL)
%		Ontology provides definitions in the namespace URL.
%		The formal definition of this is troublesome, but in
%		practice it means the ontology has triples whose
%		subjects are in the given namespace.
%
%		* uses_ns(URL)
%		The ontology depends on the given namespace.  Normally
%		means it contains triples that have predicates or
%		objects in the given namespace.
%
%		* manifest(Path)
%		Manifest file this ontology is defined in
%
%		* virtual
%		Entry is virtual (cannot be loaded)

rdf_library_index(Id, Facet) :-
	library(Id, Path, Facets),
	(   Facet = source(Path)
	;   member(Facet, Facets)
	).


		 /*******************************
		 *	MANIFEST PROCESSING	*
		 *******************************/

%%	rdf_attach_library(+Source)
%
%	Attach manifest from Source.  Source is one of
%
%		* URL
%		Load single manifest from this URL
%		* File
%		Load single manifest from this file
%		* Directory
%		Scan all subdirectories and load all =|Manifest.ttl|= or
%		=|Manifest.rdf|= found.  If Directory is a path-alias
%		(e.g., ontology(.)), _all_ referenced directories are
%		scanned for manifest files.
%
%	Encountered namespaces are registered   using rdf_register_ns/2.
%	Encountered ontologies are added to the index. If a manifest was
%	already loaded it will be reloaded  if the modification time has
%	changed.

rdf_attach_library(URL) :-
	atom(URL),
	uri_is_global(URL),
	\+ is_absolute_file_name(URL), !, % avoid interpreting C: as a schema
	process_manifest(URL).
rdf_attach_library(File) :-
	absolute_file_name(File, Path,
			   [ extensions([rdf,ttl]),
			     access(read),
			     file_errors(fail)
			   ]), !,
	process_manifest(Path).
rdf_attach_library(Dir) :-
	forall(absolute_file_name(Dir, Path,
				  [ file_type(directory),
				    access(read),
				    solutions(all)
				  ]),
	       attach_dir(Path, [])).


%%	rdf_update_library_index
%
%	Reload all Manifest files.

rdf_update_library_index :-
	forall(manifest(Location, _Time),
	       process_manifest(Location)).

attach_dir(Path, Visited) :-
	memberchk(Path, Visited), !.
attach_dir(Path, Visited) :-
	atom_concat(Path, '/*', Pattern),
	expand_file_name(Pattern, Members),
	(   member(Manifest, Members),
	    is_manifest_file(Manifest)
	->  process_manifest(Manifest)
	;   print_message(silent, rdf(no_manifest(Path)))
	),
	(   member(Dir, Members),
	    exists_directory(Dir),
	    file_base_name(Dir, Base),
	    \+ hidden_base(Base),
	    attach_dir(Dir, [Path|Visited]),
	    fail ; true
	).

hidden_base('CVS').
hidden_base('cvs').			% Windows

%%	process_manifest(+Location) is det.
%
%	Process a manifest file, registering  encountered namespaces and
%	creating clauses for library/3. No op if manifest was loaded and
%	not changed. Removes old data if the manifest was changed.
%
%	@param	Location is either a path name or a URL.

process_manifest(Source) :-
	(   uri_file_name(Source, Manifest0)
	->  absolute_file_name(Manifest0, Manifest)
	;   absolute_file_name(Source, Manifest)
	),				% Manifest is a canonical filename
	source_time(Manifest, MT),
	(   manifest(Manifest, Time),
	    (	MT =< Time
	    ->  !
	    ;	retractall(manifest(Manifest, Time)),
	        library_db(Id, URL, Facets),
		memberchk(manifest(Manifest), Facets),
		retractall(library_db(Id, URL, Facets)),
		fail
	    )
	;   read_triples(Manifest, Triples),
	    process_triples(Manifest, Triples),
	    print_message(informational, rdf(manifest(loaded, Manifest))),
	    assert(manifest(Manifest, MT))
	).

process_triples(Manifest, Triples) :-
	findall(ns(Mnemonic, NameSpace),
		extract_namespace(Triples, Mnemonic, NameSpace),
		NameSpaces),
	findall(Ontology,
		extract_ontology(Triples, Ontology),
		Ontologies),
	maplist(define_namespace, NameSpaces),
	maplist(assert_ontology(Manifest), Ontologies).

%%	extract_namespace(+Triples, -Mnemonic, -NameSpace)
%
%	True if Mnemonic is an abbreviation of NameSpace.

extract_namespace(Triples, Mnemonic, Namespace) :-
	edge(Triples, Decl, lib:mnemonic, literal(Mnemonic)),
	edge(Triples, Decl, lib:namespace, Namespace).

%%	extract_ontology(+Triples, -Ontology) is nondet.
%
%	Extract definition of an ontology

extract_ontology(Triples, library(Name, URL, Options)) :-
	edge(Triples, URL, rdf:type, Type),
	(   ontology_type(Type)
	->  file_base_name(URL, BaseName),
	    file_name_extension(Name, _, BaseName),
	    findall(Facet, facet(Triples, URL, Facet), Options)
	).

ontology_type(X) :-
	(   rdf_equal(X, lib:'Ontology')
	;   rdf_equal(X, lib:'Schema')
	;   rdf_equal(X, lib:'Instances')
	).

%%	facet(+Triples, +File, -Facet) is nondet.
%
%	Enumerate facets about File from   Triples. Facets are described
%	with rdf_library_index/2.

facet(Triples, File, title(Title)) :-
	edge(Triples, File, dc:title, literal(Title)).
:- if(rdf_current_ns(dcterms, _)).
facet(Triples, File, title(Title)) :-
	edge(Triples, File, dcterms:title, literal(Title)).
:- endif.
facet(Triples, File, version(Version)) :-
	edge(Triples, File, owl:versionInfo, literal(Version)).
facet(Triples, File, comment(Comment)) :-
	edge(Triples, File, rdfs:comment, literal(Comment)).
facet(Triples, File, base_uri(BaseURI)) :-
	edge(Triples, File, lib:baseURI, BaseURI).
facet(Triples, File, claimed_source(Source)) :-
	edge(Triples, File, lib:source, Source).
facet(Triples, File, blank_nodes(Mode)) :-
	edge(Triples, File, lib:blankNodes, literal(Mode)),
	must_be(oneof([share,noshare]), Mode).
facet(Triples, File, imports(ontology, Path)) :-
	edge(Triples, File, owl:imports, Path).
facet(Triples, File, imports(schema, Path)) :-
	edge(Triples, File, lib:schema, Path).
facet(Triples, File, imports(instances, Path)) :-
	edge(Triples, File, lib:instances, Path).
facet(Triples, File, provides_ns(NS)) :-
	edge(Triples, File, lib:providesNamespace, NSDecl),
	edge(Triples, NSDecl, lib:namespace, NS).
facet(Triples, File, uses_ns(NS)) :-
	edge(Triples, File, lib:usesNamespace, NSDecl),
	edge(Triples, NSDecl, lib:namespace, NS).
facet(Triples, File, virtual) :-
	edge(Triples, File, rdf:type, lib:'Virtual').

%%	edge(+Triples, ?S, ?P, ?O) is nondet.
%
%	Like rdf/3 over a list of Triples.

edge(Triples, S, P, O) :-
	member(rdf(S,P,O), Triples).

%%	source_time(+Source, -Modified) is semidet.
%
%	Modified is the last modification time of Source.
%
%	@error	existence_error(Type, Source).

source_time(URL, Modified) :-
	sub_atom(URL, 0, _, _, 'http://'), !,
	http_open(URL, Stream,
		  [ header(last_modified, Date),
		    method(head)
		  ]),
	close(Stream),
	Date \== '',
	parse_time(Date, Modified).
source_time(URL, Modified) :-
	uri_file_name(URL, File), !,
	time_file(File, Modified).
source_time(File, Modified) :-
	time_file(File, Modified).


%%	read_triples(+File, -Triples) is det.
%
%	Read RDF/XML or Turtle file into a list of triples.

read_triples(File, Triples) :-
	file_name_extension(_, rdf, File), !,
	load_rdf(File, Triples).
read_triples(File, Triples) :-
	file_name_extension(_, ttl, File), !,
	rdf_load_turtle(File, Triples, []).

%%	is_manifest_file(+Path)
%
%	True if Path is the name of a manifest file.

is_manifest_file(Path) :-
	file_base_name(Path, File),
	downcase_atom(File, Lwr),
	file_name_extension(Base, Ext, Lwr),
	manifest_file(Base),
	rdf_extension(Ext), !.

manifest_file('Manifest').
manifest_file('manifest').

rdf_extension(ttl).
rdf_extension(rdf).


%%	assert_ontology(+Manifest, +Term:library(Name, File, Facets)) is det.
%
%	Add ontology to our library.
%
%	@tbd	Proper behaviour of re-definition?

assert_ontology(Manifest, Term) :-
	Term = library(Name, URL, Facets),
	(   library(Name, _URL2, Facets2)
	->  memberchk(manifest(Manifest2), Facets2),
	    print_message(warning, rdf(redefined(Manifest, Name, Manifest2)))
	;   true
	),
	assert(library_db(Name, URL,
		       [ manifest(Manifest)
		       | Facets
		       ])).


%%	library(?Id, ?URL, ?Facets)
%
%	Access DB for library information.

library(Id, URL, Facets) :-
	nonvar(URL),
	normalize_url(URL, CanonicalURL),
	library_db(Id, CanonicalURL, Facets).
library(Id, URL, Facets) :-
	library_db(Id, URL, Facets).

%%	normalize_url(+URL, -Normalized)
%
%	Like uri_normalized/2, but we  also   need  (platform dependent)
%	filename canonization.

normalize_url(URL, CanonicalURL) :-
	uri_file_name(URL, File), !,
	absolute_file_name(File, CanFile),
	uri_file_name(CanonicalURL, CanFile).
normalize_url(URL, CanonicalURL) :-
	uri_normalized(URL, CanonicalURL).

%%	define_namespace(NS:ns(Mnemonic, Namespace)) is det.
%
%	Add namespace declaration for Mnemonic.

define_namespace(ns(Mnemonic, Namespace)) :-
	debug(rdf_library, 'Adding NS ~w = ~q', [Mnemonic, Namespace]),
	rdf_register_ns(Mnemonic, Namespace,
			[
			]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(no_manifest(Path))) -->
	[ 'Directory ~w has no Manifest.{ttl,rdf} file'-[Path] ].
prolog:message(rdf(redefined(Manifest, Name, Manifest2))) -->
	[ '~w: Ontology ~w already defined in ~w'-
	  [Manifest, Name, Manifest2]
	].
prolog:message(rdf(manifest(loaded, Manifest))) -->
	[ 'Loaded RDF manifest ~w'-[Manifest]
	].
prolog:message(rdf(load_conflict(C1, C2))) -->
	[ 'Conflicting loads: ~p <-> ~p'-[C1, C2] ].
prolog:message(rdf(multiple_source_for_graph(Graph, Sources))) -->
	[ 'Multiple sources for graph ~p:'-[Graph] ],
	sources(Sources).
prolog:message(rdf(loading(Files, Threads))) -->
	[ 'Loading ~D files using ~D threads ...'-[Files, Threads] ].

sources([]) --> [].
sources([rdf_load(From, _Options)|T]) -->
	[ nl, '\t~p'-[From] ],
	sources(T).
