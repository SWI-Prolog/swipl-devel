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

:- module(rdf_turtle_write,
	  [ rdf_save_turtle/2,		% +File, +Options
	    rdf_save_canonical_turtle/2	% +File, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle_base)).
:- use_module(library(option)).
:- use_module(library(record)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(rbtrees)).
:- use_module(library(apply)).
:- use_module(library(url)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(sgml_write)).


/** <module> Turtle - Terse RDF Triple Language writer

This module implements the Turtle  language   for  representing  the RDF
triple model as defined by Dave Beckett  from the Institute for Learning
and Research Technology University of Bristol in the document:

  * http://www.w3.org/TeamSubmission/turtle/
  * http://www.w3.org/TeamSubmission/2008/SUBM-turtle-20080114/#sec-conformance

The Turtle format is designed as an   RDF  serialization that is easy to
read and write by both machines and  humans. Due to the latter property,
this library goes a long way in trying to produce human-readable output.

In addition to the  human-readable  format,   this  library  can write a
_canonical_ representation of RDF graphs.   The canonical representation
has the following properties:

  * Equivalent graphs result in the same document.  Graphs are
  considered equivalent iff they contain the same _set_ of
  triples, regardless of the labeling of blank nodes in the
  graph.

  * Changes to the graph are diff-friendly.  This means

    - Prefixes are combined in the header and thus changes
    to the namespaces only result in changes in the header.
    - Blank nodes that are used only once (including collections)
    are written in-line with the object they belong to.
    - For other blank nodes we to realise stable labeling that
    is based on property-values.

@tbd	Low-level string output takes 28% of the time.  Move to C?
*/

:- record
	tw_state(graph,			% graph being saved
		 base,			% The base-URI
		 encoding=utf8,		% Desired encoding
		 indent:nonneg=8,	% Indent for ; and ,-lists
		 tab_distance:nonneg=8,	% Tab distance
		 silent:boolean=false,	% If true, do not print a message
		 subject_white_lines:nonneg=1,%Extra lines between subjects
		 align_prefixes:boolean=true,%Align prefix declarations
		 user_prefixes:boolean=true,% Use rdf_current_ns/2?
		 only_known_prefixes:boolean=false,% Only use known prefixes
		 comment:boolean=true,	% write some comments into the file
		 group:boolean=true,	% Group using ; and ,
		 single_line_bnodes:boolean=false, % No newline after ;
		 canonize_numbers:boolean=false, % How to write numbers
		 canonical:boolean=false,
		 expand:any=lookup,	% Access to the triples
					% Private fields
		 bnode_id=0,		% Incrementing bnode-id
		 nodeid_map,		% RBTree mapping NodeIDs to Refs
		 bnode_hash,		% RBTree holding reuse-count of hashes
		 subject_count,		% # subjects saved
		 triple_count=0,	% # triples saved
		 base_root,		% Root URL of base
		 base_dir,		% Directory
		 base_path,		% Path of base
		 prefix_map).		% List of Prefix-Map


:- meta_predicate
	rdf_save_turtle(+, :),
	rdf_save_canonical_turtle(+, :).

%%	rdf_save_turtle(+Out, :Options) is det.
%
%	Save an RDF graph as Turtle.  Options processed are:
%
%	    * align_prefixes(+Boolean)
%	    Nicely align the @prefix declarations
%	    * base(+Base)
%	    Save relative to the given Base
%	    * canonize_numbers(+Boolean)
%	    If =true= (default =false=), emit numeric datatypes using
%	    Prolog's write to achieve canonical output.
%	    * comment(+Boolean)
%	    It =true= (default), write some informative comments
%	    between the output segments
%	    * encoding(+Encoding)
%	    Encoding used for the output stream.  Default is UTF-8.
%	    * expand(:Goal)
%	    Query an alternative graph-representation.  See below.
%	    * indent(+Column)
%	    Indentation for ; -lists.  `0' does not indent, but
%	    writes on the same line.  Default is 8.
%	    * graph(+Graph)
%	    Save only the named graph
%	    * group(+Boolean)
%	    If =true= (default), using P-O and O-grouping.
%	    * only_known_prefixes(+Boolean)
%	    Only use prefix notation for known prefixes.  Without, some
%	    documents produce _huge_ amounts of prefixes.
%	    * silent(+Boolean)
%	    If =true= (default =false=), do not print the final
%	    informational message.
%	    * single_line_bnodes(+Bool)
%	    If =true= (default =false=), write [...] and (...) on a
%	    single line.
%	    * subject_white_lines(+Count)
%	    Extra white lines to insert between statements about a
%	    different subject.  Default is 1.
%	    * tab_distance(+Tab)
%	    Distance between tab-stops.  `0' forces the library to
%	    use only spaces for layout.  Default is 8.
%	    * user_prefixes(+Boolean)
%	    If =true= (default), use prefixes from rdf_current_ns/2.
%
%	The option =expand= allows  for   serializing  alternative graph
%	representations. It is called through   call/5,  where the first
%	argument is the expand-option, followed  by   S,P,O,G.  G is the
%	graph-option (which is by  default   a  variable).  This notably
%	allows for writing RDF graphs   represented  as rdf(S,P,O) using
%	the following code fragment:
%
%	    ==
%	    triple_in(RDF, S,P,O,_G) :-
%	    	member(rdf(S,P,O), RDF).
%
%	    	...,
%	        rdf_save_turtle(Out, [ expand(triple_in(RDF)) ]),
%	    ==
%
%	@param	Out is one of stream(Stream), a stream handle, a file-URL
%		or an atom that denotes a filename.

rdf_save_turtle(Spec, QOptions) :-
	meta_options(is_meta, QOptions, Options),
	thread_self(Me),
	thread_statistics(Me, cputime, T0),
	must_be(list, Options),
	make_tw_state(Options, State0, _Rest),
	init_base(State0, State1),
	init_prefix_map(State1, State),
	tw_state_encoding(State, Enc),
	open_output(Spec, Enc, Stream, Cleanup),
	call_cleanup(tw_graph(State, Stream),
		     Cleanup),
	thread_statistics(Me, cputime, T1),
	Time is T1-T0,
	tw_state_triple_count(State, SavedTriples),
	tw_state_subject_count(State, SavedSubjects),
	(   tw_state_silent(State, true)
	->  true
	;   print_message(informational,
			  rdf(saved(Spec, Time, SavedSubjects, SavedTriples)))
	).

is_meta(expand).

%%	rdf_save_canonical_turtle(+Spec, +Options) is det.
%
%	Save triples in  a  canonical  format.   This  is  the  same  as
%	rdf_save_turtle/3, but using different defaults. In particular:
%
%	    * encoding(utf8),
%	    * indent(0),
%	    * tab_distance(0),
%	    * subject_white_lines(1),
%	    * align_prefixes(false),
%	    * user_prefixes(false)
%	    * comment(false),
%	    * group(false),
%	    * single_line_bnodes(true)
%
%	@tbd Work in progress. Notably blank-node handling is
%	incomplete.

rdf_save_canonical_turtle(Spec, M:Options) :-
	rdf_save_turtle(Spec,
			M:[ encoding(utf8),
			    indent(0),
			    tab_distance(0),
			    subject_white_lines(1),
			    align_prefixes(false),
			    user_prefixes(false),
			    comment(false),
			    group(false),
			    single_line_bnodes(true),
			    canonical(true)
			  | Options
			  ]).

%%	open_output(+Spec, +Encoding, -Stream, -Cleanup) is det.
%
%	Open output Spec, returning a stream using Encoding.
%
%	@param	Cleanup is a goal that must be used to revert the side
%		effects of open_output/4.

open_output(stream(Out), Encoding, Out,
	    set_stream(Out, encoding(Old))) :- !,
	stream_property(Out, encoding(Old)),
	set_stream(Out, encoding(Encoding)).
open_output(Stream, Encoding, Out, Cleanup) :-
	\+ atom(Stream),
	is_stream(Stream), !,
	open_output(stream(Stream), Encoding, Out, Cleanup).
open_output(Spec, Encoding, Out,
	    close(Out)) :-
	out_to_file(Spec, File),
	open(File, write, Out, [encoding(Encoding)]).

out_to_file(URL, File) :-
	atom(URL),
	file_name_to_url(File, URL), !.
out_to_file(File, File).


		 /*******************************
		 *	      PREFIXES		*
		 *******************************/

%%	init_prefix_map(+State, -State) is det.
%
%	Set  the  prefix_map  of  State.  The  prefix  map  is  list  of
%	Prefix-URI of prefixes to use for   emitting the graph requested
%	in State. If multiple prefixes are present   where  the one is a
%	prefix of the other, the longer one appears first in the list.

init_prefix_map(State0, State) :-
	tw_state_graph(State0, Graph),
	tw_state_expand(State0, Expand),
	tw_state_only_known_prefixes(State0, OnlyKnown),
	rdf_graph_prefixes(Graph, Prefixes,
			   [ filter(turtle_prefix(OnlyKnown)),
			     expand(Expand),
			     min_count(2)
			   ]),
	remove_base(State0, Prefixes, Prefixes2),
	prefix_names(Prefixes2, State0, Pairs),
	transpose_pairs(Pairs, URI_Abrevs),
	reverse(URI_Abrevs, RURI_Abrevs),
	flip_pairs(RURI_Abrevs, PrefixMap),
	set_prefix_map_of_tw_state(PrefixMap, State0, State).


%%	turtle_prefix(+OnlyKnown, +Where, +Prefix, +URI) is semidet.
%
%	Test whether we want  to  include   the  proposed  prefix in the
%	@prefix declaration.

turtle_prefix(true, _, Prefix, _) :- !,
	rdf_current_ns(_, Prefix), !.
turtle_prefix(_, _, Prefix, URI) :-
	sub_atom(Prefix, _, 1, 0, Last),
	turtle_prefix_char(Last),
	atom_concat(Prefix, Local, URI),
	\+ sub_atom(Local, _, _, _, '.').

turtle_prefix_char('#').
turtle_prefix_char('/').


remove_base(State, Prefixes, PrefixesNoBase) :-
	tw_state_base_dir(State, BaseDir),
	atom(BaseDir), !,
	delete(Prefixes, BaseDir, PrefixesNoBase).
remove_base(_State, Prefixes, Prefixes).

flip_pairs([], []).
flip_pairs([Key-Val|Pairs], [Val-Key|Flipped]) :-
	flip_pairs(Pairs, Flipped).

prefix_names(URIs, State, Prefixes) :-
	prefix_names(URIs, State, 1, Prefixes, []).

prefix_names([], _, _, List, List) :- !.
prefix_names(URIs, State, Len, Prefixes, Tail) :-
	prefix_names(URIs, State, Len, Prefixes, PTail, Rest),
	Len1 is Len + 1,
	prefix_names(Rest, State, Len1, PTail, Tail).

prefix_names(URIs, State, Len, Prefixes, PTail, Rest) :-
	map_list_to_pairs(propose_abbrev(State, Len), URIs, Pairs), !,
	keysort(Pairs, Sorted),
	unique(Sorted, Prefixes, PTail, Rest).
prefix_names(URIs, _, _, Prefixes, PTail, []) :-
	number_prefixes(URIs, 1, Prefixes, PTail).

number_prefixes([], _, PL, PL).
number_prefixes([H|T0], N, [P-H|PL], T) :-
	atomic_concat(ns, N, P),
	succ(N, N1),
	number_prefixes(T0, N1, PL, T).

unique([], L, L, []).
unique([A-U|T0], [A-U|T], L, Rest) :-
	T0 \= [A-_|_], !,
	unique(T0, T, L, Rest).
unique([A-U|T0], Prefixes, L, [U|Rest0]) :-
	strip_keys(T0, A, T1, Rest0, Rest),
	unique(T1, Prefixes, L, Rest).

strip_keys([A-U|T0], A, T, [U|R0], R) :- !,
	strip_keys(T0, A, T, R0, R).
strip_keys(L, _, L, R, R).


%%	propose_abbrev(+State, +Len, +URI, -Abbrev) is multi.
%
%	Propose an abbreviation for URI.  Backtracking yields longer
%	ones.

propose_abbrev(_, _, URI, Abbrev) :-
	well_known_ns(Abbrev, URI), !.
propose_abbrev(State, _, URI, Abbrev) :-
	tw_state_user_prefixes(State, true),
	rdf_current_ns(Abbrev, URI), !.
propose_abbrev(_, Len, URI, Abbrev) :-
	namespace_parts(URI, Parts),
	include(abbrev_part, Parts, Names),
	reverse(Names, RevNames),
	length(Use, Len),
	append(Use, _, RevNames),
	atomic_list_concat(Use, -, Abbrev).

abbrev_part(X) :-
	turtle_name(X),
	\+ well_known_ns(X, _),
	\+ well_known_extension(X).

well_known_ns(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
well_known_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
well_known_ns(owl,  'http://www.w3.org/2002/07/owl#').
well_known_ns(xsd,  'http://www.w3.org/2001/XMLSchema#').
well_known_ns(dc,   'http://purl.org/dc/elements/1.1/').

well_known_extension(ttl).
well_known_extension(nt).
well_known_extension(n3).
well_known_extension(xml).
well_known_extension(rdf).
well_known_extension(owl).

%%	namespace_parts(+URL, -Parts)

namespace_parts(URL, Parts) :-
	atom_codes(URL, Codes),
	phrase(parts(Parts), Codes), !.
namespace_parts(URL, _) :-
	format(user_error, 'Couldn\'t split ~q~n', [URL]),
	fail.

parts(List) -->	sep2, parts2(List).

parts2([H|T]) -->
	string(Codes), 	{Codes \== []},
	sep, !,
	{atom_codes(H, Codes)},
	parts2(T).
parts2([]) --> [].

string([]) --> [].
string([H|T]) --> [H], string(T).

sep -->	sep_char, sep2.
sep([], []).

sep2 --> sep_char, !, sep2.
sep2 --> [].

sep_char --> "/".
sep_char --> ":".
sep_char --> ".".
sep_char --> "?".
sep_char --> "#".


%%	init_base(+State0, -State) is det.
%
%	Initialise dealing with the base URI.  It sets two attributes of
%	the state: base_root and base_path.

init_base(State0, State) :-
	tw_state_base(State0, BaseURI),
	atom(BaseURI), !,
	parse_url(BaseURI, Attributes),
	include(root_part, Attributes, RootAttrs),
	parse_url(BaseRoot, RootAttrs),
	memberchk(path(BasePath), Attributes),
	file_directory_name(BasePath, BaseDir),
	atomic_list_concat([BaseRoot, BaseDir, /], BaseDirURI),
	set_base_root_of_tw_state(BaseRoot, State0, State1),
	set_base_path_of_tw_state(BasePath, State1, State2),
	set_base_dir_of_tw_state(BaseDirURI, State2, State).
init_base(State, State).

root_part(protocol(_)).
root_part(host(_)).
root_part(port(_)).


		 /*******************************
		 *	        SAVE		*
		 *******************************/

%%	tw_graph(+State, +Out) is det.
%
%	Write an RDF graph as Turtle data.
%
%	@tbd Write unconnected and multi-connected blank-nodes.

tw_graph(State, Out) :-
	tw_state_prefix_map(State, PrefixMap),
	tw_prefix_map(PrefixMap, State, Out),
	subjects(State, Subjects),
	length(Subjects, SubjectCount),
	tw_state_subject_count(State, SubjectCount),
	partition(rdf_is_bnode, Subjects, BNodes, ProperSubjects),
	maplist(pair_var, BNodes, Pairs),
	ord_list_to_rbtree(Pairs, BNTree),
	tw_state_nodeid_map(State, BNTree),
	(   ProperSubjects == []
	->  true
	;   length(ProperSubjects, PSCount),
	    comment(State, 'Named toplevel resources (~D)', [PSCount], Out),
	    tw_proper_subjects(ProperSubjects, State, Out)
	),
	tw_bnodes(Pairs, State, Out).

pair_var(BNode, BNode-_).


%%	tw_prefix_map(+PrefixMap, +State, +Out) is det.
%
%	Write the @base and @prefix declarations

tw_prefix_map(PrefixMap, State, Out) :-
	tw_state_align_prefixes(State, true), !,
	longest_prefix(PrefixMap, 0, Length),
	PrefixCol is Length+10,
	tw_base(PrefixCol, State, Out),
	tw_prefix_map(PrefixMap, PrefixCol, State, Out).
tw_prefix_map(PrefixMap, State, Out) :-
	tw_base(0, State, Out),
	tw_prefix_map(PrefixMap, 0, State, Out).

longest_prefix([], L, L).
longest_prefix([Prefix-_|T], L0, L) :-
	atom_length(Prefix, L1),
	L2 is max(L0, L1),
	longest_prefix(T, L2, L).


tw_base(Col, State, Out) :-
	tw_state_base(State, Base),
	atom(Base), !,
	format(Out, '@base ~t~*|', [Col]),
	turtle_write_uri(Out, Base),
	format(Out, ' .~n', []).
tw_base(_, _, _).


tw_prefix_map([], _, _, _).
tw_prefix_map([Prefix-URI|T], Col, State, Out) :-
	format(Out, '@prefix ~t~w: ~*|', [Prefix, Col]),
	tw_relative_uri(URI, State, Out),
	format(Out, ' .~n', []),
	(   T == []
	->  true
	;   tw_prefix_map(T, Col, State, Out)
	).


%%	tw_proper_subjects(+Subjects, +State, +Out) is det.
%
%	Write the subjects that are not Bnodes.

tw_proper_subjects([], _, _).
tw_proper_subjects([H|T], State, Out) :-
	separate_subjects(State, Out),
	tw_subject(H, H, State, Out),
	tw_proper_subjects(T, State, Out).


separate_subjects(State, Out) :-
	tw_state_subject_white_lines(State, ExtraLines),
	put_n(ExtraLines, '\n', Out).

%%	tw_subject(+URI, +State, +Out) is det.
%
%	Write a toplevel non-bnode subject.

tw_subject(URI, Ref, State, Out) :-
	subject_triples(URI, State, Pairs),
	length(Pairs, Count),
	inc_triple_count(State, Count),
	group_po(Pairs, Grouped),
	tw_subject_triples(Grouped, Ref, State, Out).

group_po(Pairs, Grouped) :-
	group_pairs_by_key(Pairs, Grouped0),
	rdf_equal(rdf:type, RDFType),
	(   select(RDFType-Types, Grouped0, Grouped1)
	->  Grouped = [RDFType-Types|Grouped1]
	;   Grouped = Grouped0
	).

%%	tw_bnodes(+Pairs, +State, +Out) is det.
%
%	Write the Bnodes. Pairs is a list   URI-Ref, where Ref is one of
%	=written= if the Bnode is already written;   an integer if it is
%	used multiple times or a variable if   it  has not been written.
%	The order in which we deal with bnodes is defined as follows:
%
%	    * First, write the bnodes that are not referenced at all
%	    as toplevel bnodes using [ ... ] notation.
%
%	    * Next, write the bnodes that need written as toplevel
%	    nodes using the _:XX notation because they are referenced
%	    multiple times in the graph. Continue this process until it
%	    is exhausted.

tw_bnodes(Pairs, State, Out) :-
	tw_top_bnodes(Pairs, State, Out, Rest1),
	tw_numbered_bnodes(Rest1, State, Out, 1, Rest2),
	tw_cyclic_bnodes(Rest2, State, Out, 0).


tw_numbered_bnodes([], _, _, _, []) :- !.
tw_numbered_bnodes(Pairs, State, Out, Level, Rest) :-
	multi_referenced(Pairs, RefPairs, Rest0),
	(   RefPairs == []
	->  Rest = Rest0
	;   length(RefPairs, Count),
	    comment(State, 'Level ~D multi-referenced blank-nodes (~D)',
		    [ Level, Count ], Out),
	    tw_ref_bnodes(RefPairs, State, Out),
	    Level1 is Level + 1,
	    tw_numbered_bnodes(Rest0, State, Out, Level1, Rest)
	).

multi_referenced([], [], []).
multi_referenced([H|T], RefPairs, Rest) :-
	H = _-Ref,
	(   Ref == written
	->  multi_referenced(T, RefPairs, Rest)
	;   var(Ref)
	->  Rest = [H|TR],
	    multi_referenced(T, RefPairs, TR)
	;   assertion(Ref = bnode(_)),
	    RefPairs = [H|TRP],		% assigned reference
	    multi_referenced(T, TRP, Rest)
	).

tw_ref_bnodes([], _, _).
tw_ref_bnodes([BNode-Ref|T], State, Out) :-
	separate_subjects(State, Out),
	tw_subject(BNode, Ref, State, Out),
	tw_ref_bnodes(T, State, Out).


%%	tw_top_bnodes(+Pairs, +State, +Out, -Rest)
%
%	Write the top bnodes: those that  do   not  appear  as an object
%	anywhere.

tw_top_bnodes(Pairs, State, Out, Rest) :-
	unreferenced(Pairs, State, TopBNodes, Rest),
	(   TopBNodes == []
	->  true
	;   length(TopBNodes, Count),
	    comment(State, 'Toplevel blank-nodes (~D)', [Count], Out),
	    sort_bnodes(TopBNodes, SortedTopBNodes, State),
	    tw_top_bnodes(SortedTopBNodes, State, Out)
	).

unreferenced([], _, [], []).
unreferenced([H|T], State, UnrefPairs, Rest) :-
	H = BNode-Ref,
	(   Ref == written
	->  unreferenced(T, State, UnrefPairs, Rest)
	;   var(Ref),
	    object_link_count(BNode, State, 0)
	->  UnrefPairs = [H|URT],
	    unreferenced(T, State, URT, Rest)
	;   Rest = [H|TR],
	    unreferenced(T, State, UnrefPairs, TR)
	).

tw_top_bnodes([], _, _).
tw_top_bnodes([BNode-_|T], State, Out) :-
	tw_bnode(BNode, State, Out),
	tw_top_bnodes(T, State, Out).


tw_bnode(BNode, State, Out) :-
	subject_triples(BNode, State, Pairs),
	tw_bnode_triples(Pairs, State, Out),
	format(Out, ' .~n', []).

tw_bnode_triples(Pairs, State, Out) :-
	length(Pairs, Count),
	inc_triple_count(State, Count),
	group_po(Pairs, Grouped),
	(   tw_state_single_line_bnodes(State, true)
	->  format(Out, '[ ', []),
	    tw_triples(Grouped, -1, State, Out),
	    format(Out, ' ]', [])
	;   line_position(Out, Indent),
	    format(Out, '[ ', []),
	    line_position(Out, AIndent),
	    tw_triples(Grouped, AIndent, State, Out),
	    nl_indent(Out, State, Indent),
	    format(Out, ']', [])
	).

%%	tw_cyclic_bnodes(+Pairs, +BNode, +State, +Out, +Cycle)
%
%	The rest. These are groups of bnodes  that are reachable, but we
%	cannot find a starting point, neither from a named resource, nor
%	from an unlinked bnode. As long as we are not considering stable
%	canonical output, we can break the cycle at any point.

tw_cyclic_bnodes([], _State, _Out, _) :- !.
tw_cyclic_bnodes(Pairs, State, Out, Cycle0) :-
	(   tw_state_canonical(State, true)
	->  sort_bnode_pairs(Pairs, BNodes, State)
	;   BNodes = Pairs
	),
	succ(Cycle0, Cycle),
	BNodes = [BNode-Ref|_],
	next_bnode_id(State, BNode, Ref),
	comment(State, 'Breaking cycle ~D', [Cycle], Out),
	tw_numbered_bnodes(Pairs, State, Out, 1, Rest),
	tw_cyclic_bnodes(Rest, State, Out, Cycle).


%%	tw_subject_triples(+Grouped, +Subject, +State, +Out)
%
%	Save triples on Subject.  Combine groups of triples with the
%	same subject (;) and same subject+predicate (,).
%
%	@param	Subject is either a URI or an integer.  The latter is
%		used for writing a named bnode.

tw_subject_triples([], _, _, _) :- !.
tw_subject_triples(Grouped, URI, State, Out) :-
	tw_state_group(State, false), !,
	tw_ungrouped_triples(Grouped, URI, State, Out).
tw_subject_triples(Grouped, URI, State, Out) :-
	tw_resource(URI, State, Out),
	(   tw_state_indent(State, Indent),
	    Indent > 0
	->  nl_indent(Out, State, Indent)
	;   put_char(Out, ' '),
	    line_position(Out, Indent)
	),
	tw_triples(Grouped, Indent, State, Out),
	format(Out, ' .~n', []).

%%	tw_ungrouped_triples(+Grouped, +URI, +State, +Out)
%
%	Write triples for subject URI as one line per triple.  Used
%	for canonical output.

tw_ungrouped_triples([], _, _, _).
tw_ungrouped_triples([P-Vs|Groups], URI, State, Out) :-
	partition(rdf_is_bnode, Vs, BNVs, ProperVs),
	tw_ungrouped_values(ProperVs, P, URI, State, Out),
	sort_bnodes(BNVs, SortedBNVs, State),
	tw_ungrouped_values(SortedBNVs, P, URI, State, Out),
	tw_ungrouped_triples(Groups, URI, State, Out).

tw_ungrouped_values([], _, _, _, _).
tw_ungrouped_values([V|T], P, URI, State, Out) :-
	tw_resource(URI, State, Out),
	put_char(Out, ' '),
	tw_predicate(P, State, Out),
	put_char(Out, ' '),
	tw_object(V, State, Out),
	format(Out, ' .~n', []),
	tw_ungrouped_values(T, P, URI, State, Out).


%%	tw_triples(+Groups, +Indent, +State, +Out) is det.
%
%	Triple writer that uses ; and ,- grouping

tw_triples([P-Vs|MoreGroups], Indent, State, Out) :-
	tw_write_pvs(Vs, P, State, Out),
	(   MoreGroups == []
	->  true
	;   format(Out, ' ;', []),
	    nl_indent(Out, State, Indent),
	    tw_triples(MoreGroups, Indent, State, Out)
	).

tw_write_pvs(Values, P, State, Out) :-
	tw_predicate(P, State, Out),
	put_char(Out, ' '),
	line_position(Out, Indent),
	tw_write_vs(Values, Indent, State, Out).

tw_predicate(P, State, Out) :-
	(   rdf_equal(P, rdf:type)
	->  format(Out, 'a', [])
	;   tw_resource(P, State, Out)
	).

tw_write_vs([H|T], Indent, State, Out) :-
	tw_object(H, State, Out),
	(   T == []
	->  true
	;   format(Out, ' ,', []),
	    nl_indent(Out, State, Indent),
	    tw_write_vs(T, Indent, State, Out)
	).

%%	tw_object(+Value, +State, +Out) is det.
%
%	Write the object of a triple.

tw_object(Value, State, Out) :-
	rdf_is_bnode(Value), !,
	tw_bnode_object(Value, State, Out).
tw_object(Value, State, Out) :-
	atom(Value), !,
	tw_resource(Value, State, Out).
tw_object(Literal, State, Out) :-
	tw_literal(Literal, State, Out).

%%	tw_bnode_object(+Value, +State, +Out) is det.
%
%	Write a Bnode value.  There are a number of cases:
%
%	    * The BNode was already written.  Write the same ref.
%	    * The BNode is not shared.  Inline and set =written=
%	    * The BNode is shared.  Generate a NodeID and store it
%	    * The BNode is once as object: Generate a NodeID
%	    * The BNode is more than once object: Generate a NodeID
%	      and put in table.

tw_bnode_object(BNode, State, Out) :-
	tw_state_nodeid_map(State, BNTree),
	rb_lookup(BNode, Ref, BNTree), !,
	(   var(Ref)
	->  (   tw_unshared_bnode(BNode, State, Out)
	    ->	Ref = written
	    ;	next_bnode_id(State, BNode, Ref),
		tw_bnode_ref(Ref, Out)
	    )
	;   tw_bnode_ref(Ref, Out)
	).
tw_bnode_object(BNode, State, Out) :-
	object_link_count(BNode, State, N),
	N > 1, !,
	tw_state_nodeid_map(State, BNTree0),
	rb_insert(BNTree0, BNode, Ref, BNTree),
	set_nodeid_map_of_tw_state(BNTree, State),
	next_bnode_id(State, BNode, Ref),
	tw_bnode_ref(Ref, Out).
tw_bnode_object(BNode, State, Out) :-
	next_bnode_id(State, BNode, Ref),
	tw_bnode_ref(Ref, Out).

tw_bnode_ref(bnode(Ref), Out) :-
	(   integer(Ref)
	->  format(Out, '_:bn~w', [Ref])
	;   format(Out, '_:~w', [Ref])
	).

%%	tw_unshared_bnode(+BNode, +State, +Out) is semidet.
%
%	Write a bnode if this is the only place it is used.

tw_unshared_bnode(BNode, State, Out) :-
	object_link_count(BNode, State, 1),
	subject_triples(BNode, State, Pairs),
	(   Pairs == []
	->  format(Out, '[]', [])
	;   pairs_unshared_collection(Pairs, State, Collection)
	->  (   Collection == []
	    ->	format(Out, '()', [])
	    ;	tw_state_nodeid_map(State, BNTree),
	        rb_lookup(BNode, written, BNTree),
		length(Collection, NMembers),
		Triples is 2*NMembers,
		inc_triple_count(State, Triples),
		(   tw_state_single_line_bnodes(State, true)
		->  format(Out, '( ', []),
		    tw_collection(Collection, -1, State, Out),
		    format(Out, ' )', [])
		;   line_position(Out, Indent),
		    format(Out, '( ', []),
		    line_position(Out, AIndent),
		    tw_collection(Collection, AIndent, State, Out),
		    nl_indent(Out, State, Indent),
		    format(Out, ')', [])
		)
	    )
	;   tw_bnode_triples(Pairs, State, Out)
	).

tw_collection([H|T], Indent, State, Out) :-
	tw_object(H, State, Out),
	(   T \== []
	->  nl_indent(Out, State, Indent),
	    tw_collection(T, Indent, State, Out)
	;   true
	).

%%	unshared_collection(+URI, +State, -Members) is semidet.
%
%	True if URI denodes an RDF list that  is made up from bnodes, is
%	linked exactly once  to  its  context   and  contains  no  extra
%	triples.

unshared_collection(C, _, []) :-
	rdf_equal(C, rdf:nil), !.
unshared_collection(C, State, List) :-
	rdf_is_bnode(C),
	object_link_count(C, State, 1),
	tw_state_nodeid_map(State, BNTree),
	rb_lookup(C, written, BNTree),
	subject_triples(C, State, Pairs),
	pairs_unshared_collection(Pairs, State, List).

pairs_unshared_collection(Pairs, State, [H|T]) :-
	rdf_equal(rdf:first, RDFFirst),
	rdf_equal(rdf:rest, RDFRest),
	Pairs = [ RDFFirst-H,
		  RDFRest-Rest
		| More
		],
	(   More == []
	;   rdf_equal(rdf:type, RDFType),
	    rdf_equal(rdf:'List', RDFList),
	    More == [RDFType-RDFList]
	),
	unshared_collection(Rest, State, T).


%%	object_link_count(+BNode, +STate, -Count) is det.
%
%	Number of times BNode is used as an object in the graph

object_link_count(BNode, State, Count) :-
	tw_state_graph(State, Graph),
	tw_state_expand(State, Expand),
	findall(S-P, call(Expand,S,P,BNode,Graph), Pairs0),
	sort(Pairs0, Pairs),		% remove duplicates
	length(Pairs, Count).

%%	nl_indent(+Out, +State, +Indent) is det.
%
%	Write a newline and indent to column Indent.

nl_indent(Out, _, -1) :- !,
	put_char(Out, ' ').
nl_indent(Out, State, Indent) :-
	nl(Out),
	tw_state_tab_distance(State, TD),
	(   TD == 0
	->  tab(Out, Indent)
	;   Tabs is Indent//TD,
	    Spaces is Indent mod TD,
	    put_n(Tabs, '\t', Out),
	    put_n(Spaces, ' ', Out)
	).

put_n(N, Char, Out) :-
	N > 0, !,
	put_char(Out, Char),
	N2 is N - 1,
	put_n(N2, Char, Out).
put_n(_, _, _).


%%	subject_triples(+URI, +State, -Pairs) is det.
%
%	Pairs is a sorted list of P-O  pairs representing all triples on
%	the subject URI.

subject_triples(URI, State, Pairs) :-
	tw_state_graph(State, Graph),
	tw_state_expand(State, Expand),
	findall(P-O, call(Expand, URI, P, O, Graph), Pairs0),
	sort(Pairs0, Pairs).


		 /*******************************
		 *	    GRAPH-LOGIC		*
		 *******************************/

%%	subjects(+State, -Subjects:ord_set) is det.
%
%	Subjects is a list of all subjects in the graph requested in
%	State.

subjects(State, Subjects) :-
	tw_state_graph(State, Graph),
	tw_state_expand(State, Expand),
	(   Expand == lookup,
	    atom(Graph),
	    rdf_statistics(triples_by_file(Graph, Count)),
	    rdf_statistics(triples(Total)),
	    Count * 10 < Total
	->  findall(S, rdf(S,_,_,Graph), List),
	    sort(List, Subjects)
	;   Expand \== lookup
	->  findall(S, call(Expand, S,_,_,Graph), List),
	    sort(List, Subjects)
	;   findall(Subject, subject(State, Subject), AllSubjects),
	    sort(AllSubjects, Subjects)
	).


subject(State, Subject) :-
	tw_state_graph(State, Graph),
	(   atom(Graph)
	->  rdf_subject(Subject),
	    (   rdf(Subject, _, _, Graph)
		->  true
	    )
	;   rdf_subject(Subject)
	).


lookup(S,P,O,G) :-
	(   var(G)
	->  rdf(S,P,O)
	;   rdf(S,P,O,G)
	).


		 /*******************************
		 *	  CANONICAL ORDERING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This section deals with the two problems of canonical graphs:

    * Keep blank nodes in the same order
    * Assign stable names to blank nodes that we need to
      give a name.  There are two cases: (1) a blank nodes is
      used in more than one place and (2) a blank node series
      is cyclic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	sort_bnodes(+BNodes, -Sorted, +State) is det.
%
%	Sort a list of blank nodes.

sort_bnodes(BNodes, Sorted, _State) :-
	sort(BNodes, Sorted).

%%	sort_bnode_pairs(+Pairs, -Sorted, +State) is det.
%
%	Sort a list of Pairs BNode-Ref

sort_bnode_pairs(Pairs, Sorted, _State) :-
	sort(Pairs, Sorted).

%%	bnode_to_term(+BNode, -Term, +State)
%
%	Term is a canonical representation of the graph formed by BNode.
%	The transformation of a bnode is
%
%		bnode(p-[o1,o2,..], ..)
%
%	The arguments are alphabetically sorted   on predicate (can't we
%	leave the preds out them?) and   the  objects are alphabetically
%	sorted.  Sorting multiple bnode values?


%%	next_bnode_id(+State, +BNode, -Ref) is det.
%
%	Generate a node-id for BNode.   When writing non-canonically, we
%	simply number the bnodes.  Otherwise  we   want  a  more  stable
%	numbering. Our numbering is a hash of  the content of the bnode.
%	It is not unlikely that we find muliple copies, and therefore we
%	number the full id is bn_<hash>_<n>, <n> counting 0...

next_bnode_id(State, _BNode, bnode(Ref)) :-
	tw_state_canonical(State, false), !,
	tw_state_bnode_id(State, Ref0),
	Ref is Ref0+1,
	nb_set_bnode_id_of_tw_state(Ref, State).
next_bnode_id(State, BNode, bnode(Ref)) :-
	bnode_hash(BNode, Hash),
	tw_state_bnode_hash(State, BNHash),
	(   var(BNHash)
	->  rb_empty(BNHash)
	;   true
	),
	(   rb_update(BNHash, Hash, C0, C, BNHash1)
	->  C is C0+1
	;   C = 0,
	    rb_insert(BNHash, Hash, C, BNHash1)
	),
	set_bnode_hash_of_tw_state(BNHash1, State),
	format(atom(Ref), 'bn_~w_~d', [Hash, C]).

%%	bnode_hash(+BNode, -Hash) is det.
%
%	Hash is the hash-value for a bnode.
%
%	@tbd: Hash on content.

bnode_hash(BNode, Hash) :-
	term_hash(BNode, Hash).


		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

%%	tw_resource(+Resource, +State, +Out) is det.
%
%	Write a resource

tw_resource(BNodeID, _, Out) :-
	BNodeID = bnode(_), !,
	tw_bnode_ref(BNodeID, Out).
tw_resource(Resource, State, Out) :-
	tw_state_prefix_map(State, PrefixMap),
	member(Prefix-Full, PrefixMap),
	atom_concat(Full, Name, Resource),
	turtle_name(Name), !,
	format(Out, '~w:~w', [Prefix, Name]).
tw_resource(Resource, State, Out) :-
	tw_relative_uri(Resource, State, Out).


tw_relative_uri(Resource, State, Out) :-
	tw_state_base_root(State, Root),
	atom(Root),
	atom_concat(Root, ResPath, Resource),
	sub_atom(ResPath, 0, _, _, /),
	tw_state_base_path(State, BasePath),
	relative_path(ResPath, BasePath, RelPath), !,
	turtle_write_uri(Out, RelPath).
tw_relative_uri(Resource, _, Out) :-
	turtle_write_uri(Out, Resource).

relative_path(Path, RelTo, RelPath) :-
	atomic_list_concat(PL, /, Path),
	atomic_list_concat(RL, /, RelTo),
	delete_common_prefix(PL, RL, PL1, PL2),
	to_dot_dot(PL2, DotDot, PL1),
	atomic_list_concat(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
	delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
	to_dot_dot(T0, T, Tail).


%%	tw_literal(+Literal, +State, +Out) is det.
%
%	Write a literal value to the stream Out.

tw_literal(literal(type(Type, Value)), State, Out) :- !,
	tw_typed_literal(Type, Value, State, Out).
tw_literal(literal(lang(Lang, Value)), State, Out) :- !,
	tw_quoted_string(Value, State, Out),
	downcase_atom(Lang, TurtleLang), 	% Turtle lang = [a-z]+('-'[a-z0-9]+)*
	format(Out, '@~w', [TurtleLang]).
tw_literal(literal(Value), State, Out) :-
	atom(Value), !,
	tw_quoted_string(Value, State, Out).
tw_literal(literal(Value), State, Out) :-
	atom(Value), !,
	tw_quoted_string(Value, State, Out).
						% Add types automatically
tw_literal(literal(Value), State, Out) :-
	integer(Value), !,
	rdf_equal(Type, xsd:integer),
	tw_typed_literal(Type, Value, State, Out).
tw_literal(literal(Value), State, Out) :-
	float(Value), !,
	rdf_equal(Type, xsd:double),
	tw_typed_literal(Type, Value, State, Out).
tw_literal(literal(Value), State, Out) :-
	xml_is_dom(Value), !,
	rdf_equal(Type, rdf:'XMLLiteral'),
	tw_typed_literal(Type, Value, State, Out).
tw_literal(Literal, _State, _Out) :-
	type_error(rdf_literal, Literal).


tw_typed_literal(Type, Value, State, Out) :-
	tw_abbreviated_literal(Type, Value, State, Out), !.
tw_typed_literal(Type, Value, State, Out) :-
	(atom(Value) ; string(Value)), !,
	tw_quoted_string(Value, State, Out),
	write(Out, '^^'),
	tw_resource(Type, State, Out).
tw_typed_literal(Type, Value, State, Out) :-
	rdf_equal(Type, rdf:'XMLLiteral'), !,
	with_output_to(string(Tmp),
		       xml_write(Value, [header(false)])),
	tw_quoted_string(Tmp, State, Out),
	write(Out, '^^'),
	tw_resource(Type, State, Out).
tw_typed_literal(Type, Value, State, Out) :-
	format(string(Tmp), '~q', [Value]),
	tw_quoted_string(Tmp, State, Out),
	write(Out, '^^'),
	tw_resource(Type, State, Out).


%%	tw_abbreviated_literal(+Type, +Value, +State, +Out) is semidet.
%
%	Turtle abbreviated typed literals.
%
%	@tbd:   Deal with canonical forms (or is this a task of the
%		RDF parser?
%	@tbd:	What if the value is not in the lexical space of the type?

term_expansion((tw_abbreviated_literal(NS:Local, Value, State, Out) :- Body),
	       (tw_abbreviated_literal(Type, Value, State, Out) :- Body)) :-
	atom(NS),
	rdf_global_id(NS:Local, Type).

tw_abbreviated_literal(xsd:integer, Value, State, Out) :-
	(   tw_state_canonize_numbers(State, false)
	->  write(Out, Value)
	;   atom_number(Value, Int),
	    format(Out, '~d', [Int])
	).
tw_abbreviated_literal(xsd:double, Value, State, Out) :-
	(   tw_state_canonize_numbers(State, false)
	->  write(Out, Value)
	;   atom_number(Value, Float),
	    format(Out, '~f', [Float])
	).
tw_abbreviated_literal(xsd:decimal, Value, _, Out) :-
	format(Out, '~w', [Value]).
tw_abbreviated_literal(xsd:boolean, Value, _, Out) :-
	format(Out, '~w', [Value]).


%%	tw_quoted_string(+Atom, +State, +Out) is det.
%
%	Write  Atom  to  Out  as  a  quoted  string.  We  only  use  the
%	single-"..." representation.

tw_quoted_string(Atom, _, Out) :-
	turtle_write_quoted_string(Out, Atom).


		 /*******************************
		 *	       COMMENT		*
		 *******************************/

comment(State, Format, Args, Out) :-
	tw_state_comment(State, true), !,
	format(Out, '~n# ', []),
	format(Out, Format, Args),
	format(Out, '~n', []).
comment(_, _, _, _).



		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

inc_triple_count(State, Count) :-
	tw_state_triple_count(State, C0),
	C1 is C0+Count,
	nb_set_triple_count_of_tw_state(C1, State).

:- multifile
	prolog:message//1.

prolog:message(rdf(saved(File, Time, SavedSubjects, SavedTriples))) -->
	[ 'Saved ~D triples about ~D subjects into ~p (~3f sec)'-
	  [SavedTriples, SavedSubjects, File, Time]
	].
