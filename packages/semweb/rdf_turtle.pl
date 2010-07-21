/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2009, University of Amsterdam

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

:- module(rdf_turtle,
	  [ rdf_load_turtle/3,		% +Input, -Triples, +Options
	    rdf_read_turtle/3,		% +Input, -Triples, +Options
	    rdf_process_turtle/3	% +Input, :OnObject, +Options
	  ]).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(debug)).
:- use_module(library(uri)).
:- use_module(library(record)).
:- use_module(library(http/http_open)).
:- use_module(turtle_base).

:- meta_predicate
	rdf_process_turtle(+,2,+).

/** <module> Turtle: Terse RDF Triple Language

This module implements the Turtle  language   for  representing  the RDF
triple model as defined by Dave Beckett  from the Institute for Learning
and Research Technology University of Bristol in the document:

  * http://www.w3.org/TeamSubmission/turtle/
  * http://www.w3.org/TeamSubmission/2008/SUBM-turtle-20080114/#sec-conformance

This parser passes all tests,  except   for  test-28.ttl  (decial number
serialization) and test-29.ttl (uri containing  ...%&...). It is unclear
to me whether these tests are correct. Notably, it is unclear whether we
must do %-decoding. Certainly, this  is   expected  by various real-life
datasets that we came accross with.

This module acts as a plugin to   rdf_load/2,  for processing files with
one of the extensions =|.ttl|=, =|.n3|= or =|.nt|=.

@tbd Better error handling
*/

:- record ttl_state(base_uri,
		    resources:oneof([uri,iri])=uri,
		    prefix_map,
		    nodeid_map,
		    anon_prefix,
		    anon_count=0,
		    graph,
		    input,
		    line_no=0,
		    on_error:oneof([warning,error])=warning,
		    error_count=0).

%%	rdf_read_turtle(+Input, -Triples, +Options)
%
%	Read a stream or file into a set of triples of the format
%
%		rdf(Subject, Predicate, Object)
%
%	The representation is consistent with the SWI-Prolog RDF/XML
%	and ntriples parsers.  Provided options are:
%
%		* base_uri(+BaseURI)
%		Initial base URI.  Defaults to file://<file> for loading
%		files.
%
%		* anon_prefix(+Prefix)
%		Blank nodes are generated as <Prefix>1, <Prefix>2, etc.
%		If Prefix is not an atom blank nodes are generated as
%		node(1), node(2), ...
%
%		* resources(URIorIRI)
%		Officially, Turtle resources are IRIs.  Quite a
%		few applications however send URIs.  By default we
%		do URI->IRI mapping because this rarely causes errors.
%		To force strictly conforming mode, pass =iri=.
%
%		* prefixes(-Pairs)
%		Return encountered prefix declarations as a
%		list of Alias-URI
%
%		* namespaces(-Pairs)
%		Same as prefixes(Pairs).  Compatibility to rdf_load/2.
%
%		* base_used(-Base)
%		Base URI used for processing the data.  Unified to
%		[] if there is no base-uri.
%
%		* on_error(+ErrorMode)
%		In =warning= (default), print the error and continue
%		parsing the remainder of the file.  If =error=, abort
%		with an exception on the first error encountered.
%
%		* error_count(-Count)
%		If on_error(warning) is active, this option cane be
%		used to retrieve the number of generated errors.

rdf_read_turtle(In, Triples, Options) :-
	open_input(In, Stream, Close),
	init_state(In, Stream, Options, State),
	call_cleanup(phrase(turtle_file(State, Stream), Triples),
		     Close),
	post_options(State, Options).


%%	rdf_load_turtle(+Input, -Triples, +Options)
%
%	@deprecated Use rdf_read_turtle/3

rdf_load_turtle(Input, Triples, Options) :-
	rdf_read_turtle(Input, Triples, Options).


%%	rdf_process_turtle(+Input, :OnObject, +Options) is det.
%
%	Process Turtle input from Input, calling OnObject with a list of
%	triples. Options is the same as for rdf_load_turtle/3.
%
%	Errors encountered are sent to  print_message/2, after which the
%	parser tries to recover and parse the remainder of the data.

rdf_process_turtle(In, OnObject, Options) :-
	open_input(In, Stream, Close),
	init_state(In, Stream, Options, State),
	call_cleanup(process_stream(State, Stream, OnObject),
		     Close),
	post_options(State, Options).

post_options(State, Options) :-
	prefix_option(State, Options),
	namespace_option(State, Options),
	base_option(State, Options),
	error_option(State, Options).

prefix_option(State, Options) :-
	(   option(prefixes(Pairs), Options)
	->  ttl_state_prefix_map(State, Map),
	    assoc_to_list(Map, Pairs)
	;   true
	).
namespace_option(State, Options) :-
	(   option(namespaces(Pairs), Options)
	->  ttl_state_prefix_map(State, Map),
	    assoc_to_list(Map, Pairs)
	;   true
	).
base_option(State, Options) :-
	(   option(base_used(Base), Options)
	->  ttl_state_base_uri(State, Base)
	;   true
	).
error_option(State, Options) :-
	(   option(error_count(Count), Options)
	->  ttl_state_error_count(State, Count)
	;   true
	).


process_stream(State, In, OnObject) :-
	read_turtle_tokens(In, Tokens, State),
	debug(turtle, 'Tokens: ~w~n', [Tokens]),
	ttl_state_line_no(State, LineNo),
	(   Tokens == end_of_file
	->  true
	;   catch(phrase(triples(State, Triples), Tokens), E, true)
	->  (   var(E)
	    ->  (   Triples == []
		->  true
		;   ttl_state_graph(State, DB),
		    call(OnObject, Triples, DB:LineNo)
		)
	    ;   print_message(error, E)
	    ),
	    process_stream(State, In, OnObject)
	;   syntax_error_term(In, LineNo, cannot_parse, Error),
	    step_error(State, Error),
	    process_stream(State, In, OnObject)
	).


%%	step_error(+State, +Error) is det.
%
%	Throw Error of =on_error= is =error=.  Otherwise print the error
%	and increment =error_count=.
%
%	@error syntax_error(Culprit).

step_error(State, Error) :-
	ttl_state_on_error(State, error), !,
	throw(Error).
step_error(State, Error) :-
	ttl_state_error_count(State, E0),
	succ(E0, E),
	nb_set_error_count_of_ttl_state(E, State),
	print_message(error, Error).


%%	open_input(+Input, -Stream, -Close) is det.
%
%	Open given input.
%
%	@param  Close goal to undo the open action
%	@tbd	Synchronize with input handling of rdf_db.pl.
%	@error	existence_error, permission_error

open_input(stream(Stream), Stream, true) :- !,
	stream_property(Stream, encoding(Old)),
	(   Old == utf8
	->  Close = true
	;   set_stream(Stream, encoding(utf8)),
	    Close = set_stream(Stream, encoding(Old))
	).
open_input(Stream, Stream, Close) :-
	is_stream(Stream), !,
	open_input(stream(Stream), Stream, Close).
open_input(URL, Stream, close(Stream)) :-
	sub_atom(URL, 0, _, _, 'http://'), !,
	http_open(URL, Stream, []),
	set_stream(Stream, encoding(utf8)).
open_input(File, Stream, close(Stream)) :-
	absolute_file_name(File, Path,
			   [ access(read),
			     extensions([ttl, ''])
			   ]),
	open(Path, read, Stream, [encoding(utf8)]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The parser is a two-stage processor. The  first reads the raw file input
and generates a list of tokens, stripping   comments and white space. It
is defined to read a single  statement   upto  its  terminating '.'. The
second stage is a traditional DCG parser  generating the triples for the
statement.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

init_state(In, Stream, Options, State) :-
	(   option(base_uri(BaseURI), Options)
	->  true
	;   In = stream(_)
	->  BaseURI = []
	;   uri_is_global(In),
	    \+ is_absolute_file_name(In) 	% Avoid C:Path in Windows
	->  uri_normalized(In, BaseURI)
	;   uri_file_name(BaseURI, In)
	),
	(   option(anon_prefix(Prefix), Options)
	->  true
	;   BaseURI == []
	->  Prefix = '__bnode'
	;   atom_concat('__', BaseURI, Prefix)
	),
	option(db(DB), Options, BaseURI),
	option(on_error(OnError), Options, warning),
	option(resources(URIIRI), Options, uri),
	empty_assoc(Map),
	empty_assoc(NodeMap),
	make_ttl_state([ base_uri(BaseURI),
			 resources(URIIRI),
			 prefix_map(Map),
			 nodeid_map(NodeMap),
			 anon_prefix(Prefix),
			 graph(DB),
			 input(Stream),
			 on_error(OnError)
		       ], State).


turtle_file(State, In) -->
	{ read_turtle_tokens(In, Tokens, State),
	  debug(turtle, 'Tokens: ~w~n', [Tokens])
	},
	(   { Tokens == end_of_file }
	->  []
	;   { catch(phrase(triples(State, Triples), Tokens), E, true) }
	->  (   { var(E) }
	    ->	list(Triples),
		turtle_file(State, In)
	    ;	{ step_error(State, E) },
		turtle_file(State, In)
	    )
	;   { ttl_state_line_no(State, LineNo),
	      syntax_error_term(In, LineNo, cannot_parse, Error),
	      step_error(State, Error)
	    },
	    turtle_file(State, In)
	).

list([]) --> [].
list([H|T]) --> [H], list(T).

triples(State, []) -->
	[ '@', name(prefix), name(Prefix), : ], !,
	iri(State, URI),
	{ ttl_state_prefix_map(State, Map0),
	  put_assoc(Prefix, Map0, URI, Map),
	  set_prefix_map_of_ttl_state(Map, State)
	}.
triples(State, []) -->
	[ '@', name(prefix), ':' ], !,
	iri(State, URI),
	{ set_base_uri_of_ttl_state(URI, State)
	}.
triples(State, []) -->
	[ '@', name(base) ], !,
	iri(State,URI),
	{ set_base_uri_of_ttl_state(URI, State)
	}.
triples(State, Triples) -->
	subject(State, Subject, Triples, T),
	(   predicate_object_list(State, Subject, T, [])
	->  (   eos
	    ->	[]
	    ;	syntax_rule(State, expected(predicate_object_list))
	    )
	;   { Triples \== T }		% [ p o ; ... ] .
	->  { T = [] }
	).

eos([], []).

subject(State, Subject, T, T) -->
	resource(State, Subject), !.
subject(State, Subject, T0, T) -->
	blank(State, Subject, T0, T), !.
subject(State, _, _, _) -->
	syntax_rule(State, subject_expected).

predicate_object_list(State, Subject, Triples, Tail) -->
	verb(State, Predicate),
	object_list(State, Subject, Predicate, Triples, Tail0),
	(   [';']
	->  opt_predicate_object_list(State, Subject, Tail0, Tail)
	;   {Tail0 = Tail}
	).

opt_predicate_object_list(State, Subject, Triples, Tail) -->
	predicate_object_list(State, Subject, Triples, Tail), !.
opt_predicate_object_list(_, _, Tail, Tail) -->
	[].

object_list(State, Subject, Predicate,
	    [rdf(Subject, Predicate, Object)|T0], T) -->
	object(State, Object, T0, T1),
	(   [',']
	->  object_list(State, Subject, Predicate, T1, T)
	;   {T1 = T}
	).

verb(_, P) -->
	[name(a)], !,
	{ rdf_equal(rdf:type, P)
	}.
verb(State, P) -->
	resource(State, P).

object(State, Object, T, T) -->
	[ literal(Value) ], !,
	{ mk_object(Value, State, Object)
	}.
object(_, literal(type(Type, N)), T, T) -->
	[ numeric(Tp, Codes) ], !,
	{ numeric_url(Tp, Type),
	  normalise_number(Tp, Codes, N)
	}.
object(State, Object, T, T) -->
	resource(State, Object), !.
object(State, Object, T0, T) -->
	blank(State, Object, T0, T), !.
object(_, Object, T, T) -->
	[ name(Bool) ],
	{ boolean(Bool),
	  Object = literal(type(BoolType, Bool)),
	  rdf_equal(BoolType, xsd:boolean)
	}.
object(State, _, _, _) -->
	syntax_rule(State, expected_object).

%%	normalise_number(+Type, +Codes:list, -Literal:atom) is det.
%
%	Turtle normalisation of numbers. Currently  only implemented for
%	integers. This ensures that 0001 is parsed as "1"^^xsd:integer.
%
%	Hmmm.  Acording to test-10.ttl, this must *not* be done, so for
%	now we disable all normalization.

%normalise_number(integer, Codes, N) :-
%	number_codes(I, Codes),
%	atom_number(N, I).
normalise_number(_, Codes, N) :-
	atom_codes(N, Codes).

term_expansion(numeric_url(I, Local),
	       numeric_url(I, URI)) :-
	rdf_global_id(Local, URI).

numeric_url(integer, xsd:integer).
numeric_url(decimal, xsd:decimal).
numeric_url(double,  xsd:double).

boolean(true).
boolean(false).

resource(State, IRI) -->
	iri(State, IRI), !.
resource(State, IRI) -->
	[ :(Name) ], !,
	{ ttl_state_base_uri(State, Base),
	  atom_concat(Base, Name, URI),
	  uri_iri(State, URI, IRI)
	}.
resource(State, IRI) -->
	[ name(Prefix), : ], !,
	{ ttl_state_prefix_map(State, Map),
	  get_assoc(Prefix, Map, IRI)
	}.
resource(State, IRI) -->
	[ Prefix:Name ], !,
	{ ttl_state_prefix_map(State, Map),
	  (   get_assoc(Prefix, Map, Base)
	  ->  atom_concat(Base, Name, URI),
	      uri_iri(State, URI, IRI)
	  ;   throw(error(existence_error(prefix, Prefix), _))
	  )
	}.
resource(State, BaseIRI) -->
	[ : ], !,
	{ ttl_state_base_uri(State, BaseIRI)
	}.

uri_iri(State, URI, IRI) :-
	(   ttl_state_resources(State, uri)
	->  uri_iri(URI, IRI)
	;   IRI = URI
	).

iri(State, IRI) -->
	[ relative_uri(Rel)
	],
	{ ttl_state_base_uri(State, Base),
	  (   Rel == ''			% must be in global_url?
	  ->  IRI = Base
	  ;   uri_normalized_iri(Rel, Base, IRI)
	  )
	}.

blank(State, Resource, T, T) -->
	[ nodeId(NodeId) ], !,
	{ ttl_state_nodeid_map(State, IdMap),
	  (   get_assoc(NodeId, IdMap, Resource)
	  ->  true
	  ;   anonid(State, NodeId, Resource),
	      put_assoc(NodeId, IdMap, Resource, NewIdMap),
	      set_nodeid_map_of_ttl_state(NewIdMap, State)
	  )
	}.
blank(State, Resource, T, T) -->
	[ '[', ']' ], !,
	{ anonid(State, Resource)
	}.
blank(State, Resource, T0, T) -->
	[ '[' ], !,
	{ anonid(State, Resource)
	},
	predicate_object_list(State, Resource, T0, T),
	[ ']' ].
blank(State, Resource, T0, T) -->
	[ '(' ],
	item_list(State, Resource, T0, T).

item_list(_State, Resource, T, T) -->
	[ ')' ], !,
	{ rdf_equal(rdf:nil, Resource)
	}.
item_list(State, Resource, T0, T) -->
	{ anonid(State, Resource) },
	object(State, Object, T0, T1),
	{ rdf_equal(rdf:first, First),
	  rdf_equal(rdf:rest, Rest),
	  T1 = [ rdf(Resource, First, Object),
		 rdf(Resource, Rest, Tail)
	       | T2
	       ]
	},
	item_list(State, Tail, T2, T).


anonid(State, Node) :-
	ttl_state_anon_prefix(State, AnonPrefix),
	ttl_state_anon_count(State, C0),
	Count is C0 + 1,
	set_anon_count_of_ttl_state(Count, State),
	(   atom(AnonPrefix)
	->  atom_concat(AnonPrefix, Count, Node)
	;   Node = node(Count)
	).

anonid(State, _NodeId, Node) :-
	ttl_state_anon_prefix(State, AnonPrefix),
	atom(AnonPrefix), !,
	anonid(State, Node).
anonid(_State, NodeId, node(NodeId)).

mk_object(type(Prefix:Name, Value), State, literal(type(Type, Value))) :- !,
	  ttl_state_prefix_map(State, Map),
	  get_assoc(Prefix, Map, Base),
	  atom_concat(Base, Name, Type).
mk_object(type(relative_uri(Rel), Value), State, literal(type(Type, Value))) :- !,
	  ttl_state_base_uri(State, Base),
	  (   Rel == ''			% must be in global_url?
	  ->  Type = Base
	  ;   uri_normalized_iri(Rel, Base, Type)
	  ).
mk_object(type(:(Name), Value), State, literal(type(Type, Value))) :- !,
	  ttl_state_base_uri(State, Base),
	  atom_concat(Base, Name, Type).
mk_object(Value, _State, literal(Value)).

syntax_rule(State, Error) -->
	error_tokens(7, Tokens),
	{ ttl_state_input(State, Stream),
	  stream_property(Stream, file_name(File)),
	  ttl_state_line_no(State, LineNo),
	  atomic_list_concat(Tokens, ' ', Before),
	  format(string(Msg), '~w:~d (before "~w ...")',
		 [File, LineNo, Before]),
	  throw(error(syntax_error(Error),
		      context(_, Msg)))
	}.

%%	error_tokens(+Count, -Tokens) is det.
%
%	Return maximum Count tokens,  converted   back  to  turtle input
%	syntax.

error_tokens(N, [H|T]) -->
	{ succ(N2, N) },
	error_token(H), !,
	error_tokens(N2, T).
error_tokens(_, []) --> [].

error_token(Name) -->
	[ name(Name) ], !.
error_token(Text) -->
	[ numeric(_, Codes) ], !,
	{ atom_codes(Text, Codes) }.
error_token(Text) -->
	[ literal(Literal) ], !,
	{ literal_text(Literal, Text) }.
error_token(Text) -->
	[ URIToken ],
	{ uri_text(URIToken, Text) }, !.
error_token(Punct) -->
	[ Punct ],
	{ atom(Punct) }, !.
error_token(Rest) -->
	[ H ],
	{ term_to_atom(H, Rest) }.

literal_text(type(Type, Value), Text) :- !,
	uri_text(Type, TypeText),
	format(atom(Text), '"~w"^^~w', [Value, TypeText]).

uri_text(relative_uri(URI), Text) :-
	format(atom(Text), '<~w>', [URI]).
uri_text(:(Name), Text) :-
	format(atom(Text), ':~w', [Name]).


		 /*******************************
		 *	     TOKENISER		*
		 *******************************/

%%	read_turtle_tokens(+In, -List, +State) is det.
%
%	Read  the  next  Turtle  statement  as  a  list  of  tokens.  If
%	on_error(warning)  is  active,  failure  prints  a  message  and
%	continues reading the next statements.
%
%	The line_no property of the state is set to the start-line
%
%	@error syntax_error(Culprit)

read_turtle_tokens(In, List, State) :-
	ttl_state_on_error(State, error), !,
	line_count(In, LineNo),
	nb_set_line_no_of_ttl_state(LineNo, State),
	(   turtle_tokens(In, List)
	->  true
	;   syntax_error_term(In, LineNo, illegal_token, Error),
	    throw(Error)
	).
read_turtle_tokens(In, List, State) :-
	line_count(In, LineNo),
	nb_set_line_no_of_ttl_state(LineNo, State),
	(   catch(turtle_tokens(In, List), Error, true)
	->  (   var(Error)
	    ->  true
	    ;   print_message(error, Error),
		skip_statement(In),
		read_turtle_tokens(In, List, State)
	    )
	;   syntax_error_term(In, LineNo, illegal_token, Error),
	    print_message(error, Error),
	    skip_statement(In),
	    read_turtle_tokens(In, List, State)
	).

%%	skip_statement(+In)
%
%	Skip to the end of the statement

skip_statement(In) :-
	get_code(In, C0),
	skip_statement(C0, In).

skip_statement(-1, _) :- !.
skip_statement(0'., In) :-
	get_code(In, C),
	(   turtle_ws(C)
	->  !
	;   skip_statement(C, In)
	).
skip_statement(_, In) :-
	get_code(In, C),
	skip_statement(C, In).

%%	turtle_tokens(+In, -List)
%
%	Read a statement from a turtle file, returning the contents as a
%	list of tokens.

turtle_tokens(In, List) :-
	get_code(In, C0),
	turtle_token(C0, In, C1, Tok1),
	(   Tok1 == end_of_file
	->  List = end_of_file
	;   List = [Tok1|Tokens],
	    turtle_tokens(C1, In, Tokens)
	).

turtle_tokens(C0, In, List) :-
	(   turtle_token(C0, In, C1, H)
	->  debug(turtle(token), 'Token: ~q', [H])
	;   syntax_error(In, -1, illegal_token)
	),
	(   H == '.'
	->  List = []
	;   H == end_of_file
	->  syntax_error(In, -1, unexpected_end_of_input)
	;   List = [H|T],
	    turtle_tokens(C1, In, T)
	).

turtle_token(-1, _, -1, end_of_file) :- !.
turtle_token(0'., _, end, '.') :- !.	% Turtle does not demand a space here!
turtle_token(0'#, In, C, Token) :- !,
	get_code(In, C1),
	skip_line(C1, In, C2),
	turtle_token(C2, In, C, Token).
turtle_token(WS, In, C, Token) :-
	turtle_ws(WS), !,
	get_code(In, C1),
	turtle_token(C1, In, C, Token).
turtle_token(C0, In, C, Number) :-
	between(0'0, 0'9, C0), !,
	turtle_number(C0, In, C, Number).
turtle_token(0'-, In, C, Number) :- !,
	turtle_number(0'-, In, C, Number).
turtle_token(0'+, In, C, Number) :- !,
	turtle_number(0'+, In, C, Number).
turtle_token(0'", In, C, Literal) :- !,
	turtle_read_string(0'", In, C1, Atom),
	(   C1 == 0'@
	->  get_code(In, C2),
	    language(C2, In, C, LangCodes),
	    atom_codes(LangId, LangCodes),
	    Literal = literal(lang(LangId, Atom))
	;   C1 == 0'^,
	    peek_code(In, 0'^)
	->  get_code(In, 0'^),
	    get_code(In, C2),
	    resource_token(C2, In, C, Type),
	    Literal = literal(type(Type, Atom))
	;   C = C1,
	    Literal = literal(Atom)
	).
turtle_token(0'_, In, C, nodeId(NodeID)) :-
	peek_code(In, 0':), !,
	get_code(In, _),
	get_code(In, C1),
	turtle_read_name(C1, In, C, NodeID).
turtle_token(0'<, In, C, URI) :- !,
	resource_token(0'<, In, C, URI).
turtle_token(0':, In, C, URI) :- !,
	resource_token(0':, In, C, URI).
turtle_token(C0, In, C, Token) :-
	turtle_read_name(C0, In, C1, Name), !,
	(   C1 == 0':,
	    \+ sub_atom(Name, 0, _, _, '_'),
	    peek_code(In, C2),
	    turtle_name_start_char(C2)
	->  get_code(In, C2),
	    turtle_read_name(C2, In, C, Name2),
	    Token = (Name:Name2)
	;   Token = name(Name),
	    C = C1
	).
turtle_token(Punct, In, C, P) :-
	punctuation(Punct, P), !,
	get_code(In, C).

%%	turtle_number(+Char0, +In, -CharNext, -Value)
%
%	Value is Type:CodeList

turtle_number(0'-, In, CN, numeric(T, [0'-|Codes])) :- !,
	get_code(In, C0),
	turtle_number_nn(C0, In, CN, numeric(T, Codes)).
turtle_number(0'+, In, CN, numeric(T, [0'+|Codes])) :- !,
	get_code(In, C0),
	turtle_number_nn(C0, In, CN, numeric(T, Codes)).
turtle_number(C0, In, CN, Value) :-
	turtle_number_nn(C0, In, CN, Value).

turtle_number_nn(C, In, CN, numeric(Type, Codes)) :-
	turtle_integer_codes(C, In, CN0, Codes, T0), 	% [0-9]+
	(   CN0 == 0'.
	->  T0 = [CN0|T1],
	    get_code(In, C1),
	    turtle_integer_codes(C1, In, CN1, T1, T2), % [0-9]+.[0-9]+
	    (	exponent(CN1, In, CN, T2)
	    ->	Type = double
	    ;	CN = CN1,
		T2 = [],
		Type = decimal
	    )
	;   exponent(CN0, In, CN, T0)
	->  Type = double
	;   T0 = [],
	    CN = CN0,
	    Type = integer
	).

turtle_integer_codes(C0, In, CN, [C0|T0], T) :-
	between(0'0, 0'9, C0), !,
	get_code(In, C1),
	turtle_integer_codes(C1, In, CN, T0, T).
turtle_integer_codes(CN, _, CN, T, T).

exponent(C0, In, CN, [C0|T0]) :-
	e(C0), !,
	get_code(In, C1),
	optional_sign(C1, In, CN0, T0, T1),
	turtle_integer_codes(CN0, In, CN, T1, []).

optional_sign(C0, In, CN, [C0|T], T) :-
	sign(C0), !,
	get_code(In, CN).
optional_sign(CN, _, CN, T, T).

e(0'e).
e(0'E).

sign(0'-).
sign(0'+).				%'


					% language: [a-z]+ ('-' [a-z0-9]+ )*
language(C0, In, C, [C0|Codes]) :-
	code_type(C0, lower),
	get_code(In, C1),
	lwr_word(C1, In, C2, Codes, Tail),
	sub_langs(C2, In, C, Tail, []), !.
language(_, In, _, _) :-
	line_count(In, LineNo),
	syntax_error(In, LineNo, language_specifier).

lwr_word(C0, In, C, [C0|T0], T) :-
	code_type(C0, lower), !,
	get_code(In, C1),
	lwr_word(C1, In, C, T0, T).
lwr_word(C, _, C, T, T).

sub_langs(0'-, In, C, [0'-, C1|Codes], T) :- !,
	get_code(In, C1),
	lwrdig(C1), !,
	get_code(In, C2),
	lwrdigs(C2, In, C3, Codes, Tail),
	sub_langs(C3, In, C, Tail, T).
sub_langs(C, _, C, T, T).

lwrdig(C) :-
	code_type(C, lower), !.
lwrdig(C) :-
	code_type(C, digit).

lwrdigs(C0, In, C, [C0|T0], T) :-
	lwrdig(C0), !,
	get_code(In, C1),
	lwr_word(C1, In, C, T0, T).
lwrdigs(C, _, C, T, T).

					% resource_token
resource_token(0'<, In, C, relative_uri(URI)) :- !,
	turtle_read_relative_uri(0'<, In, C, URI).
resource_token(0':, In, C, Token) :- !,
	get_code(In, C0),
	(   turtle_read_name(C0, In, C, Name)
	->  Token = :(Name)
	;   Token = :,
	    C = C0
	).
resource_token(C0, In, C, Prefix:Name) :-
	turtle_read_name(C0, In, C1, Prefix),
	\+ sub_atom(Prefix, 0, _, _, '_'), !,
	C1 == 0':,
	get_code(In, C2),
	turtle_read_name(C2, In, C, Name).


punctuation(0'(, '(').
punctuation(0'), ')').
punctuation(0'[, '[').
punctuation(0'], ']').
punctuation(0',, ',').
punctuation(0'@, '@').
punctuation(0':, ':').
punctuation(0';, ';').

					% comment
skip_line(0xA, In, C) :- !,
	get_code(In, C).
skip_line(0xD, In, C) :- !,
	get_code(In, C).
skip_line(-1, _, -1) :- !.
skip_line(_, In, C) :-
	get_code(In, C1),
	skip_line(C1, In, C).

					% ws
turtle_ws(0x9).
turtle_ws(0xA).
turtle_ws(0xD).
turtle_ws(0x20).

syntax_error(Stream, Line, Which) :-
	syntax_error_term(Stream, Line, Which, Error),
	throw(Error).

syntax_error_term(Stream, -1, Which, Error) :- !,
	stream_property(Stream, file_name(File)),
	line_count(Stream, LineNo),
	line_position(Stream, LinePos),
	character_count(Stream, CharIndex),
	Error = error(syntax_error(Which),
		      file(File, LineNo, LinePos, CharIndex)).
syntax_error_term(Stream, LineNo, Which, Error) :-
	stream_property(Stream, file_name(File)),
	Error = error(syntax_error(Which),
		      file(File, LineNo, -1, -1)).


		 /*******************************
		 *	    RDF-DB HOOK		*
		 *******************************/

:- multifile
	rdf_db:rdf_load_stream/3,
	rdf_db:rdf_file_type/2.

rdf_db:rdf_load_stream(turtle, Stream, _Module:Options) :-
	rdf_db:graph(Options, Id),
	rdf_transaction(rdf_process_turtle(Stream, assert_triples, Options),
			parse(Id)).

assert_triples([], _).
assert_triples([rdf(S,P,O)|T], Location) :-
	rdf_assert(S,P,O,Location),
	assert_triples(T, Location).

rdf_db:rdf_file_type(ttl, turtle).
rdf_db:rdf_file_type(n3,  turtle).	% not really, but good enough
rdf_db:rdf_file_type(nt,  turtle).	% not really, but good enough
