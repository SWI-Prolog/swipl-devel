/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2007, University of Amsterdam

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
	    rdf_process_turtle/3	% +Input, :ObObject, +Options
	  ]).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(debug)).
:- use_module(library(url)).
:- use_module(library(http/http_open)).

:- meta_predicate
	rdf_process_turtle(+,:,+).

/** <module> Turtle - Terse RDF Triple Language

This module implements the Turtle  language   for  representing  the RDF
triple model as defined by Dave Beckett  from the Institute for Learning
and Research Technology University of Bristol in the document:

	* http://www.ilrt.bris.ac.uk/discovery/2004/01/turtle/

The current parser handles all positive   and negative examples provided
by the above document at october 17, 2004.

@tbd	* Much better error handling
	* Write turtle data
*/

%%	rdf_load_turtle(+Input, -Triples, +Options)
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

rdf_load_turtle(In, Triples, Options) :-
	open_input(In, Stream),
	init_state(In, Stream, Options, State),
	call_cleanup(phrase(turtle_file(State, Stream), Triples),
		     close(Stream)).


%%	rdf_process_turtle(+Input, :OnObject, +Options) is det.
%
%	Process Turtle input from Input, calling OnObject with a list
%	of triples.  Options is the same as for rdf_load_turtle/3.

rdf_process_turtle(In, OnObject, Options) :-
	strip_module(OnObject, M, G),
	open_input(In, Stream),
	init_state(In, Stream, Options, State),
	process_stream(State, Stream, M:G).


process_stream(State, In, OnObject) :-
	line_count(In, LineNo),
	nb_setarg(8, State, LineNo),
	(   turtle_tokens(In, Tokens)
	->  debug(turtle, 'Tokens: ~w~n', [Tokens])
	;   syntax_error(In, LineNo, illegal_token)
	),
	(   Tokens == end_of_file
	->  true
	;   phrase(triples(State, Triples), Tokens)
	->  (   Triples == []
	    ->	true
	    ;	arg(6, State, DB),
		call(OnObject, Triples, DB:LineNo)
	    ),
	    process_stream(State, In, OnObject)
	;   syntax_error(In, LineNo, cannot_parse)
	).


%%	open_input(+Input, -Stream) is det.
%
%	Open given input.
%
%	@tbd	Synchronize with input handling of rdf_db.pl.
%	@error	existence_error, permission_error

open_input(stream(Stream), Stream) :- !.
open_input(Stream, Stream) :-
	is_stream(Stream), !.
open_input(URL, Stream) :-
	sub_atom(URL, 0, _, _, 'http://'), !,
	http_open(URL, Stream, []).
open_input(File, Stream) :-
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
statement. State:

	| arg(1) | -BaseURI		|
	| arg(2) | Prefix --> URI map	|
	| arg(3) | NodeID --> URI map	|
	| arg(4) | AnonPrefix		|
	| arg(5) | AnonCount		|
	| arg(6) | DB			|
	| arg(7) | Input		|
	| arg(8) | StartLine		|
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

init_state(In, Stream, Options, State) :-
	(   option(base_uri(BaseURI), Options)
	->  true
	;   In = stream(_)
	->  BaseURI = []
	;   is_absolute_url(In)
	->  BaseURI = In
	;   absolute_file_name(In, File),
	    atom_concat('file://', File, BaseURI)
	),
	(   option(anon_prefix(Prefix), Options)
	->  true
	;   BaseURI == []
	->  Prefix = '__bnode'
	;   atom_concat('__', BaseURI, Prefix)
	),
	option(db(DB), Options, BaseURI),
	empty_assoc(Map),
	empty_assoc(NodeMap),
	State = state(BaseURI, Map, NodeMap, Prefix, 1, DB, Stream, 0).


turtle_file(State, In) -->
	{ line_count(In, LineNo),
	  nb_setarg(8, State, LineNo),
	  (   turtle_tokens(In, Tokens)
	  ->  debug(turtle, 'Tokens: ~w~n', [Tokens])
	  ;   syntax_error(In, LineNo, illegal_token)
	  )
	},
	(   { Tokens == end_of_file }
	->  []
	;   { phrase(triples(State, Triples), Tokens) }
	->  Triples,
	    turtle_file(State, In)
	;   { syntax_error(In, LineNo, cannot_parse)
	    }
	).

triples(State, []) -->
	[ '@', name(prefix), name(Prefix), : ], !,
	uri(State, URI),
	{ arg(2, State, Map0),
	  put_assoc(Prefix, Map0, URI, Map),
	  setarg(2, State, Map)
	}.
triples(State, []) -->
	[ '@', name(prefix), ':' ], !,
	uri(State, URI),
	{ setarg(1, State, URI)
	}.
triples(State, []) -->
      ['@',name(base)],!,
      uri(State,URI),
      { setarg(1,State,URI)
      }.
triples(State, Triples) -->
	subject(State, Subject, Triples, T),
	(   predicate_object_list(State, Subject, T, [])
	->  []
	;   { Triples \== T }		% [ p o ; ... ] .
	->  { T = [] }
	).
	
subject(State, Subject, T, T) -->
	resource(State, Subject), !.
subject(State, Subject, T0, T) -->
	blank(State, Subject, T0, T), !.
subject(State, _, _, _) -->
	syntax_error(State, subject_expected).

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
	  atom_codes(N, Codes)
	}.
object(State, Object, T, T) -->
	resource(State, Object), !.
object(State, Object, T0, T) -->
	blank(State, Object, T0, T), !.
object(State, _, _, _) -->
	syntax_error(State, expected_object).

term_expansion(numeric_url(I, Local),
	       numeric_url(I, URI)) :-
	rdf_global_id(Local, URI).

numeric_url(integer, xsd:integer).
numeric_url(decimal, xsd:decimal).
numeric_url(double,  xsd:double).

resource(State, URI) -->
	uri(State, URI), !.
resource(State, URI) -->
	[ :(Name) ], !,
	{ arg(1, State, Base),
	  atom_concat(Base, Name, URI)
	}.
resource(State, URI) -->
	[ name(Prefix), : ], !,
	{ arg(2, State, Map),
	  get_assoc(Prefix, Map, URI)
	}.
resource(State, URI) -->
	[ Prefix:Name ], !,
	{ arg(2, State, Map),
	  (   get_assoc(Prefix, Map, Base)
	  ->  atom_concat(Base, Name, URI)
	  ;   throw(error(existence_error(prefix, Prefix), _))
	  )
	}.
resource(State, BaseURI) -->
	[ : ], !,
	{ arg(1, State, BaseURI)
	}.
	

uri(State, URI) -->
	[ relative_uri(Rel)
	],
	{ arg(1, State, Base),
	  (   Rel == ''			% must be in global_url?
	  ->  URI = Base
	  ;   global_url(Rel, Base, URI)
	  )
	}.

blank(State, Resource, T, T) -->
	[ nodeId(NodeId) ], !,
	{ arg(3, State, IdMap),
	  (   get_assoc(NodeId, IdMap, Resource)
	  ->  true
	  ;   anonid(State, NodeId, Resource),
	      put_assoc(NodeId, IdMap, Resource, NewIdMap),
	      setarg(3, State, NewIdMap)
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
	arg(4, State, AnonPrefix),
	arg(5, State, Count),
	(   atom(AnonPrefix)
	->  atom_concat(AnonPrefix, Count, Node)
	;   Node = node(Count)
	),
	C2 is Count + 1,
	setarg(5, State, C2).

anonid(State, _NodeId, Node) :-
	arg(4, State, AnonPrefix),
	atom(AnonPrefix), !,
	anonid(State, Node).
anonid(_State, NodeId, node(NodeId)).

mk_object(type(Prefix:Name, Value), State, literal(type(Type, Value))) :- !,
	  arg(2, State, Map),
	  get_assoc(Prefix, Map, Base),
	  atom_concat(Base, Name, Type).
mk_object(type(relative_uri(Rel), Value), State, literal(type(Type, Value))) :- !,
	  arg(1, State, Base),
	  (   Rel == ''			% must be in global_url?
	  ->  Type = Base
	  ;   global_url(Rel, Base, Type)
	  ).
mk_object(type(:(Name), Value), State, literal(type(Type, Value))) :- !,
	  arg(1, State, Base),
	  atom_concat(Base, Name, Type).
mk_object(Value, _State, literal(Value)).

syntax_error(State, Error) -->
	[ Before ],
	{ arg(7, State, Stream),
	  stream_property(Stream, file_name(File)),
	  arg(8, State, LineNo),
	  format(string(Msg), '~w:~d (before ~w)',
		 [File, LineNo, Before]),
	  throw(error(syntax_error(Error),
		      context(_, Msg)))
	}.


		 /*******************************
		 *	     TOKENISER		*
		 *******************************/

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
turtle_token(0'., In, C, '.') :- !,
	get_code(In, C).
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
	get_code(In, C1),
	turtle_string(C1, In, C2, Codes),
	atom_codes(Atom, Codes),
	(   C2 == 0'@
	->  get_code(In, C3),
	    language(C3, In, C, LangCodes),
	    atom_codes(LangId, LangCodes),
	    Literal = literal(lang(LangId, Atom))
	;   C2 == 0'^,
	    peek_code(In, 0'^)
	->  get_code(In, 0'^),
	    get_code(In, C3),
	    resource_token(C3, In, C, Type),
	    Literal = literal(type(Type, Atom))
	;   C = C2,
	    Literal = literal(Atom)
	).
turtle_token(0'_, In, C, nodeId(NodeID)) :-
	peek_code(In, 0':), !,
	get_code(In, _),
	get_code(In, C1),
	name(C1, In, C, NodeID).
turtle_token(0'<, In, C, URI) :- !,
	resource_token(0'<, In, C, URI).
turtle_token(0':, In, C, URI) :- !,
	resource_token(0':, In, C, URI).
turtle_token(C0, In, C, Token) :-
	name(C0, In, C1, Name), !,
	(   C1 == 0':,
	    \+ sub_atom(Name, 0, _, _, '_'),
	    peek_code(In, C2),
	    name_start_char(C2)
	->  get_code(In, C2),
	    name(C2, In, C, Name2),
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

%%	turtle_string(+Char, +In, -NextChar, -String) is det.
%
%	Extract a turtle string. We have  seen   a  single  ". There are
%	-like Python- two types  of  turtle   strings,  one  written  as
%	"hello" and the other as """hello""".

turtle_string(0'", In, Next, String) :-
	get_code(In, C),
	(   C == 0'"
	->  get_code(In, C2),
	    turtle_q_string(C2, In, Next, q3, String)
	;   Next = C,
	    String = []
	).
turtle_string(C, In, Next, String) :-
	turtle_q_string(C, In, Next, q1, String).


turtle_q_string(-1, In, _, _, []) :- !,
	syntax_error(In, -1, unexpected_end_of_input).
turtle_q_string(0'", In, C, Q, String) :- !,
	get_code(In, C2),
	(   Q == q1
	->  C = C2,
	    String = []
	;   /* Q == q3 */
	    C2 == 0'"
        ->  get_code(In, C3),
	    (	C3 == 0'"
	    ->  get_code(In, C),
	        String = []
	    ;   String = [0'", 0'"|Rest],
	        turtle_q_string(C3, In, C, q3, Rest)
	    )
	;   String = [0'"|Rest],
	    turtle_q_string(C2, In, C, q3, Rest)
	).
turtle_q_string(0'\\, In, C, Q, [H|T]) :- !,
	get_code(In, C1),
	string_escape(C1, In, C2, H),
	turtle_q_string(C2, In, C, Q, T).
turtle_q_string(C0, In, C, Q, [C0|T]) :-
	get_code(In, C1),
	turtle_q_string(C1, In, C, Q, T).


string_escape(0'n, In, C, 0'\n) :- !,
	get_code(In, C).
string_escape(0'", In, C, 0'") :- !,
	get_code(In, C).
string_escape(0'\\, In, C, 0'\\) :- !,
	get_code(In, C).
string_escape(0't, In, C, 0'\t) :- !,
	get_code(In, C).
string_escape(0'r, In, C, 0'\r) :- !,
	get_code(In, C).
string_escape(0'u, In, C, Code) :- !,
	get_hhhh(In, Code),
	get_code(In, C).
string_escape(0'U, In, C, Code) :- !,
	get_hhhh(In, Code0),
	get_hhhh(In, Code1),
	Code is Code0 << 16 + Code1,
	get_code(In, C).

get_hhhh(In, Code) :-
	get_code(In, C1), code_type(C1, xdigit(D1)),
	get_code(In, C2), code_type(C2, xdigit(D2)),
	get_code(In, C3), code_type(C3, xdigit(D3)),
	get_code(In, C4), code_type(C4, xdigit(D4)),
	Code is D1<<12+D2<<8+D3<<4+D4.

					% language: [a-z]+ ('-' [a-z0-9]+ )*
language(C0, In, C, [C0|Codes]) :-
	code_type(C0, lower),
	get_code(In, C1),
	lwr_word(C1, In, C2, Codes, Tail),
	sub_langs(C2, In, C, Tail, []).

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
	get_code(In, C1),
	uri_chars(C1, In, C, Codes),
	atom_codes(URI, Codes).
resource_token(0':, In, C, Token) :- !,
	get_code(In, C0),
	(   name(C0, In, C, Name)
	->  Token = :(Name)
	;   Token = :,
	    C = C0
	).
resource_token(C0, In, C, Prefix:Name) :-
	name(C0, In, C1, Prefix),
	\+ sub_atom(Prefix, 0, _, _, '_'), !,
	C1 == 0':,
	get_code(In, C2),
	name(C2, In, C, Name).


%%	uri_chars(+Char, +In:stream, -NextChar, -UriChars) is semidet.

uri_chars(0'>, In, C, []) :- !,
	get_code(In, C).
uri_chars(0'\\, In, C, [H|T]) :- !,
	get_code(In, C1),
	string_escape(C1, In, C2, H),
	uri_chars(C2, In, C, T).
uri_chars(-1, _, _, _) :- !,
	fail.
uri_chars(C0, In, C, [C0|T]) :-
	get_code(In, C1),
	uri_chars(C1, In, C, T).

					% name
name(C0, In, C, Atom) :-
	name_start_char(C0),
	get_code(In, C1),
	name_chars(C1, In, C, T),
	atom_codes(Atom, [C0|T]).

name_chars(C0, In, C, [C0|T]) :-
	name_char(C0), !,
	get_code(In, C1),
	name_chars(C1, In, C, T).
name_chars(C, _, C, []).

name_start_char(C) :- code_type(C, csymf).
name_start_char(C) :- between(0xC0, 0xD6, C).
name_start_char(C) :- between(0xD8, 0xF6, C).
name_start_char(C) :- between(0xF8, 0x2FF, C).
name_start_char(C) :- between(0x370, 0x37D, C).
name_start_char(C) :- between(0x37F, 0x1FFF, C).
name_start_char(C) :- between(0x200C, 0x200D, C).
name_start_char(C) :- between(0x2070, 0x218F, C).
name_start_char(C) :- between(0x2C00, 0x2FEF, C).
name_start_char(C) :- between(0x3001, 0xD7FF, C).
name_start_char(C) :- between(0xF900, 0xFDCF, C).
name_start_char(C) :- between(0xFDF0, 0xFFFD, C).
name_start_char(C) :- between(0x10000, 0xEFFFF, C).

name_char(C) :-	name_start_char(C).
name_char(0'-).
name_char(D) :-	code_type(D, digit).
name_char(0xB7).
name_char(C) :- between(0x0300, 0x036F, C).
name_char(C) :- between(0x203F, 0x2040, C).

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
skip_line(_, In, C) :- !,
	get_code(In, C1),
	skip_line(C1, In, C).

					% ws
turtle_ws(0x9).
turtle_ws(0xA).
turtle_ws(0xD).
turtle_ws(0x20).

syntax_error(Stream, -1, Which) :- !,
	stream_property(Stream, file_name(File)),
	line_count(Stream, LineNo),
	line_position(Stream, LinePos),
	character_count(Stream, CharIndex),
	throw(error(syntax_error(Which),
		    file(File, LineNo, LinePos, CharIndex))).
syntax_error(Stream, LineNo, Which) :-
	stream_property(Stream, file_name(File)),
	throw(error(syntax_error(Which),
		    file(File, LineNo, -1, -1))).

		 /*******************************
		 *	    RDF-DB HOOK		*
		 *******************************/

:- multifile
	rdf_db:rdf_load_stream/3,
	rdf_db:rdf_file_type/2.

rdf_db:rdf_load_stream(turtle, Stream, Options) :-
	option(db(Id), Options),
	rdf_transaction(rdf_process_turtle(Stream, assert_triples, Options),
			parse(Id)).

assert_triples([], _).
assert_triples([rdf(S,P,O)|T], Location) :-
	rdf_assert(S,P,O,Location),
	assert_triples(T, Location).

rdf_db:rdf_file_type(ttl, turtle).


		 /*******************************
		 *	   XREF SUPPORT		*
		 *******************************/

:- multifile
	prolog:meta_goal/2.

prolog:meta_goal(rdf_process_turtle(_,G,_), [G+2]).
