/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_nt,
	  [ load_rdf_nt/2		% +File, -Triples
	  ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module parses n-tuple files as defined   by  the W3C RDF working in
http://www.w3.org/TR/rdf-testcases/#ntriples.   This   format     is   a
simplified version of the RDF N3 notation   used  in the *.nt files that
are used to describe the normative outcome of the RDF test-cases.

The returned list terms are of the form

	rdf(Subject, Predicate, Object)

where

	# Subject
	is an atom or node(Id) for anonymous nodes

	# Predicate
	is an atom

	# Object
	is an atom, node(Id), literal(Atom) or xml(Atom)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


load_rdf_nt(File, Triples) :-
	open_nt_file(File, In, Close),
	call_cleanup(stream_to_triples(In, Triples), Close).

%	open_nt_file(+Input, -Stream, -Close)
%	
%	Open Input, returning Stream and a goal to cleanup Stream if it
%	was opened.

open_nt_file(stream(Stream), Stream, true).
open_nt_file('$stream'(In), '$stream'(In), true).
open_nt_file(Spec, Stream, close(Stream)) :-
	absolute_file_name(Spec,
			   [ access(read),
			     extensions([nt,''])
			   ], Path),
	open(Path, read, Stream).

%	stream_to_triples(+Stream, -ListOfTriples)
%	
%	Read Stream, returning all its triples

stream_to_triples(In, Triples) :-
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  Triples = []
	;   phrase(line(Triples, Tail), Line),
	    stream_to_triples(In, Tail)
	).

line(Triples, Tail) -->
	wss,
	(   comment
	->  {Triples = Tail}
	;   triple(Triple)
	->  {Triples = [Triple|Tail]}
	).

comment -->
	"#", !,
	skip_rest.
comment -->
	end_of_input.

triple(rdf(Subject, Predicate, Object)) -->
	subject(Subject), ws, wss,
	predicate(Predicate), ws, wss,
	object(Object), wss, ".", wss.

subject(Subject) -->
	uniref(Subject), !.
subject(Subject) -->
	node_id(Subject).

predicate(Predicate) -->
	uniref(Predicate).

object(Object) -->
	uniref(Object), !.
object(Object) -->
	node_id(Object).
object(Object) -->
	literal(Object).


uniref(URI) -->
	"<",
	escaped_uri_codes(Codes),
	">", !,
	{ atom_codes(URI, Codes)
	}.

node_id(node(Id)) -->			% anonymous nodes
	"_:",
	name_start(C0),
	name_codes(Codes),
	{ atom_codes(Id, [C0|Codes])
	}.

literal(Literal) -->
	lang_string(Literal), !.
literal(Literal) -->
	xml_string(Literal).
	

%	name_start(-Code)
%	name_codes(-ListfCodes)
%	
%	Parse identifier names

name_start(C) -->
	[C],
	{ code_type(C, alpha)
	}.

name_codes([C|T]) -->
	[C],
	{ code_type(C, alnum)
	}, !,
	name_codes(T).
name_codes([]) -->
	[].


%	escaped_uri_codes(-CodeList)
%	
%	Decode string holding %xx escaped characters.

escaped_uri_codes([]) -->
	[].
escaped_uri_codes([C|T]) -->
	"%", [D0,D1], !,
	{ code_type(D0, xdigit(V0)),
	  code_type(D1, xdigit(V1)),
	  C is V0<<4 + V1
	},
	escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
	[C],
	escaped_uri_codes(T).


%	lang_string()
%	
%	Process a language string

lang_string(String) -->
	"\"",
	string(Codes),
	"\"", !,
	{ atom_codes(Atom, Codes)
	},
	(   "-"
	->  language(Lang),
	    { String = literal(Atom, Lang)
	    }
	;   { String = literal(Atom)
	    }
	).

%	xml_string(String)
%	
%	Handle xml"..."

xml_string(xml(String)) -->
	"xml\"",			% really no whitespace?
	string(Codes),
	"\"",
	{ atom_codes(String, Codes)
	}.

string([]) -->
	[].
string([C0|T]) -->
	string_char(C0),
	string(T).

string_char(0'\) -->
	"\\\\".
string_char(0'") -->
	"\\\"".
string_char(10) -->
	"\\n".
string_char(13) -->
	"\\r".
string_char(9) -->
	"\\t".
string_char(C) -->
	"\\u",
	'4xdigits'(C).
string_char(C) -->
	"\\u",
	'4xdigits'(C0),
	'4xdigits'(C1),
	{ C is C0<<16 + C1
	}.
string_char(C) -->
	[C].
	
'4xdigits'(C) -->
	[C0,C1,C2,C3],
	{ code_type(C0, xdigit(V0)),
	  code_type(C1, xdigit(V1)),
	  code_type(C2, xdigit(V2)),
	  code_type(C3, xdigit(V3)),
	  
	  C is V0<<12 + V1<<8 + V2<<4 + V3
	}.

%	language(-Lang)
%	
%	Return xml:lang language identifier.

language(Lang) -->
	lang_code(C0),
	lang_codes(Codes),
	{ atom_codes(Lang, [C0|Codes])
	}.

lang_code(C) -->
	[C],
	{ C \== 0'.,
	  \+ code_type(C, white)
	}.

lang_codes([C|T]) -->
	lang_code(C), !,
	lang_codes(T).
lang_codes([]) -->
	[].


		 /*******************************
		 *	       BASICS		*
		 *******************************/

skip_rest(_,[]).

ws -->
	[C],
	{ code_type(C, white)
	}.

end_of_input([], []).
	

wss -->
	ws, !,
	wss.
wss -->
	[].
