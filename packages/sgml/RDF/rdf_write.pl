/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

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

:- module(rdf_write,
	  [ rdf_write_xml/2		% +Stream, +Triples
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(lists)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(assoc)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module write an RDF/XML document  from   a  list  of triples of the
format rdf(Subject, Predicate, Object).  It   is  primarily intended for
communicating computed RDF model fragments   to  external programs using
RDF/XML.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	     WRITE RDFXML	*
		 *******************************/

rdf_write_xml(Out, Triples) :-
	sort(Triples, Unique),
	rdf_write_header(Out, Unique),
	rdf_write_triples(Unique, Out),
	rdf_write_footer(Out).


		 /*******************************
		 *	  HEADER/FOOTER		*
		 *******************************/

%	rdf_write_header(+Out, +Triples)
%
%	Save XML document header, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_write_header(Out, Triples) :-
	xml_encoding(Out, Encoding),
	format(Out, '<?xml version=\'1.0\' encoding=\'~w\'?>~n', [Encoding]),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	used_namespaces(Triples, NSList),
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
	

xml_encoding(Out, Encoding) :-
	stream_property(Out, encoding(Enc)),
	(   xml_encoding_name(Enc, Encoding)
	->  true
	;   throw(error(domain_error(rdf_encoding, Enc), _))
	).

xml_encoding_name(ascii,       'US-ASCII').
xml_encoding_name(iso_latin_1, 'ISO-8859-1').
xml_encoding_name(utf8,        'UTF-8').


%	used_namespaces(+Triples, -List)
%
%	Return the list of namespaces used in a set of triples

used_namespaces(Triples, NSList) :-
	decl_used_predicate_ns(Triples),
	resources(Triples, Resources),
	empty_assoc(A0),
	put_assoc(rdf, A0, *, A1),	% needed for rdt:RDF
	res_used_namespaces(Resources, _NoNS, A1, A),
	assoc_to_list(A, List),
	keys(List, NSList).

keys([], []).
keys([K-_|T0], [K|T]) :-
	keys(T0, T).

res_used_namespaces([], [], A, A).
res_used_namespaces([Resource|T], NoNS, A0, A) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, _Local, Resource), !,
	put_assoc(NS, A0, *, A1),
	res_used_namespaces(T, NoNS, A1, A).
res_used_namespaces([R|T0], [R|T], A0, A) :-
	res_used_namespaces(T0, T, A0, A).

resources(Triples, Resources) :-
	phrase(resources(Triples), Raw),
	sort(Raw, Resources).

resources([]) -->
	[].
resources([rdf(S,P,O)|T]) -->
	[S,P],
	(   { atom(O)}
	->  [O]
	;   []
	),
	resources(T).
	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For every URL used as a predicate  we   *MUST*  define a namespace as we
cannot use names holding /, :, etc. as XML identifiers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
	predicate_ns/2.

decl_used_predicate_ns(Triples) :-
	retractall(predicate_ns(_,_)),
	(   member(rdf(_,P,_), Triples),
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


rdf_write_footer(Out) :-
	format(Out, '</rdf:RDF>~n', []).


		 /*******************************
		 *	      TRIPLES		*
		 *******************************/

rdf_write_triples(Triples, Out) :-
	rdf_write_triples(Triples, Out, [], Anon),
	rdf_write_anon(Anon, Out, Anon).

rdf_write_triples([], _, Anon, Anon).
rdf_write_triples([H|T0], Out, Anon0, Anon) :-
	arg(1, H, S),
	subject_triples(S, [H|T0], T, OnSubject),
	(   anonymous_subject(S)
	->  rdf_write_triples(T, Out, [anon(S,_,OnSubject)|Anon0], Anon)
	;   rdf_write_subject(OnSubject, S, Out, Anon0),
	    rdf_write_triples(T, Out, Anon0, Anon)
	).

subject_triples(S, [H|T0], T, [H|M]) :-
	arg(1, H, S), !,
	subject_triples(S, T0, T, M).
subject_triples(_, T, T, []).


rdf_write_anon([], _, _).
rdf_write_anon([anon(Subject, Done, Triples)|T], Out, Anon) :-
	Done \== true, !,
	rdf_write_subject(Triples, Subject, Out, Anon),
	rdf_write_anon(T, Out, Anon).
rdf_write_anon([_|T], Out, Anon) :-
	rdf_write_anon(T, Out, Anon).

rdf_write_subject(Triples, Subject, Out, Anon) :-
	rdf_write_subject(Triples, Out, Subject, -, 0, Anon), !,
	format(Out, '~n', []).
rdf_write_subject(_, Subject, _, _) :-
	throw(error(rdf_save_failed(Subject), 'Internal error')).

rdf_write_subject(Triples, Out, Subject, DefNS, Indent, Anon) :-
	rdf_equal(rdf:type, RdfType),
	select(rdf(_, RdfType,Type), Triples, Triples1),
	rdf_id(Type, DefNS, TypeId),
	xml_is_name(TypeId), !,
	format(Out, '~*|<~w', [Indent, TypeId]),
	save_about(Out, Subject),
	save_attributes(Triples1, DefNS, Out, TypeId, Indent, Anon).
rdf_write_subject(Triples, Out, Subject, _DefNS, Indent, Anon) :-
	format(Out, '~*|<rdf:Description', [Indent]),
	save_about(Out, Subject),
	save_attributes(Triples, rdf, Out, rdf:'Description', Indent, Anon).

xml_is_name(_NS:Atom) :- !,
	xml_name(Atom).
xml_is_name(Atom) :-
	xml_name(Atom).

save_about(_Out, Subject) :-
	anonymous_subject(Subject), !.
save_about(Out, Subject) :-
	stream_property(Out, encoding(Encoding)),
	rdf_value(Subject, QSubject, Encoding),
	format(Out, ' rdf:about="~w"', [QSubject]).

%	save_attributes(+List, +DefNS, +Out, Element)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Triples, DefNS, Out, Element, Indent, Anon) :-
	split_attributes(Triples, InTag, InBody),
	SubIndent is Indent + 2,
	save_attributes2(InTag, DefNS, tag, Out, SubIndent, Anon),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, _, body, Out, SubIndent, Anon),
	    format(Out, '~N~*|</~w>~n', [Indent, Element])
	).

%	split_attributes(+Triples, -HeadAttrs, -BodyAttr)
%	
%	Split attribute (Name=Value) list into attributes for the head
%	and body. Attributes can only be in the head if they are literal
%	and appear only one time in the attribute list.

split_attributes(Triples, HeadAttr, BodyAttr) :-
	duplicate_attributes(Triples, Dupls, Singles),
	simple_literal_attributes(Singles, HeadAttr, Rest),
	append(Dupls, Rest, BodyAttr).

%	duplicate_attributes(+Attrs, -Duplicates, -Singles)
%	
%	Extract attributes that appear more than onces as we cannot
%	dublicate an attribute in the head according to the XML rules.

duplicate_attributes([], [], []).
duplicate_attributes([H|T], Dupls, Singles) :-
	arg(2, H, Name),
	named_attributes(Name, T, D, R),
	D \== [],
	append([H|D], Dupls2, Dupls), !,
	duplicate_attributes(R, Dupls2, Singles).
duplicate_attributes([H|T], Dupls2, [H|Singles]) :-
	duplicate_attributes(T, Dupls2, Singles).

named_attributes(_, [], [], []) :- !.
named_attributes(Name, [H|T], D, R) :-
	(   arg(2, H, Name)
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

in_tag_attribute(rdf(_,_,literal(Text))) :-
	atom(Text),			% may not have lang qualifier
	atom_length(Text, Len),
	Len < 60.

%	save_attributes(+List, +DefNS, +TagOrBody, +Out)
%
%	Save a list of attributes.

save_attributes2([], _, _, _, _, _).
save_attributes2([H|T], DefNS, Where, Out, Indent, DB) :-
	save_attribute(Where, H, DefNS, Out, Indent, DB),
	save_attributes2(T, DefNS, Where, Out, Indent, DB).

save_attribute(tag, rdf(_, Name, literal(Value)), DefNS, Out, Indent, _Anon) :-
	AttIndent is Indent + 2,
	rdf_att_id(Name, DefNS, NameText),
	stream_property(Out, encoding(Encoding)),
	xml_quote_attribute(Value, QVal, Encoding),
	format(Out, '~N~*|~w="~w"', [AttIndent, NameText, QVal]).
save_attribute(body, rdf(_,Name,literal(Literal)), DefNS, Out, Indent, _) :- !,
	rdf_id(Name, DefNS, NameText),
	(   Literal = lang(Lang, Value)
	->  rdf_id(Lang, DefNS, LangText),
	    format(Out, '~N~*|<~w xml:lang="~w">',
		   [Indent, NameText, LangText])
	;   Literal = type(Type, Value)
	->  stream_property(Out, encoding(Encoding)),
	    rdf_value(Type, QVal, Encoding),
	    format(Out, '~N~*|<~w rdf:datatype="~w">',
		   [Indent, NameText, QVal])
	;   atomic(Literal)
	->  format(Out, '~N~*|<~w>', [Indent, NameText]),
	    Value = Literal
	;   format(Out, '~N~*|<~w rdf:parseType="Literal">',
		   [Indent, NameText]),
	    Value = Literal
	),
	save_attribute_value(Value, Out, Indent),
	format(Out, '</~w>', [NameText]).
save_attribute(body, rdf(_, Name, Value), DefNS, Out, Indent, Anon) :-
	anonymous_subject(Value),
	memberchk(anon(Value, true, ValueTriples), Anon), !,
	rdf_id(Name, DefNS, NameText),
	SubIndent is Indent + 2,
	(   rdf_equal(RdfType, rdf:type),
	    rdf_equal(ListClass, rdf:'List'),
	    memberchk(rdf(_, RdfType, ListClass), ValueTriples)
	->  format(Out, '~N~*|<~w rdf:parseType="Collection">~n',
		   [Indent, NameText]),
	    rdf_save_list(ValueTriples, Out, Value, DefNS, SubIndent, Anon)
	;   format(Out, '~N~*|<~w>~n',
		   [Indent, NameText]),
	    rdf_write_subject(ValueTriples, Out, Value, DefNS, SubIndent, Anon)
	),
	format(Out, '~N~*|</~w>~n', [Indent, NameText]).
save_attribute(body, rdf(_, Name, Value), DefNS, Out, Indent, _Anon) :-
	stream_property(Out, encoding(Encoding)),
	rdf_value(Value, QVal, Encoding),
	rdf_id(Name, DefNS, NameText),
	format(Out, '~N~*|<~w rdf:resource="~w"/>', [Indent, NameText, QVal]).

save_attribute_value(Value, Out, _) :-	% strings
	atom(Value), !,
	stream_property(Out, encoding(Encoding)),
	xml_quote_cdata(Value, QVal, Encoding),
	write(Out, QVal).
save_attribute_value(Value, Out, _) :-	% numbers
	number(Value), !,
	writeq(Out, Value).		% quoted: preserve floats
save_attribute_value(Value, Out, Indent) :-
	xml_is_dom(Value), !,
	XMLIndent is Indent+2,
	xml_write(Out, Value,
		  [ header(false),
		    indent(XMLIndent)
		  ]).
save_attribute_value(Value, _Out, _) :-
	throw(error(save_attribute_value(Value), _)).

rdf_save_list(_, _, List, _, _, _) :-
	rdf_equal(List, rdf:nil), !.
rdf_save_list(ListTriples, Out, List, DefNS, Indent, Anon) :-
	rdf_equal(RdfFirst, rdf:first),
	memberchk(rdf(List, RdfFirst, First), ListTriples),
	(   anonymous_subject(First),
	    memberchk(anon(First, true, FirstTriples), Anon)
	->  nl(Out),
	    rdf_write_subject(FirstTriples, Out, First, DefNS, Indent, Anon)
	;   stream_property(Out, encoding(Encoding)),
	    rdf_value(First, QVal, Encoding),
	    format(Out, '~N~*|<rdf:Description about="~w"/>',
		   [Indent, QVal])
	),
	(   rdf_equal(RdfRest, rdf:rest),
	    memberchk(rdf(List, RdfRest, List2), ListTriples),
	    \+ rdf_equal(List2, rdf:nil),
	    memberchk(anon(List2, true, List2Triples), Anon)
	->  rdf_save_list(List2Triples, Out, List2, DefNS, Indent, Anon)
	;   true
	).

%	anonymous_subject(+Subject)
%	
%	Test if a resource is anonymous. This is highly dubious.
%	Probably we need to store this in the database.  The current
%	release of the RDF parser guarantees that all anonymous ids
%	start with __.

anonymous_subject(S) :-
	atom(S),
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


%	rdf_value(+Resource, -Text, +Encoding)
%	
%	According  to  "6.4  RDF  URI  References"  of  the  RDF  Syntax
%	specification, a URI reference is  UNICODE string not containing
%	control sequences, represented as  UTF-8   and  then  as escaped
%	US-ASCII.
%	
%	NOTE: the to_be_described/1 trick  ensures   entity  rewrite  in
%	resources that start with 'http://t-d-b.org?'. This   is  a of a
%	hack to save the artchive data   in  the MultiMedian project. We
%	should use a more general mechanism.
%	
%	NOTE: < 5.6.3 doesn't  export   rdf_quote_uri/2.  Remove rdf_db:
%	after a while.

rdf_value(V, Text, Encoding) :-
	to_be_described(Prefix),
	atom_concat(Prefix, V1, V), !,
	ns(NS, Full),
	atom_concat(Full, Local, V1), !,
	rdf_db:rdf_quote_uri(Local, QLocal0),
	xml_quote_attribute(QLocal0, QLocal, Encoding),
	concat_atom([Prefix, '&', NS, (';'), QLocal], Text).
rdf_value(V, Text, Encoding) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	rdf_db:rdf_quote_uri(Local, QLocal0),
	xml_quote_attribute(QLocal0, QLocal, Encoding),
	concat_atom(['&', NS, (';'), QLocal], Text).
rdf_value(V, Q, Encoding) :-
	rdf_db:rdf_quote_uri(V, Q0),
	xml_quote_attribute(Q0, Q, Encoding).

to_be_described('http://t-d-b.org?').


		 /*******************************
		 *	       UTIL		*
		 *******************************/

ns(Id, Full) :-
	rdf_db:ns(Id, Full).
