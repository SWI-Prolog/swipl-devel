/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
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

:- module(rdf_write,
	  [ rdf_write_xml/2		% +Stream, +Triples
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(lists)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(library(debug)).


/** <module> Write RDF/XML from a list of triples

This module writes an RDF/XML document  from   a  list of triples of the
format rdf(Subject, Predicate, Object).  It   is  primarily intended for
communicating computed RDF model fragments   to  external programs using
RDF/XML.

When used from the HTTP library, use the following code:

==
reply_graph(RDF) :-
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n'),
	rdf_write_xml(current_output, RDF).
==

@author	Jan Wielemaker
@see	library(semweb/rdf_db) offers saving a named graph directly from
	the RDF database.
*/


		 /*******************************
		 *	     WRITE RDFXML	*
		 *******************************/

%%	rdf_write_xml(+Out:stream, +Triples:list(rdf(S,P,O))) is det.
%
%	Write an RDF/XML serialization of Triples to Out.

rdf_write_xml(Out, Triples) :-
	sort(Triples, Unique),
	rdf_write_header(Out, Unique),
	node_id_map(Unique, AnonIDs),
	rdf_write_triples(Unique, AnonIDs, Out),
	rdf_write_footer(Out).


		 /*******************************
		 *	  HEADER/FOOTER		*
		 *******************************/

%%	rdf_write_header(+Out, +Triples)
%
%	Save XML document header, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_write_header(Out, Triples) :-
	xml_encoding(Out, Enc, Encoding),
	format(Out, '<?xml version=\'1.0\' encoding=\'~w\'?>~n', [Encoding]),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	used_namespaces(Triples, NSList),
	(   member(Id, NSList),
	    ns(Id, NS),
	    rdf_quote_uri(NS, QNS),
	    xml_quote_attribute(QNS, NSText0, Enc),
	    xml_escape_parameter_entity(NSText0, NSText),
	    format(Out, '~N    <!ENTITY ~w \'~w\'>', [Id, NSText]),
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


xml_encoding(Out, Enc, Encoding) :-
	stream_property(Out, encoding(Enc)),
	(   xml_encoding_name(Enc, Encoding)
	->  true
	;   throw(error(domain_error(rdf_encoding, Enc), _))
	).

xml_encoding_name(ascii,       'US-ASCII').
xml_encoding_name(iso_latin_1, 'ISO-8859-1').
xml_encoding_name(utf8,        'UTF-8').

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

%%	used_namespaces(+Triples:list(rdf(S,P,O)), -List:atom) is det.
%
%	Return the list of namespace abbreviations used in a set of
%	triples.

used_namespaces(Triples, NSList) :-
	decl_used_predicate_ns(Triples),
	resources(Triples, Resources),
	empty_assoc(A0),
	put_assoc(rdf, A0, *, A1),	% needed for rdf:RDF
	res_used_namespaces(Resources, _NoNS, A1, A),
	assoc_to_keys(A, NSList).


res_used_namespaces([], [], A, A).
res_used_namespaces([Resource|T], NoNS, A0, A) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Resource),
	xml_name(Local), !,
	put_assoc(NS, A0, *, A1),
	res_used_namespaces(T, NoNS, A1, A).
res_used_namespaces([R|T0], [R|T], A0, A) :-
	res_used_namespaces(T0, T, A0, A).

%%	resources(+Triples:list(rdf(S,P,O)), -Resources:list(atom)) is det.
%
%	Resources is the set of resources referenced in Triples.

resources(Triples, Resources) :-
	phrase(resources(Triples), Raw),
	sort(Raw, Resources).

resources([]) -->
	[].
resources([rdf(S,P,O)|T]) -->
	[S,P],
	object_resources(O),
	resources(T).

object_resources(Atom) -->
	{ atom(Atom) }, !,
	[ Atom ].
object_resources(literal(type(Type, _))) --> !,
	[ Type ].
object_resources(_) -->
	[].

%%	decl_used_predicate_ns(+Triples:list(rdf(S,P,O)))
%
%	For every URL used as a predicate   we *MUST* define a namespace
%	as we cannot use names holding /, :, etc. as XML identifiers.

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
	rdf_global_id(NS:Local, Pred),
	xml_name(Local), !,
	assert(predicate_ns(Pred, NS)).
decl_predicate_ns(Pred) :-
	is_bag_li_predicate(Pred), !.
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
xml_code(0'-).				% '


rdf_write_footer(Out) :-
	format(Out, '</rdf:RDF>~n', []).


		 /*******************************
		 *	    ANONYMOUS IDS	*
		 *******************************/

%%	node_id_map(+Triples, -IdMap) is det.
%
%	Create an assoc Resource -> NodeID for those anonymous resources
%	in Triples that need  a  NodeID.   This  implies  all  anonymous
%	resources that are used multiple times as object value.

node_id_map(Triples, IdMap) :-
	anonymous_objects(Triples, Objs),
	msort(Objs, Sorted),
	empty_assoc(IdMap0),
	nodeid_map(Sorted, 0, IdMap0, IdMap).

anonymous_objects([], []).
anonymous_objects([rdf(_,_,O)|T0], Anon) :-
	rdf_is_bnode(O), !,
	Anon = [O|T],
	anonymous_objects(T0, T).
anonymous_objects([_|T0], T) :-
	anonymous_objects(T0, T).

nodeid_map([], _, Map, Map).
nodeid_map([H,H|T0], Id, Map0, Map) :- !,
	remove_leading(H, T0, T),
	atom_concat(bn, Id, NodeId),
	put_assoc(H, Map0, NodeId, Map1),
	Id2 is Id + 1,
	nodeid_map(T, Id2, Map1, Map).
nodeid_map([_|T], Id, Map0, Map) :-
	nodeid_map(T, Id, Map0, Map).

remove_leading(H, [H|T0], T) :- !,
	remove_leading(H, T0, T).
remove_leading(_, T, T).


		 /*******************************
		 *	      TRIPLES		*
		 *******************************/

rdf_write_triples(Triples, NodeIDs, Out) :-
	rdf_write_triples(Triples, NodeIDs, Out, [], Anon),
	rdf_write_anon(Anon, NodeIDs, Out, Anon).

rdf_write_triples([], _, _, Anon, Anon).
rdf_write_triples([H|T0], NodeIDs, Out, Anon0, Anon) :-
	arg(1, H, S),
	subject_triples(S, [H|T0], T, OnSubject),
	(   rdf_is_bnode(S)
	->  rdf_write_triples(T, NodeIDs, Out, [anon(S,_,OnSubject)|Anon0], Anon)
	;   rdf_write_subject(OnSubject, S, NodeIDs, Out, Anon0),
	    rdf_write_triples(T, NodeIDs, Out, Anon0, Anon)
	).

subject_triples(S, [H|T0], T, [H|M]) :-
	arg(1, H, S), !,
	subject_triples(S, T0, T, M).
subject_triples(_, T, T, []).


rdf_write_anon([], _, _, _).
rdf_write_anon([anon(Subject, Done, Triples)|T], NodeIDs, Out, Anon) :-
	Done \== true, !,
	Done = true,
	rdf_write_subject(Triples, Subject, NodeIDs, Out, Anon),
	rdf_write_anon(T, NodeIDs, Out, Anon).
rdf_write_anon([_|T], NodeIDs, Out, Anon) :-
	rdf_write_anon(T, NodeIDs, Out, Anon).

rdf_write_subject(Triples, Subject, NodeIDs, Out, Anon) :-
	rdf_write_subject(Triples, Out, Subject, NodeIDs, -, 0, Anon), !,
	format(Out, '~n', []).
rdf_write_subject(_, Subject, _, _, _) :-
	throw(error(rdf_save_failed(Subject), 'Internal error')).

rdf_write_subject(Triples, Out, Subject, NodeIDs, DefNS, Indent, Anon) :-
	rdf_equal(rdf:type, RdfType),
	select(rdf(_, RdfType,Type), Triples, Triples1),
	\+ rdf_is_bnode(Type),
	rdf_id(Type, DefNS, TypeId),
	xml_is_name(TypeId), !,
	format(Out, '~*|<', [Indent]),
	rdf_write_id(Out, TypeId),
	save_about(Out, Subject, NodeIDs),
	save_attributes(Triples1, DefNS, Out, NodeIDs, TypeId, Indent, Anon).
rdf_write_subject(Triples, Out, Subject, NodeIDs, _DefNS, Indent, Anon) :-
	format(Out, '~*|<rdf:Description', [Indent]),
	save_about(Out, Subject, NodeIDs),
	save_attributes(Triples, rdf, Out, NodeIDs, rdf:'Description', Indent, Anon).

xml_is_name(_NS:Atom) :- !,
	xml_name(Atom).
xml_is_name(Atom) :-
	xml_name(Atom).

save_about(Out, Subject, NodeIDs) :-
	rdf_is_bnode(Subject), !,
	(   get_assoc(Subject, NodeIDs, NodeID)
	->  format(Out,' rdf:nodeID="~w"', [NodeID])
	;   true
	).
save_about(Out, Subject, _) :-
	stream_property(Out, encoding(Encoding)),
	rdf_value(Subject, QSubject, Encoding),
	format(Out, ' rdf:about="~w"', [QSubject]), !.
save_about(_, _, _) :-
	assertion(fail).

%%	save_attributes(+List, +DefNS, +Out, +NodeIDs, Element, +Indent, +Anon)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Triples, DefNS, Out, NodeIDs, Element, Indent, Anon) :-
	split_attributes(Triples, InTag, InBody),
	SubIndent is Indent + 2,
	save_attributes2(InTag, DefNS, tag, Out, NodeIDs, SubIndent, Anon),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, _, body, Out, NodeIDs, SubIndent, Anon),
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

in_tag_attribute(rdf(_,P,literal(Text))) :-
	atom(Text),			% may not have lang qualifier
	atom_length(Text, Len),
	Len < 60,
	\+ is_bag_li_predicate(P).


%	save_attributes(+List, +DefNS, +TagOrBody, +Out, +NodeIDs, +Indent, +Anon)
%
%	Save a list of attributes.

save_attributes2([], _, _, _, _, _, _).
save_attributes2([H|T], DefNS, Where, Out, NodeIDs, Indent, Anon) :-
	save_attribute(Where, H, DefNS, Out, NodeIDs, Indent, Anon),
	save_attributes2(T, DefNS, Where, Out, NodeIDs, Indent, Anon).

%%	save_attribute(+Where, +Triple, +DefNS, +Out, +NodeIDs, +Indent, +Anon)

save_attribute(tag, rdf(_, Name, literal(Value)), DefNS, Out, _, Indent, _Anon) :-
	AttIndent is Indent + 2,
	rdf_att_id(Name, DefNS, NameText),
	stream_property(Out, encoding(Encoding)),
	xml_quote_attribute(Value, QVal, Encoding),
	format(Out, '~N~*|', [AttIndent]),
	rdf_write_id(Out, NameText),
	format(Out, '="~w"', [QVal]).
save_attribute(body, rdf(_,Name,literal(Literal)), DefNS, Out, _, Indent, _) :- !,
	rdf_p_id(Name, DefNS, NameText),
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	(   Literal = lang(Lang, Value)
	->  rdf_id(Lang, DefNS, LangText),
	    format(Out, ' xml:lang="~w">', [LangText])
	;   Literal = type(Type, Value)
	->  (   rdf_equal(Type, rdf:'XMLLiteral')
	    ->	write(Out, ' rdf:parseType="Literal">'),
		Value = Literal
	    ;	stream_property(Out, encoding(Encoding)),
		rdf_value(Type, QVal, Encoding),
		format(Out, ' rdf:datatype="~w">', [QVal])
	    )
	;   atomic(Literal)
	->  write(Out, '>'),
	    Value = Literal
	;   write(Out, ' rdf:parseType="Literal">'),
	    Value = Literal
	),
	save_attribute_value(Value, Out, Indent),
	write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_attribute(body, rdf(_, Name, Value), DefNS, Out, NodeIDs, Indent, Anon) :-
	rdf_is_bnode(Value),
	memberchk(anon(Value, Done, ValueTriples), Anon), !,
	rdf_p_id(Name, DefNS, NameText),
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	(   var(Done)
	->  Done = true,
	    SubIndent is Indent + 2,
	    (   rdf_equal(RdfType, rdf:type),
		rdf_equal(ListClass, rdf:'List'),
		memberchk(rdf(_, RdfType, ListClass), ValueTriples)
	    ->  format(Out, ' rdf:parseType="Collection">~n', []),
		rdf_save_list(ValueTriples, Out, Value, NodeIDs, DefNS, SubIndent, Anon)
	    ;   format(Out, '>~n', []),
		rdf_write_subject(ValueTriples, Out, Value, NodeIDs, DefNS, SubIndent, Anon)
	    ),
	    format(Out, '~N~*|</', [Indent]),
	    rdf_write_id(Out, NameText),
	    format(Out, '>~n', [])
	;   get_assoc(Value, NodeIDs, NodeID)
	->  format(Out, ' rdf:nodeID="~w"/>', [NodeID])
	;   assertion(fail)
	).
save_attribute(body, rdf(_, Name, Value), DefNS, Out, _, Indent, _Anon) :-
	stream_property(Out, encoding(Encoding)),
	rdf_value(Value, QVal, Encoding),
	rdf_p_id(Name, DefNS, NameText),
	format(Out, '~N~*|<', [Indent]),
	rdf_write_id(Out, NameText),
	format(Out, ' rdf:resource="~w"/>', [QVal]).

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

rdf_save_list(_, _, List, _, _, _, _) :-
	rdf_equal(List, rdf:nil), !.
rdf_save_list(ListTriples, Out, List, NodeIDs, DefNS, Indent, Anon) :-
	rdf_equal(RdfFirst, rdf:first),
	memberchk(rdf(List, RdfFirst, First), ListTriples),
	(   rdf_is_bnode(First),
	    memberchk(anon(First, true, FirstTriples), Anon)
	->  nl(Out),
	    rdf_write_subject(FirstTriples, Out, First, NodeIDs, DefNS, Indent, Anon)
	;   stream_property(Out, encoding(Encoding)),
	    rdf_value(First, QVal, Encoding),
	    format(Out, '~N~*|<rdf:Description about="~w"/>',
		   [Indent, QVal])
	),
	(   rdf_equal(RdfRest, rdf:rest),
	    memberchk(rdf(List, RdfRest, List2), ListTriples),
	    \+ rdf_equal(List2, rdf:nil),
	    memberchk(anon(List2, true, List2Triples), Anon)
	->  rdf_save_list(List2Triples, Out, List2, NodeIDs, DefNS, Indent, Anon)
	;   true
	).

%%	rdf_p_id(+Resource, +DefNS, -NSLocal)
%
%	As rdf_id/3 for predicate names.  Maps _:<N> to rdf:li.
%
%	@tbd	Ensure we are talking about an rdf:Bag

rdf_p_id(LI, _, 'rdf:li') :-
	is_bag_li_predicate(LI), !.
rdf_p_id(Resource, DefNS, NSLocal) :-
	rdf_id(Resource, DefNS, NSLocal).

%%	is_bag_li_predicate(+Pred) is semidet.
%
%	True if Pred is _:N, as used  for members of an rdf:Bag, rdf:Seq
%	or rdf:Alt.

is_bag_li_predicate(Pred) :-
	atom_concat('_:', AN, Pred),
	catch(atom_number(AN, N), _, true), integer(N), N >= 0, !.


%%	rdf_id(+Resource, +DefNS, -NSLocal)
%
%	Generate a NS:Local name for Resource given the indicated
%	default namespace.  This call is used for elements.

rdf_id(Id, NS, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id),
	xml_name(Local), !.
rdf_id(Id, _, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id),
	xml_name(Local), !.
rdf_id(Id, _, Id).


%%	rdf_write_id(+Out, +NSLocal) is det.
%
%	Write an identifier. We cannot use native write on it as both NS
%	and Local can be operators.

rdf_write_id(Out, NS:Local) :- !,
	format(Out, '~w:~w', [NS, Local]).
rdf_write_id(Out, Atom) :-
	write(Out, Atom).


%%	rdf_att_id(+URI, +DefNS, -ID)

rdf_att_id(Id, _, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id),
	xml_name(Local), !.
rdf_att_id(Id, _, Id).


%%	rdf_value(+Resource, -Text, +Encoding)
%
%	According  to  "6.4  RDF  URI  References"  of  the  RDF  Syntax
%	specification, a URI reference is  UNICODE string not containing
%	control sequences, represented as  UTF-8   and  then  as escaped
%	US-ASCII.
%
%	NOTE: the to_be_described/1 trick  ensures   entity  rewrite  in
%	resources that start with 'http://t-d-b.org?'. This   is  a of a
%	hack to save the artchive data   in  the MultimediaN project. We
%	should use a more general mechanism.

rdf_value(V, Text, Encoding) :-
	to_be_described(Prefix),
	atom_concat(Prefix, V1, V),
	ns(NS, Full),
	atom_concat(Full, Local, V1), !,
	rdf_quote_uri(Local, QLocal0),
	xml_quote_attribute(QLocal0, QLocal, Encoding),
	atomic_list_concat([Prefix, '&', NS, (';'), QLocal], Text).
rdf_value(V, Text, Encoding) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	rdf_quote_uri(Local, QLocal0),
	xml_quote_attribute(QLocal0, QLocal, Encoding),
	atomic_list_concat(['&', NS, (';'), QLocal], Text).
rdf_value(V, Q, Encoding) :-
	rdf_quote_uri(V, Q0),
	xml_quote_attribute(Q0, Q, Encoding).

to_be_described('http://t-d-b.org?').


		 /*******************************
		 *	       UTIL		*
		 *******************************/

ns(Id, Full) :-
	rdf_db:ns(Id, Full).
