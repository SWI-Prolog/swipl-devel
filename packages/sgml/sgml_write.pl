/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker & Richard O'Keefe
    E-mail:        wielemaker@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

:- module(sgml_write,
	  [ html_write/2,       	%          +Data, +Options
	    html_write/3,		% +Stream, +Data, +Options
	    sgml_write/2,		%          +Data, +Options
	    sgml_write/3,		% +Stream, +Data, +Options
	    xml_write/2,		%          +Data, +Options
	    xml_write/3			% +Stream, +Data, +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(sgml)).
:- use_module(library(debug)).
:- use_module(library(assoc)).
:- use_module(library(option)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides the inverse functionality   of  the sgml.pl parser
library, writing XML, SGML and HTML documents from the parsed output. It
is intended to allow rewriting in a  different dialect or encoding or to
perform document transformation in Prolog on the parsed representation.

The current implementation is  particularly   keen  on getting character
encoding and the use of character  entities   right.  Some work has been
done providing layout, but space handling in   XML  and SGML make this a
very hazardous area.

The Prolog-based low-level character and  escape   handling  is the real
bottleneck in this library and will probably be   moved  to C in a later
stage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	xml_write( [+Stream,] +Data, +Options)
%	sgml_write([+Stream,] +Data, +Options)
%	html_write([+Stream,] +Data, +Options)
%	
%	Write a term as created by the SGML/XML parser to a stream in
%	SGML or XML format.  Options:
%	
%		* dtd(DTD)
%		The DTD.  This is needed for SGML documents that contain
%		elements with content model EMPTY.  Characters which may
%		not be written directly in the Stream's encoding will be
%		written using character data entities from the DTD if at
%		all possible, otherwise as numeric character references.
%		Note that the DTD will NOT be written out at all; as yet
%		there is no way to write out an internal subset,  though
%		it would not be hard to add one.
%
%		* doctype(DocType)
%		Document type for the SGML document type declaration.
%		If omitted it is taken from the root element.  There is
%		never any point in having this be disagree with the
%		root element.  A <!DOCTYPE> declaration will be written
%		if and only if at least one of doctype(_), public(_), or
%		system(_) is provided in Options.
%
%		* public(PubId)
%		The public identifier to be written in the <!DOCTYPE> line.
%
%		* system(SysId)
%		The system identifier to be written in the <!DOCTYPE> line. 
%		
%		* header(Bool)
%		If Bool is 'false', do not emit the <xml ...> header
%		line.  (xml_write/3 only)
%		
%		* nsmap([Id=URI ...])
%		When emitting embedded XML, assume these namespaces
%		are already defined from the environment.  (xml_write/3
%		only).
%		
%		* indent(Indent)
%		Indentation of the document (for embedding)
%	
%		* layout(Bool)
%		Emit/do not emit layout characters to make output
%		readable.
%	
%		* net(Bool)
%		Use/do not use Null End Tags.
%		For XML, this applies only to empty elements, so you get
%		    <foo/>	(default, net(true))
%		    <foo></foo>	(net(false))
%		For SGML, this applies to empty elements, so you get
%		    <foo>	(if foo is declared to be EMPTY in the DTD)
%		    <foo></foo>	(default, net(false))
%		    <foo//	(net(true))
%		and also to elements with character content not containing /
%		    <b>xxx</b>	(default, net(false))
%		    <b/xxx/	(net(true)).
%
%	Note that if the stream is UTF-8, the system will write special
%	characters as UTF-8 sequences, while if it is ISO Latin-1 it
%	will use (character) entities if there is a DTD that provides
%	them, otherwise it will use numeric character references.

xml_write(Data, Options) :-
	current_output(Stream),
	xml_write(Stream, Data, Options).

xml_write(Stream0, Data, Options) :-
	fix_user_stream(Stream0, Stream),
	(   stream_property(Stream, encoding(text))
	->  set_stream(Stream, encoding(utf8)),
	    call_cleanup(xml_write(Stream, Data, Options),
			 set_stream(Stream, encoding(text)))
	;   new_state(xml, State),
	    init_state(Options, State),
	    get_state(State, nsmap, NSMap),
	    add_missing_namespaces(Data, NSMap, Data1),
	    emit_xml_encoding(Stream, Options),
	    emit_doctype(Options, Data, Stream),
	    write_initial_indent(State, Stream),
	    emit(Data1, Stream, State)
	).


sgml_write(Data, Options) :-
	current_output(Stream),
	sgml_write(Stream, Data, Options).

sgml_write(Stream0, Data, Options) :-
	fix_user_stream(Stream0, Stream),
	(   stream_property(Stream, encoding(text))
	->  set_stream(Stream, encoding(utf8)),
	    call_cleanup(sgml_write(Stream, Data, Options),
			 set_stream(Stream, encoding(text)))
	;   new_state(sgml, State),
	    init_state(Options, State),
	    write_initial_indent(State, Stream),
	    emit_doctype(Options, Data, Stream),
	    emit(Data, Stream, State)
	).


html_write(Data, Options) :-
	current_output(Stream),
	html_write(Stream, Data, Options).

html_write(Stream, Data, Options) :-
	sgml_write(Stream, Data,
		   [ dtd(html)
		   | Options
		   ]).

fix_user_stream(user, user_output) :- !.
fix_user_stream(Stream, Stream).


init_state([], _).
init_state([H|T], State) :-
	update_state(H, State),
	init_state(T, State).

update_state(dtd(DTD), State) :- !,
	(   atom(DTD)
	->  dtd(DTD, DTDObj)
	;   DTDObj = DTD
	),
	set_state(State, dtd, DTDObj),
	dtd_character_entities(DTDObj, EntityMap),
	set_state(State, entity_map, EntityMap).
update_state(nsmap(Map), State) :- !,
	set_state(State, nsmap, Map).
update_state(indent(Indent), State) :- !,
	must_be(Indent, integer),
	set_state(State, indent, Indent).
update_state(layout(Bool), State) :- !,
	must_be(Bool, bool),
	set_state(State, layout, Bool).
update_state(doctype(_), _) :- !.
update_state(public(_),  _) :- !.
update_state(system(_),  _) :- !.
update_state(net(Bool), State) :- !,
	must_be(Bool, bool),
	set_state(State, net, Bool).
update_state(header(Bool), _) :- !,
	must_be(Bool, bool).
update_state(Option, _) :-
	throw(error(domain_error(xml_write_option, Option), _)).

must_be(Arg, Type) :-
	call(Type, Arg), !.
must_be(Arg, Type) :-
	throw(error(type_error(Type, Arg), _)).

bool(true).
bool(false).


%	emit_xml_encoding(+Stream, +Options)
%	
%	Emit the XML fileheader with   encoding information. Setting the
%	right encoding on the output stream  must be done before calling
%	xml_write/3.

emit_xml_encoding(Out, Options) :-
	option(header(Hdr), Options, true),
	Hdr == true, !,
	stream_property(Out, encoding(Encoding)),
	(   Encoding == utf8
	->  format(Out, '<?xml version="1.0" encoding="UTF-8"?>~n~n', [])
	;   Encoding == iso_latin_1
	->  format(Out, '<?xml version="1.0" encoding="ISO-8859-1"?>~n~n', [])
	;   throw(error(domain_error(xml_encoding, Encoding), _))
	).
emit_xml_encoding(_, _).


%	emit_doctype(+Options, +Data, +Stream)
%	
%	Emit the document-type declaration.
%	There is a problem with the first clause if we are emitting SGML:
%	the SGML DTDs for HTML up to HTML 4 *do not allow* any 'version'
%	attribute; so the only time this is useful is when it is illegal!

emit_doctype(_Options, Data, Out) :-
	(   memberchk(element(html,Att,_), Data)
	;   Data = element(html,Att,_)
	),
	memberchk(version=Version, Att),
	!,
	format(Out, '<!DOCTYPE HTML PUBLIC "~w">~n~n', [Version]).
emit_doctype(Options, Data, Out) :-
	(   memberchk(public(PubId), Options) -> true
	;   PubId = (-)
	),
	(   memberchk(system(SysId), Options) -> true
	;   SysId = (-)
	),
	\+ (PubId == (-),
	    SysId == (-),
	    \+ memberchk(doctype(_), Options)
	),
	(   Data  =   element(DocType,_,_)
	;   memberchk(element(DocType,_,_), Data)
	;   memberchk(doctype(DocType), Options)
	),
	!,
	write_doctype(Out, DocType, PubId, SysId).
emit_doctype(_, _, _).

write_doctype(Out, DocType, -, -) :- !,
	format(Out, '<!DOCTYPE ~w []>~n~n', [DocType]).
write_doctype(Out, DocType, -, SysId) :- !,
	format(Out, '<!DOCTYPE ~w SYSTEM "~w">~n~n', [DocType,SysId]).
write_doctype(Out, DocType, PubId, -) :- !,
	format(Out, '<!DOCTYPE ~w PUBLIC "~w">~n~n', [DocType,PubId]).
write_doctype(Out, DocType, PubId, SysId) :-
	format(Out, '<!DOCTYPE ~w PUBLIC "~w" "~w">~n~n', [DocType,PubId,SysId]).


%	emit(+Element, +Out, +State, +Options)
%	
%	Emit a single element

emit([], _, _) :- !.
emit([H|T], Out, State) :- !,
	emit(H, Out, State),
	emit(T, Out, State).
emit(Element, Out, State) :-
	\+ \+ emit_element(Element, Out, State).

emit_element(pi(PI), Out, State) :-
	get_state(State, entity_map, EntityMap),
	write(Out, <?),
	write_quoted(Out, PI, "", EntityMap),
	(   get_state(State, dialect, xml) ->
	    write(Out, ?>)
	;   write(Out, >)
	).
emit_element(element(Name, Attributes, Content), Out, State) :-
	att_length(Attributes, State, Alen),
	(   Alen > 60,
	    get_state(State, layout, true)
	->  Sep = nl,
	    AttIndent = 4
	;   Sep = sp,
	    AttIndent = 0
	),
	(   get_state(State, dialect, xml)
	->  update_nsmap(Attributes, State)
	;   true
	),
	put_char(Out, '<'),
	emit_name(Name, Out, State),
	(   AttIndent > 0
	->  \+ \+ ( inc_indent(State, AttIndent),
	            attributes(Attributes, Sep, Out, State)
		  )
	;   attributes(Attributes, Sep, Out, State)
	),
	content(Content, Out, Name, State).

attributes([], _, _, _).
attributes([H|T], Sep, Out, State) :-
	(   Sep == nl
	->  write_indent(State, Out)
	;   put_char(Out, ' ')
	),
	attribute(H, Out, State),
	attributes(T, Sep, Out, State).

attribute(Name=Value, Out, State) :-
	emit_name(Name, Out, State),
	put_char(Out, =),
	sgml_write_attribute(Out, Value, State).

att_length(Atts, State, Len) :-
	att_length(Atts, State, 0, Len).

att_length([], _, Len, Len).
att_length([A0|T], State, Len0, Len) :-
	alen(A0, State, AL),
	Len1 is Len0 + 1 + AL,
	att_length(T, State, Len1, Len).

alen(URI:Name=Value, State, Len) :- !,
	atom_length(Value, AL),
	atom_length(Name, NL),
	get_state(State, nsmap, Nsmap),
	(   memberchk(NS=URI, Nsmap)
	->  atom_length(NS, NsL)
	;   atom_length(URI, NsL)
	),
	Len is AL+NL+NsL+3.
alen(Name=Value, _, Len) :-
	atom_length(Name, NL),
	atom_length(Value, AL),
	Len is AL+NL+3.

emit_name(Name, Out, _) :-
	atom(Name), !,
	write(Out, Name).
emit_name(URI:Name, Out, State) :-
	get_state(State, nsmap, NSMap),
	memberchk(NS=URI, NSMap), !,
	(   NS == []
	->  write(Out, Name)
	;   format(Out, '~w:~w', [NS, Name])
	).
emit_name(Term, Out, _) :-
	write(Out, Term).

%	update_nsmap(+Attributes, !State)
%	
%	Modify the nsmap of State to reflect modifications due to xmlns
%	arguments.

update_nsmap(Attributes, State) :-
	get_state(State, nsmap, Map0),
	update_nsmap(Attributes, Map0, Map),
	set_state(State, nsmap, Map).

update_nsmap([], Map, Map).
update_nsmap([xmlns:NS=URI|T], Map0, Map) :- !,
	set_nsmap(NS, URI, Map0, Map1),
	update_nsmap(T, Map1, Map).
update_nsmap([xmlns=URI|T], Map0, Map) :- !,
	set_nsmap([], URI, Map0, Map1),
	update_nsmap(T, Map1, Map).
update_nsmap([_|T], Map0, Map) :- !,
	update_nsmap(T, Map0, Map).

set_nsmap(NS, URI, Map0, Map) :-
	select(NS=_, Map0, Map1), !,
	Map = [NS=URI|Map1].
set_nsmap(NS, URI, Map, [NS=URI|Map]).


%	content(+Content, +Out, +Element, +State, +Options)
%	
%	Emit the content part of a structure  as well as the termination
%	for the content. For empty content   we have three versions: XML
%	style '/>', SGML declared EMPTY element (nothing) or normal SGML
%	element (we must close with the same element name).

content([], Out, Element, State) :- !,	% empty element
    (   get_state(State, net, true)
    ->  (   get_state(State, dialect, xml) ->
            write(Out, />)
	;   empty_element(State, Element) ->
	    write(Out, >)
	;   write(Out, //)
	)
    ;/* get_state(State, net, false) */
	write(Out, >),
	(   get_state(State, dialect, sgml),
	    empty_element(State, Element)
	->  true
	;   emit_close(Element, Out, State)
	)
    ).
content([Atom], Out, Element, State) :-
	atom(Atom), !,
	(   get_state(State, dialect, sgml),
	    get_state(State, net, true),
	    \+ sub_atom(Atom, _, _, _, /),
	    atom_length(Atom, Len),
	    Len < 20
	->  write(Out, /),
	    sgml_write_content(Out, Atom, State),
	    write(Out, /)
	;/* XML or not NET */
	    write(Out, >),
	    sgml_write_content(Out, Atom, State),
	    emit_close(Element, Out, State)
	).
content(Content, Out, Element, State) :-
	get_state(State, layout, true),
	/* If xml:space='preserve' is present, */
	/* we MUST NOT tamper with white space at all. */
	\+ (Element = element(_,Atts,_),
	    memberchk('xml:space'=preserve, Atts)
	),
	element_content(Content, Elements),
	!,
	format(Out, >, []),
	\+ \+ (
	    inc_indent(State),
	    write_element_content(Elements, Out, State)
	),
	write_indent(State, Out),
	emit_close(Element, Out, State).
content(Content, Out, Element, State) :-
	format(Out, >, []),
	write_mixed_content(Content, Out, Element, State),
	emit_close(Element, Out, State).


emit_close(Element, Out, State) :-
	write(Out, '</'),
	emit_name(Element, Out, State),
	write(Out, '>').


write_mixed_content([], _, _, _).
write_mixed_content([H|T], Out, Element, State) :-
	write_mixed_content_element(H, Out, State),
	write_mixed_content(T, Out, Element, State).

write_mixed_content_element(H, Out, State) :-
	(   atom(H)
	->  sgml_write_content(Out, H, State)
	;   functor(H, element, 3)
	->  emit(H, Out, State)
	;   functor(H, pi, 1)
	->  emit(H, Out, State)
	;   H = sdata(Data)		% cannot be written without entity!
	->  print_message(warning, sgml_write(sdata_as_cdata(Data))),
	    sgml_write_content(Out, Data, State)
	;   assertion(fail)
	).


element_content([], []).
element_content([element(Name,Atts,C)|T0], [element(Name,Atts,C)|T]) :- !,
	element_content(T0, T).
element_content([Blank|T0], T) :-
	atom(Blank),
	atom_codes(Blank, Codes),
	all_blanks(Codes),
	element_content(T0, T).

all_blanks([]).
all_blanks([H|T]) :-
	code_type(H, space),
	all_blanks(T).

write_element_content([], _, _).
write_element_content([H|T], Out, State) :-
	write_indent(State, Out),
	emit(H, Out, State),
	write_element_content(T, Out, State).


		 /*******************************
		 *	     NAMESPACES		*
		 *******************************/

%	add_missing_namespaces(+DOM0, +NsMap, -DOM)
%	
%	Add xmlns:NS=URI definitions to the toplevel element(s) to
%	deal with missing namespaces.

add_missing_namespaces([], _, []) :- !.
add_missing_namespaces([H0|T0], Def, [H|T]) :- !,
	add_missing_namespaces(H0, Def, H),
	add_missing_namespaces(T0, Def, T).
add_missing_namespaces(Elem0, Def, Elem) :-
	Elem0 = element(Name, Atts0, Content), !,
	missing_namespaces(Elem0, Def, Missing),
	(   Missing == []
	->  Elem = Elem0
	;   add_missing_ns(Missing, Atts0, Atts),
	    Elem = element(Name, Atts, Content)
	).
add_missing_namespaces(DOM, _, DOM).	% CDATA, etc.

add_missing_ns([], Atts, Atts).
add_missing_ns([H|T], Atts0, Atts) :-
	generate_ns(H, NS),
	add_missing_ns(T, [xmlns:NS=H|Atts0], Atts).

generate_ns(URI, NS) :-
	default_ns(URI, NS), !.
generate_ns(_, NS) :-
	gensym(xns, NS).

default_ns('http://www.w3.org/2001/XMLSchema-instance', xsi).

%	missing_namespaces(+DOM, +NSMap, -Missing)
%	
%	Return a list of URIs appearing in DOM that are not covered
%	by xmlns definitions.

missing_namespaces(DOM, Defined, Missing) :-
	missing_namespaces(DOM, Defined, [], Missing).

missing_namespaces([], _, L, L) :- !.
missing_namespaces([H|T], Def, L0, L) :- !,
	missing_namespaces(H, Def, L0, L1),
	missing_namespaces(T, Def, L1, L).
missing_namespaces(element(Name, Atts, Content), Def, L0, L) :- !,
	update_nsmap(Atts, Def, Def1),
	missing_ns(Name, Def1, L0, L1),
	missing_att_ns(Atts, Def1, L1, L2),
	missing_namespaces(Content, Def1, L2, L).
missing_namespaces(_, _, L, L).

missing_att_ns([], _, M, M).
missing_att_ns([Name=_|T], Def, M0, M) :-
	missing_ns(Name, Def, M0, M1),
	missing_att_ns(T, Def, M1, M).

missing_ns(URI:_, Def, M0, M) :- !,
	(   (   memberchk(_=URI, Def)
	    ;	memberchk(URI, M0)
	    ;	URI = xml		% predefined ones
	    ;	URI = xmlns
	    )
	->  M = M0
	;   M = [URI|M0]
	).
missing_ns(_, _, M, M).

		 /*******************************
		 *	   QUOTED WRITE		*
		 *******************************/

sgml_write_attribute(Out, Values, State) :-
	is_list(Values), !,
	get_state(State, entity_map, EntityMap),
	put_char(Out, '"'),
	write_quoted_list(Values, Out, """&", EntityMap),
	put_char(Out, '"').
sgml_write_attribute(Out, Value, State) :-
	get_state(State, entity_map, EntityMap),
	put_char(Out, '"'),
	write_quoted(Out, Value, """&", EntityMap),
	put_char(Out, '"').

write_quoted_list([], _, _, _).
write_quoted_list([H|T], Out, Escape, EntityMap) :-
	write_quoted(Out, H, Escape, EntityMap),
	(   T == []
	->  true
	;   put_char(Out, ' '),
	    write_quoted_list(T, Out, Escape, EntityMap)
	).


sgml_write_content(Out, Value, State) :-
	get_state(State, entity_map, EntityMap),
	write_quoted(Out, Value, "<&>", EntityMap).


write_quoted(Out, Atom, Escape, EntityMap) :-
	atom_codes(Atom, Codes),
	writeq(Codes, Out, Escape, EntityMap).


writeq([], _, _, _).
writeq([H|T], Out, Escape, EntityMap) :-
	(   memberchk(H, Escape)
	->  write_entity(H, Out, EntityMap)
	;   H >= 256
	->  (   stream_property(Out, encoding(utf8))
	    ->	put_code(Out, H)
	    ;	write_entity(H, Out, EntityMap)
	    )
	;   put_code(Out, H)
	),
	writeq(T, Out, Escape, EntityMap).


write_entity(Code, Out, EntityMap) :-
	(   get_assoc(Code, EntityMap, EntityName)
	->  format(Out, '&~w;', [EntityName])
	;   format(Out, '&#~w;', [Code])
	).


		 /*******************************
		 *	    INDENTATION		*
		 *******************************/

write_initial_indent(State, Out) :-
	(   get_state(State, indent, Indent),
	    Indent > 0
	->  emit_indent(Indent, Out)
	;   true
	).

write_indent(State, _) :-
	get_state(State, layout, false), !.
write_indent(State, Out) :-
	get_state(State, indent, Indent),
	emit_indent(Indent, Out).

emit_indent(Indent, Out) :-
	Tabs is Indent // 8,
	Spaces is Indent mod 8,
	format(Out, '~N', []),
	write_n(Tabs, '\t', Out),
	write_n(Spaces, ' ', Out).
	
write_n(N, Char, Out) :-
	(   N > 0
	->  put_char(Out, Char),
	    N2 is N - 1,
	    write_n(N2, Char, Out)
	;   true
	).
	
inc_indent(State) :-
	inc_indent(State, 2).

inc_indent(State, Inc) :-
	state(indent, Arg),
	arg(Arg, State, I0),
	I is I0 + Inc,
	setarg(Arg, State, I).


		 /*******************************
		 *	   DTD HANDLING		*
		 *******************************/

%	empty_element(+State, +Element)
%	
%	True if Element is declared  with   EMPTY  content in the (SGML)
%	DTD.

empty_element(State, Element) :-
	get_state(State, dtd, DTD),
	DTD \== (-),
	dtd_property(DTD, element(Element, _, empty)).
	
%	dtd_character_entities(+DTD, -Map)
%	
%	Return an assoc mapping character entities   to their name. Note
%	that the entity representation is a bit dubious. Entities should
%	allow for a wide-character version and avoid the &#..; trick.

dtd_character_entities(DTD, Map) :-
	empty_assoc(Empty),
	dtd_property(DTD, entities(Entities)),
	fill_entity_map(Entities, DTD, Empty, Map).

fill_entity_map([], _, Map, Map).
fill_entity_map([H|T], DTD, Map0, Map) :-
	(   dtd_property(DTD, entity(H, CharEntity)),
	    atom(CharEntity),
	    (	sub_atom(CharEntity, 0, _, _, '&#'),
		sub_atom(CharEntity, _, _, 0, ';')
	    ->  sub_atom(CharEntity, 2, _, 1, Name),
		atom_number(Name, Code)
	    ;	atom_length(CharEntity, 1),
		char_code(CharEntity, Code)
	    )
	->  put_assoc(Code, Map0, H, Map1),
	    fill_entity_map(T, DTD, Map1, Map)
	;   fill_entity_map(T, DTD, Map0, Map)
	).



		 /*******************************
		 *	      FIELDS		*
		 *******************************/

state(indent,     1).			% current indentation
state(layout,	  2).			% use layout (true/false)
state(dtd,        3).			% DTD for entity names
state(entity_map, 4).			% compiled entity-map
state(dialect,	  5).			% xml/sgml
state(nsmap,	  6).			% defined namespaces
state(net,	  7).			% Should null end-tags be used?

new_state(Dialect,
    state(
	0,		% indent
	true,		% layout
	-,		% DTD
	EntityMap,	% entity_map
	Dialect,	% dialect
	[],		% NS=Full map
	Net		% Null End-Tags?
    )) :-
	( Dialect == sgml -> Net = false ; Net = true ),
	empty_assoc(EntityMap).

get_state(State, Field, Value) :-
	state(Field, Arg),
	arg(Arg, State, Value).

set_state(State, Field, Value) :-
	state(Field, Arg),
	setarg(Arg, State, Value).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(sgml_write(sdata_as_cdata(Data))) -->
	[ 'SGML-write: emitting SDATA as CDATA: "~p"'-[Data] ].
