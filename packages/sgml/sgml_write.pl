/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
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
	  [ sgml_write/3,		% +Stream, +Data, +Options
	    xml_write/3,		% +Stream, +Data, +Options
	    html_write/3		% +Stream, +Data, +Options
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

The current implementation is  particulary   keen  on  getting character
encoding and the use of character  entities   right.  Some work has been
done providing layout, but space-handling in XML   and SGML make this is
very hazerdous area.

The Prolog-based low-level character and  escape   handling  is the real
bottleneck in this library and will probably be   moved  to C in a later
stage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	xml_write(+Stream, +Data, +Options)
%	sgml_write(+Stream, +Data, +Options)
%	
%	Write a term is created by the SGML/XML parser to a stream in
%	SGML or XML format.  Options:
%	
%		* dtd(DTD)
%		The DTD. Needed for SGML documents that contain elements
%		with empty content model.
%
%		* doctype(DOCTYPE)
%		Doctype for the SGML documentype declaration. If omitted
%		it is taken from the first element.
%	
%	Note that if the stream is UTF-8,  the system will write special
%	characters as UTF-8 sequences, while  if   it  is ISO Latin-1 it
%	will use (character) entities.

xml_write(Stream, Data, Options) :-
	new_state(State),
	set_state(State, dialect, xml),
	init_state(Options, State),
	emit_xml_encoding(Stream, Options),
	emit(Data, Stream, State).


sgml_write(Stream, Data, Options) :-
	new_state(State),
	set_state(State, dialect, sgml),
	init_state(Options, State),
	emit_doctype(Options, Data, Stream),
	emit(Data, Stream, State).


html_write(Stream, Data, Options) :-
	sgml_write(Stream, Data,
		   [ dtd(html)
		   | Options
		   ]).


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
update_state(doctype(_), _) :- !.
update_state(header(_), _) :- !.
update_state(Option, _) :-
	throw(error(domain_error(xml_write_option, Option), _)).


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
%	Emit the document-type declaration.  We also need a way to add
%	public and/or system identifiers here.

emit_doctype(_Options, Data, Out) :-
	(   Data = [element(html,Att,_)]
	;   Data = element(html,Att,_)
	),
	memberchk(version=Version, Att), !,
	format(Out, '<!DOCTYPE HTML PUBLIC "~w">~n~n', [Version]).
emit_doctype(Options, Data, Out) :-
	(   memberchk(doctype(Doctype), Options)
	;   Data = [element(Doctype,_,_)]
	;   Data = element(Doctype)
	), !,
	upcase_atom(Doctype, DOCTYPE),
	format(Out, '<!DOCTYPE ~w []>~n~n', [DOCTYPE]).
emit_doctype(_, _, _).


%	emit(+Element, +Out, +State, +Options)
%	
%	Emit a single element

emit([], _, _) :- !.
emit([H|T], Out, State) :- !,
	emit(H, Out, State),
	emit(T, Out, State).
emit(Element, Out, State) :-
	\+ \+ emit_element(Element, Out, State).

emit_element(element(Name, Attributes, Content), Out, State) :-
	att_length(Attributes, State, Alen),
	(   Alen > 60
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
	(   get_state(State, dialect, xml)
	->  format(Out, '/>', [])
	;   write(Out, '>'),
	    (   empty_element(State, Element)
	    ->	true
	    ;	emit_close(Element, Out, State)
	    )
	).
content([Atom], Out, Element, State) :-
	atom(Atom), !,
	format(Out, '>', []),
	sgml_write_content(Out, Atom, State),
	emit_close(Element, Out, State).
content(Content, Out, Element, State) :-
	element_content(Content, Elements), !,
	format(Out, '>', []),
	\+ \+ (inc_indent(State),
	       write_element_content(Elements, Out, State)),
	write_indent(State, Out),
	emit_close(Element, Out, State).
content(Content, Out, Element, State) :-
	format(Out, '>', []),
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
	;   H = pi(PI)
	->  format(Out, '<?', []),
	    sgml_write_content(Out, PI, State), % Is this ok?
	    format(Out, '>', [])
	;   H = sdata(Data)		% cannot be written without entity!
	->  print_message(warning, sgml_write(sdata_as_cdata(Data))),
	    sgml_write_content(Out, Data, State)
	;   assume(fail)
	).


element_content([], []).
element_content([element(Name,Atts,C)|T0], [element(Name,Atts,C)|T]) :- !,
	element_content(T0, T).
element_content([Blank|T0], T) :-
	blank_atom(Blank),
	element_content(T0, T).

blank_atom(Atom) :-
	atom_codes(Atom, Codes),
	all_blanks(Codes).

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
		 *	   QUOTED WRITE		*
		 *******************************/

sgml_write_attribute(Out, Values, State) :-
	is_list(Values), !,
	get_state(State, entity_map, EntityMap),
	put_char(Out, '"'),
	write_quoted_list(Values, Out, "\"&", EntityMap),
	put_char(Out, '"').
sgml_write_attribute(Out, Value, State) :-
	get_state(State, entity_map, EntityMap),
	put_char(Out, '"'),
	write_quoted(Out, Value, "\"&", EntityMap),
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
	    ),
	    put_assoc(Code, Map0, H, Map1),
	    fill_entity_map(T, DTD, Map1, Map)
	;   fill_entity_map(T, DTD, Map0, Map)
	).



		 /*******************************
		 *	      FIELDS		*
		 *******************************/

state(indent,     1).
state(dtd,        2).
state(entity_map, 3).
state(dialect,	  4).
state(nsmap,	  5).

new_state(state(0,			% indent
		-,			% DTD
		EntityMap,		% entity_map
		xml,			% dialect
	        [])) :-			% NS=Full map
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
