/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2002-2007, University of Amsterdam

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


:- module(rdf,
	  [ load_rdf/2,			% +File, -Triples
	    load_rdf/3,			% +File, -Triples, :Options
	    xml_to_rdf/3,		% +XML, -Triples, +Options
	    process_rdf/3		% +File, :OnTriples, :Options
	  ]).

:- meta_predicate
	load_rdf(+, -, :),
	process_rdf(+, :, :).

:- use_module(library(sgml)).		% Basic XML loading
:- use_module(library(option)).		% option/3
:- use_module(library(lists)).
:- use_module(rdf_parser).		% Basic parser
:- use_module(rdf_triple).		% Generate triples

%%	load_rdf(+File, -Triples) is det.
%%	load_rdf(+File, -Triples, :Options) is det.
%
%	Parse an XML file holding an RDF term into a list of RDF triples.
%	see rdf_triple.pl for a definition of the output format. Options:
%
%		* base_uri(+URI)
%		URI to use as base
%
%		* expand_foreach(+Bool)
%		Apply each(Container, Pred, Object) on the members of
%		Container
%
%		* namespaces(-Namespaces:list(NS=URL))
%		Return list of namespaces declared using xmlns:NS=URL in
%		the document.  This can be used to update the namespace
%		list with rdf_register_ns/2.
%
%	@see	Use process_rdf/3 for processing large documents in
%		_|call-back|_ style.

load_rdf(File, Triples) :-
	load_rdf(File, Triples, []).

load_rdf(File, Triples, M:Options0) :-
	entity_options(Options0, EntOptions, Options1),
	meta_options(load_meta_option, M:Options1, Options),
	init_ns_collect(Options, NSList),
	load_structure(File,
		       [ RDFElement
		       ],
		       [ dialect(xmlns),
			 space(sgml),
			 call(xmlns, rdf:on_xmlns)
		       | EntOptions
		       ]),
	rdf_start_file(Options, Cleanup),
	call_cleanup(xml_to_rdf(RDFElement, Triples0, Options),
		     rdf_end_file(Cleanup)),
	exit_ns_collect(NSList),
	post_process(Options, Triples0, Triples).

entity_options([], [], []).
entity_options([H|T0], Entities, Rest) :-
	(   H = entity(_,_)
	->  Entities = [H|ET],
	    entity_options(T0, ET, Rest)
	;   Rest = [H|RT],
	    entity_options(T0, Entities, RT)
	).

load_meta_option(convert_typed_literal).

%%	xml_to_rdf(+XML, -Triples, +Options)

xml_to_rdf(XML, Triples, Options) :-
	is_list(Options), !,
	make_rdf_state(Options, State, _),
	xml_to_plrdf(XML, RDF, State),
	rdf_triples(RDF, Triples).
xml_to_rdf(XML, BaseURI, Triples) :-
	atom(BaseURI), !,
	xml_to_rdf(XML, Triples, [base_uri(BaseURI)]).


		 /*******************************
		 *	 POST-PROCESSING	*
		 *******************************/

post_process([], Triples, Triples).
post_process([expand_foreach(true)|T], Triples0, Triples) :- !,
	expand_each(Triples0, Triples1),
	post_process(T, Triples1, Triples).
post_process([_|T], Triples0, Triples) :- !,
	post_process(T, Triples0, Triples).


		 /*******************************
		 *	      EXPAND		*
		 *******************************/

expand_each(Triples0, Triples) :-
	select(rdf(each(Container), Pred, Object),
	       Triples0, Triples1), !,
	each_triples(Triples1, Container, Pred, Object, Triples2),
	expand_each(Triples2, Triples).
expand_each(Triples, Triples).

each_triples([], _, _, _, []).
each_triples([H0|T0], Container, P, O,
	     [H0, rdf(S,P,O)|T]) :-
	H0 = rdf(Container, rdf:A, S),
	member_attribute(A), !,
	each_triples(T0, Container, P, O, T).
each_triples([H|T0], Container, P, O, [H|T]) :-
	each_triples(T0, Container, P, O, T).

member_attribute(A) :-
	sub_atom(A, 0, _, _, '_').	% must check number?


		 /*******************************
		 *	     BIG FILES		*
		 *******************************/

%%	process_rdf(+Input, :OnObject, :Options)
%
%	Process RDF from Input. Input is either an atom or a term of the
%	format stream(Handle). For each   encountered  description, call
%	OnObject(+Triples) to handle the  triples   resulting  from  the
%	description. Defined Options are:
%
%		* base_uri(+URI)
%		Determines the reference URI.
%
%		* db(DB)
%		When loading from a stream, the source is taken from
%		this option or -if non-existent- from base_uri.
%
%		* lang(LanguageID)
%		Set initial language (as xml:lang)
%
%		* convert_typed_literal(:Convertor)
%		Call Convertor(+Type, +Content, -RDFObject) to create
%		a triple rdf(S, P, RDFObject) instead of rdf(S, P,
%		literal(type(Type, Content)).
%
%		*  namespaces(-Namespaces:list(NS=URL))
%		Return list of namespaces declared using xmlns:NS=URL in
%		the document.  This can be used to update the namespace
%		list with rdf_register_ns/2.
%
%		* entity(Name, Value)
%		Overrule entity values found in the file
%
%		* embedded(Boolean)
%		If =true=, do not give warnings if rdf:RDF is embedded
%		in other XML data.

process_rdf(File, OnObject, M:Options0) :-
	is_list(Options0), !,
	entity_options(Options0, EntOptions, Options1),
	meta_options(load_meta_option, M:Options1, Options2),
	process_options(Options2, ProcessOptions, Options),
	option(base_uri(BaseURI), Options, ''),
	rdf_start_file(Options, Cleanup),
	strip_module(OnObject, Module, Pred),
	nb_setval(rdf_object_handler, Module:Pred),
	nb_setval(rdf_options, Options),
	nb_setval(rdf_state, -),
	init_ns_collect(Options, NSList),
	(   File = stream(In)
	->  Source = BaseURI
	;   is_stream(File)
	->  In = File,
	    option(db(Source), Options, BaseURI)
	;   open(File, read, In, [type(binary)]),
	    Close = In,
	    Source = File
	),
	new_sgml_parser(Parser, [dtd(DTD)]),
	def_entities(EntOptions, DTD),
	set_sgml_parser(Parser, file(Source)),
	set_sgml_parser(Parser, dialect(xmlns)),
	set_sgml_parser(Parser, space(sgml)),
	do_process_rdf(Parser, In, NSList, Close, Cleanup, ProcessOptions).
process_rdf(File, BaseURI, OnObject) :-
	process_rdf(File, OnObject, [base_uri(BaseURI)]).

def_entities([], _).
def_entities([entity(Name, Value)|T], DTD) :- !,
	def_entity(DTD, Name, Value),
	def_entities(T, DTD).
def_entities([_|T0], DTD) :-
	def_entities(T0, DTD).

def_entity(DTD, Name, Value) :-
	open_dtd(DTD, [], Stream),
	xml_quote_attribute(Value, QValue),
	format(Stream, '<!ENTITY ~w "~w">~n', [Name, QValue]),
	close(Stream).


do_process_rdf(Parser, In, NSList, Close, Cleanup, Options) :-
	call_cleanup((   sgml_parse(Parser,
				    [ source(In),
				      call(begin, rdf:on_begin),
				      call(xmlns, rdf:on_xmlns)
				    | Options
				    ]),
			 exit_ns_collect(NSList)
		     ),
		     cleanup_process(Close, Cleanup, Parser)).

cleanup_process(In, Cleanup, Parser) :-
	(   var(In)
	->  true
	;   close(In)
	),
	free_sgml_parser(Parser),
	nb_delete(rdf_options),
	nb_delete(rdf_object_handler),
	nb_delete(rdf_state),
	nb_delete(rdf_nslist),
	rdf_end_file(Cleanup).

on_begin(NS:'RDF', Attr, _) :-
	rdf_name_space(NS), !,
	nb_getval(rdf_options, Options),
	make_rdf_state(Options, State0, _),
	rdf_modify_state(Attr, State0, State),
	nb_setval(rdf_state, State).
on_begin(Tag, Attr, Parser) :-
	nb_getval(rdf_state, State),
	(   State == (-)
	->  nb_getval(rdf_options, RdfOptions),
	    (	memberchk(embedded(true), RdfOptions)
	    ->	true
	    ;	print_message(warning, rdf(unexpected(Tag, Parser)))
	    )
	;   get_sgml_parser(Parser, line(Start)),
	    get_sgml_parser(Parser, file(File)),
	    sgml_parse(Parser,
		       [ document(Content),
			 parse(content)
		       ]),
	    nb_getval(rdf_object_handler, OnTriples),
	    element_to_plrdf(element(Tag, Attr, Content), Objects, State),
	    rdf_triples(Objects, Triples),
	    call(OnTriples, Triples, File:Start)
	).

%%	on_xmlns(+NS, +URL, +Parser)
%
%	Build up the list of   encountered xmlns:NS=URL declarations. We
%	use  destructive  assignment  here   as    an   alternative   to
%	assert/retract, ensuring thread-safety and better performance.

on_xmlns(NS, URL, _Parser) :-
	(   nb_getval(rdf_nslist, List),
	    List = list(L0)
	->  nb_linkarg(1, List, [NS=URL|L0])
	;   true
	).

init_ns_collect(Options, NSList) :-
	(   option(namespaces(NSList), Options, -),
	    NSList \== (-)
	->  nb_setval(rdf_nslist, list([]))
	;   nb_setval(rdf_nslist, -),
	    NSList = (-)
	).

exit_ns_collect(NSList) :-
	(   NSList == (-)
	->  true
	;   nb_getval(rdf_nslist, list(NSList))
	).


process_options(Options, Process, RestOptions) :-
	select_option(content_length(Len), Options, RestOptions), !,
	Process = [content_length(Len)].
process_options(Options, [], Options).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

%	Catch messages.  sgml/4 is generated by the SGML2PL binding.

prolog:message(rdf(unparsed(Data))) -->
	{ phrase(unparse_xml(Data), XML)
	},
	[ 'RDF: Failed to interpret "~s"'-[XML] ].
prolog:message(rdf(shared_blank_nodes(N))) -->
	[ 'RDF: Shared ~D blank nodes'-[N] ].
prolog:message(rdf(not_a_name(Name))) -->
	[ 'RDF: argument to rdf:ID is not an XML name: ~p'-[Name] ].
prolog:message(rdf(redefined_id(Id))) -->
	[ 'RDF: rdf:ID ~p: multiple definitions'-[Id] ].
prolog:message(rdf(unexpected(Tag, Parser))) -->
	{ get_sgml_parser(Parser, file(File)),
	  get_sgml_parser(Parser, line(Line))
	},
	[ 'RDF: ~w:~d: Unexpected element ~w'-[File, Line, Tag] ].


		 /*******************************
		 *	    XML-TO-TEXT		*
		 *******************************/

unparse_xml([]) --> !,
	[].
unparse_xml([H|T]) --> !,
	unparse_xml(H),
	unparse_xml(T).
unparse_xml(Atom) -->
	{ atom(Atom)
	}, !,
	atom(Atom).
unparse_xml(element(Name, Attr, Content)) -->
	"<",
	identifier(Name),
	attributes(Attr),
	(   { Content == []
	    }
	->  "/>"
	;   ">",
	    unparse_xml(Content)
	).

attributes([]) -->
	[].
attributes([H|T]) -->
	attribute(H),
	attributes(T).

attribute(Name=Value) -->
	" ",
	identifier(Name),
	"=",
	value(Value).

identifier(NS:Local) --> !,
	"{", atom(NS), "}",
	atom(Local).
identifier(Local) -->
	atom(Local).

atom(Atom, Text, Rest) :-
	atom_codes(Atom, Chars),
	append(Chars, Rest, Text).

value(Value) -->
	{ atom_codes(Value, Chars)
	},
	"\"",
	quoted(Chars),
	"\"".

quoted([]) -->
	[].
quoted([H|T]) -->
	quote(H), !,
	quoted(T).

quote(0'<) --> "&lt;".
quote(0'>) --> "&gt;".
quote(0'") --> "&quot;".
quote(0'&) --> "&amp;".
quote(X)   --> [X].


		 /*******************************
		 *	       XREF		*
		 *******************************/

:- multifile prolog:meta_goal/2.
prolog:meta_goal(process_rdf(_,G,_), [G+2]).
