/*  $Id$

    Part of SWI-Prolog RDF parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf,
	  [ load_rdf/2,			% +File, -Triples
	    load_rdf/3,			% +File, -Triples, +Options
	    xml_to_rdf/3,		% +XML, +BaseURI, -Triples
	    process_rdf/3		% +File, +BaseURI, :OnTriples
	  ]).

:- meta_predicate(process_rdf(+, :, +)).

:- use_module(library(sgml)).		% Basic XML loading
:- use_module(library(option)).		% option/3
:- use_module(rdf_parser).		% Basic parser
:- use_module(rdf_triple).		% Generate triples

%	load_rdf(+File, -Triples[, +Options])
%
%	Parse an XML file holding an RDF term into a list of RDF triples.
%	see rdf_triple.pl for a definition of the output format. Options:
%
%	# base_uri(URI)
%	URI to use as base
%
%	# expand_foreach(Bool)
%	Apply each(Container, Pred, Object) on the members of Container
%		
%	# namespaces([NS=URL, ...])
%	Return list of namespaces declared using xmlns:NS=URL in
%	the document.  This can be used to update the namespace
%	list with rdf_register_ns/2.

load_rdf(File, Triples) :-
	load_rdf(File, Triples, []).

load_rdf(File, Triples, Options0) :-
	meta_options(Options0, Options),
	init_ns_collect(Options, NSList),
	load_structure(File,
		       [ RDFElement
		       ],
		       [ dialect(xmlns),
			 space(sgml),
			 call(xmlns, rdf:on_xmlns)
		       ]),
	rdf_start_file(Options, Cleanup),
	call_cleanup(xml_to_rdf(RDFElement, Triples0, Options),
		     rdf_end_file(Cleanup)),
	exit_ns_collect(NSList),
	post_process(Options, Triples0, Triples).
	
%	xml_to_rdf(+XML, -Triples, +Options)

xml_to_rdf(XML, Triples, Options) :-
	is_list(Options), !,
	xml_to_plrdf(XML, RDF, Options),
	rdf_triples(RDF, Triples).
xml_to_rdf(XML, BaseURI, Triples) :-
	atom(BaseURI), !,
	xml_to_rdf(XML, Triples, [base_uri(BaseURI)]).


set_anon_prefix([], []) :- !.
set_anon_prefix(BaseURI, [Ref]) :-
	concat_atom(['__', BaseURI, '#'], AnonBase),
	asserta(anon_prefix(AnonBase), Ref).


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

%	process_rdf(+Input, :OnObject, +Options)
%	
%	Process RDF from Input. Input is either an atom or a term of the
%	format stream(Handle). For each   encountered  description, call
%	OnObject(+Triples) to handle the  triples   resulting  from  the
%	description. Defined Options are:
%	
%		# base_uri(+URI)
%		Determines the reference URI.
%		
%		# lang(LanguageID)
%		Set initial language (as xml:lang)
%		
%		# convert_typed_literal(:Convertor)
%		Call Convertor(+Type, +Content, -RDFObject) to create
%		a triple rdf(S, P, RDFObject) instead of rdf(S, P,
%		literal(type(Type, Content)).
%		
%		# namespaces([NS=URL, ...])
%		Return list of namespaces declared using xmlns:NS=URL in
%		the document.  This can be used to update the namespace
%		list with rdf_register_ns/2.

process_rdf(File, OnObject, Options0) :-
	is_list(Options0), !,
	meta_options(Options0, Options),
	option(base_uri(BaseURI), Options, []),
	rdf_start_file(Options, Cleanup),
	strip_module(OnObject, Module, Pred),
	nb_setval(rdf_object_handler, Module:Pred),
	nb_setval(rdf_options, Options),
	nb_setval(rdf_state, -),
	init_ns_collect(Options, NSList),
	(   File = stream(In)
	->  Source = BaseURI
	;   File = '$stream'(_)
	->  In = File,
	    Source = BaseURI
	;   open(File, read, In, [type(binary)]),
	    Close = In,
	    Source = File
	),
	new_sgml_parser(Parser, []),
	set_sgml_parser(Parser, file(Source)),
	set_sgml_parser(Parser, dialect(xmlns)),
	set_sgml_parser(Parser, space(sgml)),
	do_process_rdf(Parser, In, NSList, Close, Cleanup).
process_rdf(File, BaseURI, OnObject) :-
%	print_message(warning,
%		      format('process_rdf(): new argument order', [])),
	process_rdf(File, OnObject, [base_uri(BaseURI)]).


do_process_rdf(Parser, In, NSList, Close, Cleanup) :-
	call_cleanup((   sgml_parse(Parser,
				    [ source(In),
				      call(begin, rdf:on_begin),
				      call(end,   rdf:on_end),
				      call(xmlns, rdf:on_xmlns)
				    ]),
			 exit_ns_collect(NSList)
		     ),
		     cleanup_process(Close, Cleanup)).

cleanup_process(In, Cleanup) :-
	(   var(In)
	->  true
	;   close(In)
	),
	nb_delete(rdf_options),
	nb_delete(rdf_object_handler),
	nb_delete(rdf_state),
	nb_delete(rdf_nslist),
	rdf_end_file(Cleanup).

on_end(NS:'RDF', _) :-
	rdf_name_space(NS),
	nb_setval(rdf_description_options, -).

on_begin(NS:'RDF', Attr, _) :-
	rdf_name_space(NS), !,
	nb_getval(rdf_options, Options0),
	modify_state(Attr, Options0, Options),
	nb_setval(rdf_state, Options).
on_begin(Tag, Attr, Parser) :-
	nb_getval(rdf_state, Options),
	Options \== (-), !,
	get_sgml_parser(Parser, line(Start)),
	get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(Content),
		     parse(content)
		   ]),
	nb_getval(rdf_object_handler, OnTriples),
	element_to_plrdf(element(Tag, Attr, Content), Objects, Options),
	rdf_triples(Objects, Triples),
	call(OnTriples, Triples, File:Start).

%	on_xmlns(+NS, +URL, +Parser)
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

modify_state([], Options, Options).
modify_state([H|T], Options0, Options) :-
	modify_state1(H, Options0, Options1),
	modify_state(T, Options1, Options).

modify_state1(xml:base = Base, Options0, Options) :- !,
	set_option(base_uri(Base), Options0, Options).
modify_state1(xml:lang = Lang, Options0, Options) :- !,
	set_option(lang(Lang), Options0, Options).
modify_state1(_, Options, Options).

set_option(Opt, Options0, [Opt|Options]) :-
	functor(Opt, F, A),
	functor(VO, F, A),
	delete(Options0, VO, Options).


%	meta_options(+OptionsIn, -OptionsOut)
%	
%	Do module qualification for options that are module sensitive.

:- module_transparent
	meta_options/2.

meta_options([], []).
meta_options([Name=Value|T0], List) :-
	atom(Name), !,
	Opt =.. [Name, Value],
	meta_options([Opt|T0], List).
meta_options([H0|T0], [H|T]) :-
	(   H0 = convert_typed_literal(Handler)
	->  strip_module(Handler, M, P),
	    H = convert_typed_literal(M:P)
	;   H = H0
	),
	meta_options(T0, T).


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
