/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_parser,
	  [ xml_to_plrdf/3,		% +XMLTerm, +BaseURI, -RDFTerm
	    rdf_name_space/1
	  ]).
:- use_module(uri).
:- use_module(rewrite).

:- op(500, fx, \?).			% Optional (attrs)

term_expansion(F, T) :- rew_term_expansion(F, T).
goal_expansion(F, T) :- rew_goal_expansion(F, T).

:- multifile rdf_name_space/1.
:- dynamic   rdf_name_space/1.

rdf_name_space('http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdf_name_space('http://www.w3.org/TR/REC-rdf-syntax').

:- dynamic
	base_uri/1.

%	xml_to_rdf(+RDFElementOrObject, +BaseURI, -RDFTerm)
%
%	Translate an XML (using namespaces) term into an Prolog term
%	representing the RDF data.  This term can then be fed into
%	rdf_triples/[2,3] to create a list of RDF triples.
%
%	if `BaseURI' == [], local URI's are not globalised.


xml_to_plrdf(element(_:'RDF', _, Objects), BaseURI, RDF) :- !,
	asserta(base_uri(BaseURI), Ref),
	rewrite(\rdf_objects(RDF), Objects),
	erase(Ref).
xml_to_plrdf(Objects, BaseURI, RDF) :-
	asserta(base_uri(BaseURI), Ref),
	(   is_list(Objects)
	->  rewrite(\rdf_objects(RDF), Objects)
	;   rewrite(\rdf_object(RDF), Objects)
	),
	erase(Ref).

rdf_objects([]) ::=
	[].
rdf_objects([H|T]) ::=
	[ \rdf_object_or_error(H)
	| \rdf_objects(T)
	].

rdf_object_or_error(H) ::=
	\rdf_object(H), !.
rdf_object_or_error(unparsed(Data)) ::=
	Data.

rdf_object(container(Type, Id, Elements)) ::=
	\container(Type, Id, Elements), !.
rdf_object(description(Type, About, BagID, Properties)) ::=
	\description(Type, About, BagID, Properties).


		 /*******************************
		 *	    DESCRIPTION		*
		 *******************************/

description(description, About, BagID, Properties) ::=
	element(\rdf('Description'),
		\attrs([ \?idAboutAttr(About),
			 \?bagIdAttr(BagID)
		       | \propAttrs(PropAttrs)
		       ]),
		\propertyElts(PropElts)),
	{ !, append(PropAttrs, PropElts, Properties)
	}.
description(Type, About, BagID, Properties) ::=
	element(Type,
		\attrs([ \?idAboutAttr(About),
			 \?bagIdAttr(BagID)
		       | \propAttrs(PropAttrs)
		       ]),
		\propertyElts(PropElts)),
	{ append(PropAttrs, PropElts, Properties)
	}.
		
propAttrs([]) ::=
	[], !.
propAttrs([H|T]) ::=
	[ \propAttr(H)
	| \propAttrs(T)
	].

propAttr(rdf:type = URI) ::=
	\rdf(type) = \uri(URI), !.
propAttr(Name = literal(Value)) ::=
	Name = Value.

propertyElts([]) ::=
	[], !.
propertyElts([H|T]) ::=
	[ \propertyElt(Id, Name, Value)
	| \propertyElts(T)
	],
	{ mkprop(Name, Value, Prop),
	  (   var(Id)
	  ->  H = Prop
	  ;   H = id(Id, Prop)
	  )
	}.

mkprop(NS:Local, Value, rdf:Local = Value) :-
	rdf_name_space(NS), !.
mkprop(Name, Value, Name = Value).


propertyElt(Id, Name, literal(Value)) ::=
	element(Name,
		\attrs([ \parseLiteral,
			 \?idAttr(Id)
		       ]),
		Value), !.
propertyElt(_, Name, description(description, Id, _, Properties)) ::=
	element(Name,
		\attrs([ \parseResource,
			 \?idTermAttr(Id)
		       ]),
		\propertyElts(Properties)), !.
propertyElt(Id, Name, literal(Value)) ::=
	element(Name,
		\attrs([ \?idAttr(Id)
		       ]),
		[ Value ]),
	{ atom(Value), !
	}.
propertyElt(Id, Name, Value) ::=
	element(Name,
		\attrs([ \?idAttr(Id)
		       ]),
		\an_rdf_object(Value)), !.
propertyElt(_Id, Name, description(description, About, BagID, Properties)) ::=
	element(Name,
		\attrs([ \?idRefAttr(About),
			 \?bagIdAttr(BagID)
		       | \propAttrs(Properties)
		       ]),
		[]), !.
propertyElt(Id, Name, unparsed(Value)) ::=
	element(Name,
		\attrs([ \?idAttr(Id)
		       ]),
		Value).

idTermAttr(id(Id)) ::=
	\idAttr(Id).

idAboutAttr(id(Id)) ::=
	\idAttr(Id), !.
idAboutAttr(about(About)) ::=
	\aboutAttr(About), !.
idAboutAttr(AboutEach) ::=
	\aboutEachAttr(AboutEach).

idRefAttr(Id) ::=
	\idAttr(Id), !.
idRefAttr(about(URI)) ::=
	\resourceAttr(URI).


%	an_rdf_object(-Object)
%
%	Deals with an object, but there may be spaces around.  I'm still
%	not sure where to deal with these.  Best is to ask the XML parser
%	to get rid of them, So most likely this code will change if this
%	happens.

an_rdf_object(Object) ::=
	[ \rdf_object(Object)
	], !.
an_rdf_object(Object) ::=
	[ \rdf_object(Object),
	  \blank
	], !.
an_rdf_object(Object) ::=
	[ \blank
	| \an_rdf_object(Object)
	].

blank ::=
	A,
	{ atom(A),
	  atom_chars(A, Chars),
	  all_blank(Chars)
	}.

all_blank([]).
all_blank([H|T]) :-
	char_type(H, space),		% SWI-Prolog specific
	all_blank(T).


		 /*******************************
		 *	   RDF ATTRIBUTES	*
		 *******************************/

idAttr(Id) ::=
	\rdf('ID') = \globalid(Id).

bagIdAttr(Id) ::=
	\rdf(bagID) = \globalid(Id).

aboutAttr(About) ::=
	\rdf(about) = \uri(About).

aboutEachAttr(each(AboutEach)) ::=
	\rdf(aboutEach) = \uri(AboutEach), !.
aboutEachAttr(prefix(Prefix)) ::=
	\rdf(aboutEachPrefix) = \uri(Prefix), !.

resourceAttr(URI) ::=
	\rdf(resource) = \uri(URI).


uri(URI) ::=
	A,
	{   (   base_uri(Base)
	    ->  Base \== []
	    )
	->  canonical_uri(A, Base, URI)
	;   sub_atom(A, 0, _, _, #)
	->  sub_atom(A, 1, _, 0, URI)
	;   URI = A
	}.

globalid(Id) ::=
	A,
	{   (   base_uri(Base)
	    ->  Base \== []
	    )
	->  concat_atom([Base, A], #, Id)
	;   sub_atom(A, 0, _, _, #)		% Protege 1.3 -hack
	->  sub_atom(A, 1, _, 0, Id),
	    print_message(warning, rdf(protege(id, A)))
	;   Id = A
	}.




		 /*******************************
		 *	     CONTAINERS		*
		 *******************************/

container(Type, Id, Elements) ::=
	element(\containertype(Type),
		\attrs([ \?idAttr(Id)
		       | \memberAttrs(Elements)
		       ]),
		[]), !.
container(Type, Id, Elements) ::=
	element(\containertype(Type),
		\attrs([ \?idAttr(Id)
		       ]),
		\memberElts(Elements)).

containertype(Type) ::=
	\rdf(Type),
	{ containertype(Type)
	}.

containertype('Bag').
containertype('Seq').
containertype('Alt').

memberElts([]) ::=
	[].
memberElts([H|T]) ::=
	[ \memberElt(H)
	| \memberElts(T)
	].

memberElt(LI) ::=
	\referencedItem(LI).
memberElt(LI) ::=
	\inlineItem(LI).

referencedItem(LI) ::=
	element(\rdf(li),
		[ \resourceAttr(LI) ],
		[]).

inlineItem(literal(LI)) ::=
	element(\rdf(li),
		[ \parseLiteral ],
		LI).
inlineItem(description(description, _, _, Properties)) ::=
	element(\rdf(li),
		[ \parseResource ],
		\propertyElts(Properties)).
inlineItem(LI) ::=
	element(\rdf(li),
		[],
		[\rdf_object(LI)]), !.	% inlined object
inlineItem(literal(LI)) ::=
	element(\rdf(li),
		[],
		[LI]).			% string value

memberAttrs([]) ::=
	[].
memberAttrs([H|T]) ::=
	[ \memberAttr(H)
	| \memberAttrs(T)
	].

memberAttr(li(Id, Value)) ::=		% Id should be _<n>
	\rdf(Id) = Value.

parseLiteral  ::= \rdf(parseType) = 'Literal'.
parseResource ::= \rdf(parseType) = 'Resource'.


		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

rdf(Tag) ::=
	NS:Tag,
	{ rdf_name_space(NS), !
	}.


		 /*******************************
		 *	       BASICS		*
		 *******************************/

attrs(Bag) ::=
	L0,
	{ do_attrs(Bag, L0)
	}.

do_attrs([], _) :- !.
do_attrs([\?H|T], L0) :- !,		% optional
	(   select(X, L0, L),
	    rewrite(\H, X)
	->  true
	;   L = L0
	),
	do_attrs(T, L).
do_attrs([H|T], L0) :-
	select(X, L0, L),
	rewrite(H, X), !,
	do_attrs(T, L).
do_attrs(C, L) :-
	rewrite(C, L).
