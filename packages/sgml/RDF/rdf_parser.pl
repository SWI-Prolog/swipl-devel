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

%	xml_to_rdf(+RDFElementOrObject, +BaseURI, -RDFTerm)
%
%	Translate an XML (using namespaces) term into an Prolog term
%	representing the RDF data.  This term can then be fed into
%	rdf_triples/[2,3] to create a list of RDF triples.
%
%	if `BaseURI' == [], local URI's are not globalised.


xml_to_plrdf(Element, Base0, RDF) :-
	set_base_uri(Element, Base0, E, Base), !,
	xml_to_plrdf(E, Base, RDF).
xml_to_plrdf(element(_:'RDF', _, Objects), BaseURI, RDF) :- !,
	rewrite(\rdf_objects(RDF, BaseURI), Objects).
xml_to_plrdf(Objects, BaseURI, RDF) :-
	(   is_list(Objects)
	->  rewrite(\rdf_objects(RDF, BaseURI), Objects)
	;   rewrite(\rdf_object(RDF, BaseURI), Objects)
	).

rdf_objects([], _Base) ::=
	[], !.
rdf_objects([H|T], Base) ::=
	[ \rdf_object_or_error(H, Base)
	| \rdf_objects(T, Base)
	].

rdf_object_or_error(H, Base) ::=
	\rdf_object(H, Base), !.
rdf_object_or_error(_, unparsed(Data)) ::=
	Data.

rdf_object(container(Type, Id, Elements), Base) ::=
	\container(Type, Id, Elements, Base), !.
rdf_object(description(Type, About, BagID, Properties), Base) ::=
	\description(Type, About, BagID, Properties, Base).


		 /*******************************
		 *	    DESCRIPTION		*
		 *******************************/

description(Type, About, BagID, Properties, Base0) ::=
	E0,
	{ set_base_uri(E0, Base0, E, Base), !,
	  rewrite(\description(Type, About, BagID, Properties, Base), E)
	}.
description(description, About, BagID, Properties, Base) ::=
	element(\rdf('Description'),
		\attrs([ \?idAboutAttr(About, Base),
			 \?bagIdAttr(BagID, Base)
		       | \propAttrs(PropAttrs, Base)
		       ]),
		\propertyElts(PropElts, Base)),
	{ !, append(PropAttrs, PropElts, Properties)
	}.
description(Type, About, BagID, Properties, Base) ::=
	element(Type,
		\attrs([ \?idAboutAttr(About, Base),
			 \?bagIdAttr(BagID, Base)
		       | \propAttrs(PropAttrs, Base)
		       ]),
		\propertyElts(PropElts, Base)),
	{ append(PropAttrs, PropElts, Properties)
	}.
		
propAttrs([], _) ::=
	[], !.
propAttrs([H|T], Base) ::=
	[ \propAttr(H, Base)
	| \propAttrs(T, Base)
	].

propAttr(rdf:type = URI, Base) ::=
	\rdf_or_unqualified(type) = \uri(URI, Base), !.
propAttr(Name = literal(Value), _) ::=
	Name = Value.

propertyElts([], _) ::=
	[], !.
propertyElts([H|T], Base) ::=
	[ \propertyElt(Id, Name, Value, Base)
	| \propertyElts(T, Base)
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


propertyElt(Id, Name, Value, Base0) ::=
	E0,
	{ set_base_uri(E0, Base0, E, Base), !,
	  rewrite(\propertyElt(Id, Name, Value, Base), E)
	}.
propertyElt(Id, Name, literal(Value), Base) ::=
	element(Name,
		\attrs([ \parseLiteral,
			 \?idAttr(Id, Base)
		       ]),
		Value), !.
propertyElt(_, Name, description(description, Id, _, Properties), Base) ::=
	element(Name,
		\attrs([ \parseResource,
			 \?idTermAttr(Id, Base)
		       ]),
		\propertyElts(Properties, Base)), !.
propertyElt(Id, Name, literal(Value), Base) ::=
	element(Name,
		\attrs([ \?idAttr(Id, Base)
		       ]),
		[ Value ]),
	{ atom(Value), !
	}.
propertyElt(Id, Name, Value, Base) ::=
	element(Name,
		\attrs([ \?idAttr(Id, Base)
		       ]),
		\an_rdf_object(Value, Base)), !.
propertyElt(_Id, Name, description(description, About, BagID, Properties), Base) ::=
	element(Name,
		\attrs([ \?idRefAttr(About, Base),
			 \?bagIdAttr(BagID, Base)
		       | \propAttrs(Properties, Base)
		       ]),
		[]), !.
propertyElt(Id, Name, unparsed(Value), Base) ::=
	element(Name,
		\attrs([ \?idAttr(Id, Base)
		       ]),
		Value).

idTermAttr(id(Id), Base) ::=
	\idAttr(Id, Base).

idAboutAttr(id(Id), Base) ::=
	\idAttr(Id, Base), !.
idAboutAttr(about(About), Base) ::=
	\aboutAttr(About, Base), !.
idAboutAttr(AboutEach, Base) ::=
	\aboutEachAttr(AboutEach, Base).

idRefAttr(Id, Base) ::=
	\idAttr(Id, Base), !.
idRefAttr(about(URI), Base) ::=
	\resourceAttr(URI, Base).


%	an_rdf_object(-Object, +BaseURI)
%
%	Deals with an object, but there may be spaces around.  I'm still
%	not sure where to deal with these.  Best is to ask the XML parser
%	to get rid of them, So most likely this code will change if this
%	happens.

an_rdf_object(Object, Base) ::=
	[ \rdf_object(Object, Base)
	], !.
an_rdf_object(Object, Base) ::=
	[ \rdf_object(Object, Base),
	  \blank
	], !.
an_rdf_object(Object, Base) ::=
	[ \blank
	| \an_rdf_object(Object, Base)
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

idAttr(Id, Base) ::=
	\rdf_or_unqualified('ID') = \globalid(Id, Base).

bagIdAttr(Id, Base) ::=
	\rdf_or_unqualified(bagID) = \globalid(Id, Base).

aboutAttr(About, Base) ::=
	\rdf_or_unqualified(about) = \uri(About, Base).

aboutEachAttr(each(AboutEach), Base) ::=
	\rdf_or_unqualified(aboutEach) = \uri(AboutEach, Base), !.
aboutEachAttr(prefix(Prefix), Base) ::=
	\rdf_or_unqualified(aboutEachPrefix) = \uri(Prefix, Base), !.

resourceAttr(URI, Base) ::=
	\rdf_or_unqualified(resource) = \uri(URI, Base).


uri(URI, Base) ::=
	A,
	{   Base \== []
	->  canonical_uri(A, Base, URI)
	;   sub_atom(A, 0, _, _, #)
	->  sub_atom(A, 1, _, 0, URI)
	;   URI = A
	}.

globalid(Id, Base) ::=
	A,
	{   Base \== []
	->  concat_atom([Base, A], #, Id)
	;   sub_atom(A, 0, _, _, #)		% Protege 1.3 -hack
	->  sub_atom(A, 1, _, 0, Id),
	    print_message(warning, rdf(protege(id, A)))
	;   Id = A
	}.




		 /*******************************
		 *	     CONTAINERS		*
		 *******************************/

container(Type, Id, Elements, Base0) ::=
	E0,
	{ set_base_uri(E0, Base0, E, Base), !,
	  rewrite(\container(Type, Id, Elements, Base), E)
	}.
container(Type, Id, Elements, Base) ::=
	element(\containertype(Type),
		\attrs([ \?idAttr(Id, Base)
		       | \memberAttrs(Elements)
		       ]),
		[]), !.
container(Type, Id, Elements, Base) ::=
	element(\containertype(Type),
		\attrs([ \?idAttr(Id, Base)
		       ]),
		\memberElts(Elements, Base)).

containertype(Type) ::=
	\rdf(Type),
	{ containertype(Type)
	}.

containertype('Bag').
containertype('Seq').
containertype('Alt').

memberElts([], _) ::=
	[].
memberElts([H|T], Base) ::=
	[ \memberElt(H, Base)
	| \memberElts(T, Base)
	].

memberElt(LI, Base) ::=
	\referencedItem(LI, Base).
memberElt(LI, Base) ::=
	\inlineItem(LI, Base).

referencedItem(LI, Base0) ::=
	E0,
	{ set_base_uri(E0, Base0, E, Base), !,
	  rewrite(\referencedItem(LI, Base), E)
	}.
referencedItem(LI, Base) ::=
	element(\rdf_or_unqualified(li),
		[ \resourceAttr(LI, Base) ],
		[]).

inlineItem(Item, Base0) ::=
	E0,
	{ set_base_uri(E0, Base0, E, Base), !,
	  rewrite(\inlineItem(Item, Base), E)
	}.
inlineItem(literal(LI), _Base) ::=
	element(\rdf_or_unqualified(li),
		[ \parseLiteral ],
		LI).
inlineItem(description(description, _, _, Properties), Base) ::=
	element(\rdf_or_unqualified(li),
		[ \parseResource ],
		\propertyElts(Properties, Base)).
inlineItem(LI, Base) ::=
	element(\rdf_or_unqualified(li),
		[],
		[\rdf_object(LI, Base)]), !.	% inlined object
inlineItem(literal(LI), _Base) ::=
	element(\rdf_or_unqualified(li),
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

parseLiteral  ::= \rdf_or_unqualified(parseType) = 'Literal'.
parseResource ::= \rdf_or_unqualified(parseType) = 'Resource'.


		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

rdf(Tag) ::=
	NS:Tag,
	{ rdf_name_space(NS), !
	}.

rdf_or_unqualified(Tag) ::=
	Tag.
rdf_or_unqualified(Tag) ::=
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

%	set_base_uri(+Element0, +Base0, -Element, -Base)
%	
%	If Element0 contains xml:base = Base1, strip it from the
%	attributes list and recalculate the new base-uri

set_base_uri(element(Name, Attrs0, Content), Base0,
	     element(Name, Attrs, Content),  Base) :-
	select(xml:base=Base1, Attrs0, Attrs), !,
	canonical_uri(Base1, Base0, Base).

		 /*******************************
		 *     HELP PCE-EMACS A BIT	*
		 *******************************/

:- multifile
	emacs_prolog_colours:term_colours/2,
	emacs_prolog_colours:goal_classification/2.

expand(c(X), _, X) :- !.
expand(In,   Pattern, Colours) :-
	compound(In), !,
	In =.. [F|Args],
	expand_list(Args, PatternArgs, ColourArgs),
	Pattern =.. [F|PatternArgs],
	Colours = functor(F) - ColourArgs.
expand(X, X, classify).

expand_list([], [], []).
expand_list([H|T], [PH|PT], [CH|CT]) :-
	expand(H, PH, CH),
	expand_list(T, PT, CT).

:- discontiguous
	term_expansion/2.

term_expansion(term_colours(C),
	       emacs_prolog_colours:term_colours(Pattern, Colours)) :-
	expand(C, Pattern, Colours).

term_colours((c(head(+(1))) ::= c(match), {c(body)})).
term_colours((c(head(+(1))) ::= c(match))).

emacs_prolog_colours:goal_classification(\_, expanded).

:- multifile
	prolog:meta_goal/2.

prolog:meta_goal(rewrite(A, _), [A]).
prolog:meta_goal(\A,		[A+1]).
