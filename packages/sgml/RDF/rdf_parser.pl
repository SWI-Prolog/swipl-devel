/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
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

:- module(rdf_parser,
	  [ xml_to_plrdf/3,		% +XMLTerm, -RDFTerm, +Options
	    element_to_plrdf/3,		% +ContentList, -RDFTerm, +Options
	    rdf_name_space/1
	  ]).
:- use_module(rewrite).

:- op(500, fx, \?).			% Optional (attrs)

term_expansion(F, T) :- rew_term_expansion(F, T).
goal_expansion(F, T) :- rew_goal_expansion(F, T).

:- multifile rdf_name_space/1.
:- dynamic   rdf_name_space/1.

rdf_name_space('http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdf_name_space('http://www.w3.org/TR/REC-rdf-syntax').

%	xml_to_rdf(+RDFElementOrObject, -RDFTerm, +Options)
%
%	Translate an XML (using namespaces) term into an Prolog term
%	representing the RDF data.  This term can then be fed into
%	rdf_triples/[2,3] to create a list of RDF triples.
%
%	if `BaseURI' == [], local URI's are not globalised.


xml_to_plrdf(Element, RDF, Options) :-
	is_list(Element), !,
	rewrite(\xml_content_objects(RDF, Options), Element).
xml_to_plrdf(Element, RDF, Options) :-
	rewrite(\xml_objects(RDF, Options), Element).

element_to_plrdf(Element, RDF, Options) :-
	rewrite(\nodeElementList(RDF, Options), [Element]).

xml_objects(Objects, Options0) ::=
	E0,
	{ modify_state(E0, Options0, E, Options), !,
	  rewrite(\xml_objects(Objects, Options), E)
	}.
xml_objects(Objects, Options) ::=
	element((\rdf('RDF'), !),
		_,
		\nodeElementList(Objects, Options)),
	!.
xml_objects(Objects, Options) ::=
	element(_, _, \xml_content_objects(Objects, Options)).

xml_content_objects([], _) ::=
	[].
xml_content_objects([H|T], Options) ::=
	[ \xml_objects(H, Options)
	| \xml_content_objects(T, Options)
	].


nodeElementList([], _Options) ::=
	[], !.
nodeElementList(L, Options) ::=
	[ (\ws, !)
	| \nodeElementList(L, Options)
	].
nodeElementList([H|T], Options) ::=
	[ \nodeElementOrError(H, Options)
	| \nodeElementList(T, Options)
	].

nodeElementOrError(H, Options) ::=
	\nodeElement(H, Options), !.
nodeElementOrError(unparsed(Data), _Options) ::=
	Data.

nodeElement(container(Type, Id, Elements), Options) ::=
	\container(Type, Id, Elements, Options), !. % compatibility
nodeElement(description(Type, About, BagID, Properties), Options) ::=
	\description(Type, About, BagID, Properties, Options).


		 /*******************************
		 *	    DESCRIPTION		*
		 *******************************/

description(Type, About, BagID, Properties, Options0) ::=
	E0,
	{ modify_state(E0, Options0, E, Options), !,
	  rewrite(\description(Type, About, BagID, Properties, Options), E)
	}.
description(description, About, BagID, Properties, Options) ::=
	element(\rdf('Description'),
		\attrs([ \?idAboutAttr(About, Options),
			 \?bagIdAttr(BagID, Options)
		       | \propAttrs(PropAttrs, Options)
		       ]),
		\propertyElts(PropElts, Options)),
	{ !, append(PropAttrs, PropElts, Properties)
	}.
description(Type, About, BagID, Properties, Options) ::=
	element(Type,
		\attrs([ \?idAboutAttr(About, Options),
			 \?bagIdAttr(BagID, Options)
		       | \propAttrs(PropAttrs, Options)
		       ]),
		\propertyElts(PropElts, Options)),
	{ append(PropAttrs, PropElts, Properties)
	}.
		
propAttrs([], _) ::=
	[], !.
propAttrs([H|T], Options) ::=
	[ \propAttr(H, Options)
	| \propAttrs(T, Options)
	].

propAttr(rdf:type = URI, Options) ::=
	\rdf_or_unqualified(type) = \uri(URI, Options), !.
propAttr(Name = Literal, Options) ::=
	Name = Value,
	{ mkliteral(Value, Literal, Options)
	}.

propertyElts([], _) ::=
	[], !.
propertyElts(Elts, Options) ::=
	[ (\ws, !)
	| \propertyElts(Elts, Options)
	].
propertyElts([H|T], Options) ::=
	[ \propertyElt(H, Options)
	| \propertyElts(T, Options)
	].

propertyElt(E, Options) ::=
	\propertyElt(Id, Name, Value, Options),
	{ mkprop(Name, Value, Prop),
	  (   var(Id)
	  ->  E = Prop
	  ;   E = id(Id, Prop)
	  )
	}.

mkprop(NS:Local, Value, rdf:Local = Value) :-
	rdf_name_space(NS), !.
mkprop(Name, Value, Name = Value).


propertyElt(Id, Name, Value, Options0) ::=
	E0,
	{ modify_state(E0, Options0, E, Options), !,
	  rewrite(\propertyElt(Id, Name, Value, Options), E)
	}.
					% 5.14 emptyPropertyElt
propertyElt(Id, Name, Value, Options) ::=
	element(Name, A, \all_ws),
	{ !,
	  rewrite(\emptyPropertyElt(Id, Value, Options), A)
	}.

propertyElt(_, Name, description(description, Id, _, Properties), Options) ::=
	element(Name,
		\attrs([ \parseResource,
			 \?idAboutAttr(Id, Options)
		       ]),
		\propertyElts(Properties, Options)),
	!.
propertyElt(_, Name, Literal, Options) ::=
	element(Name,
		\attrs([ \parseLiteral
		       ]),
		Content),
	{ !,
	  literal_value(Content, Literal, Options)
	}.
propertyElt(Id, Name, collection(Elements), Options) ::=
	element(Name,
		\attrs([ \parseCollection,
			 \?idAttr(Id, Options)
		       ]),
		\nodeElementList(Elements, Options)).
propertyElt(Id, Name, Literal, Options) ::=
	element(Name,
		\attrs([ \typeAttr(Type, Options),
			 \?idAttr(Id, Options)
		       ]),
		Content),
	{ typed_literal(Type, Content, Literal, Options)
	}.
propertyElt(Id, Name, Literal, Options) ::=
	element(Name,
		\attrs([ \?idAttr(Id, Options)
		       ]),
		[ Value ]),
	{ atom(Value), !,
	  mkliteral(Value, Literal, Options)
	}.
propertyElt(Id, Name, Value, Options) ::=
	element(Name,
		\attrs([ \?idAttr(Id, Options)
		       ]),
		\an_rdf_object(Value, Options)), !.
propertyElt(Id, Name, unparsed(Value), Options) ::=
	element(Name,
		\attrs([ \?idAttr(Id, Options)
		       ]),
		Value).

emptyPropertyElt(Id, literal(''), Options) ::=
	\attrs([ \?idAttr(Id, Options),
		 \?parseLiteral
	       | \noMoreAttrs
	       ]), !.
emptyPropertyElt(Id,
		 description(description, About, BagID, Properties),
		 Options) ::=
	\attrs([ \?idAttr(Id, Options),
		 \?aboutResourceEmptyElt(About, Options),
		 \?bagIdAttr(BagID, Options),
		 \?parseResource
	       | \propAttrs(Properties, Options)
	       ]), !.

aboutResourceEmptyElt(about(URI), Options) ::=
	\resourceAttr(URI, Options), !.
aboutResourceEmptyElt(node(URI), _Options) ::=
	\nodeIDAttr(URI).

%	literal_value(+In, -Value, +Options)
%	
%	Translate a literal into its  value.   Notably  if  the value is
%	plain  CDATA,  remove  the   list.    This   predicate   handles
%	parseType="Literal" attributes.

literal_value([Value], Literal, Options) :-
	atomic(Value), !,
	mkliteral(Value, Literal, Options).
literal_value(Value, literal(Value), _).


%	mkliteral(+Atom, -Object, +Options)
%	
%	Translate attribute value Atom into an RDF object using the
%	lang(Lang) option from Options.

mkliteral(Text, literal(Val), Options) :-
	atom(Text),
	(   memberchk(lang(Lang), Options),
	    Lang \== ''
	->  Val = lang(Lang, Text)
	;   Val = Text
	).

%	typed_literal(+Type, +Content, -Literal, +Options)
%	
%	Handle a literal attribute with rdf:dataType=Type qualifier. NB:
%	possibly  it  is  faster  to  use  a  global  variable  for  the
%	conversion hook.

typed_literal(Type, Content, literal(Object), Options) :-
	memberchk(convert_typed_literal(Convert), Options), !,
	(   catch(call(Convert, Type, Content, Object), E, true)
	->  (   var(E)
	    ->	true
	    ;	Object = E
	    )
	;   Object = error(cannot_convert(Type, Content), _)
	).
typed_literal(Type, [Text], literal(type(Type, Text)), _Options) :- !.
typed_literal(Type, Content, literal(type(Type, Content)), _Options).
	

idTermAttr(id(Id), Options) ::=
	\idAttr(Id, Options).

idAboutAttr(id(Id), Options) ::=
	\idAttr(Id, Options), !.
idAboutAttr(about(About), Options) ::=
	\aboutAttr(About, Options), !.
idAboutAttr(node(About), _Options) ::=
	\nodeIDAttr(About), !.
idAboutAttr(AboutEach, Options) ::=
	\aboutEachAttr(AboutEach, Options).

idRefAttr(Id, Options) ::=
	\idAttr(Id, Options), !.
idRefAttr(about(URI), Options) ::=
	\resourceAttr(URI, Options).

%	an_rdf_object(-Object, +OptionsURI)
%
%	Deals with an object, but there may be spaces around.  I'm still
%	not sure where to deal with these.  Best is to ask the XML parser
%	to get rid of them, So most likely this code will change if this
%	happens.

an_rdf_object(Object, Options) ::=
	[ \nodeElement(Object, Options)
	], !.
an_rdf_object(Object, Options) ::=
	[ (\ws, !)
	| \an_rdf_object(Object, Options)
	].
an_rdf_object(Object, Options) ::=
	[ \nodeElement(Object, Options),
	  \ws
	], !.

ws ::=
	A,
	{ atom(A),
	  atom_chars(A, Chars),
	  all_blank(Chars), !
	}.
ws ::=
	pi(_).

all_ws ::=
	[], !.
all_ws ::=
	[\ws | \all_ws].

all_blank([]).
all_blank([H|T]) :-
	char_type(H, space),		% SWI-Prolog specific
	all_blank(T).


		 /*******************************
		 *	   RDF ATTRIBUTES	*
		 *******************************/

idAttr(Id, Options) ::=
	\rdf_or_unqualified('ID') = \uniqueid(Id, Options).

bagIdAttr(Id, Options) ::=
	\rdf_or_unqualified(bagID) = \globalid(Id, Options).

aboutAttr(About, Options) ::=
	\rdf_or_unqualified(about) = \uri(About, Options).

nodeIDAttr(About) ::=
	\rdf_or_unqualified(nodeID) = About.

%	Not allowed in current RDF!

aboutEachAttr(each(AboutEach), Options) ::=
	\rdf_or_unqualified(aboutEach) = \uri(AboutEach, Options), !.
aboutEachAttr(prefix(Prefix), Options) ::=
	\rdf_or_unqualified(aboutEachPrefix) = \uri(Prefix, Options), !.

resourceAttr(URI, Options) ::=
	\rdf_or_unqualified(resource) = \uri(URI, Options).

typeAttr(Type, Options) ::=
	\rdf_or_unqualified(dataType) = \uri(Type, Options).

uri(URI, Options) ::=
	A,
	{   memberchk(base_uri(Base), Options),
	    Base \== []
	->  canonical_uri(A, Base, URI)
	;   sub_atom(A, 0, _, _, #)
	->  sub_atom(A, 1, _, 0, URI)
	;   URI = A
	}.

globalid(Id, Options) ::=
	A,
	{   make_globalid(A, Options, Id)
	}.

uniqueid(Id, Options) ::=
	A,
	{   unique_xml_name(A),
	    make_globalid(A, Options, Id)
	}.

unique_xml_name(Name) :-
	(   xml_name(Name)
	->  true
	;   print_message(warning, rdf(not_a_name(Name)))
	).

make_globalid(In, Options, Id) :-
	(   memberchk(base_uri(Base), Options),
	    Base \== []
	->  (   is_absolute_url(In)
	    ->	Id = In
	    ;	concat_atom([Base, In], #, Id)
	    )
	;   sub_atom(In, 0, _, _, #)
	->  sub_atom(In, 1, _, 0, Id)
	;   Id = In
	).


%	canonical_uri(+In, +Base, -Absolute)

canonical_uri('', Base, Base) :- !.	% '' expands to xml:base
canonical_uri(URI, [], URI) :- !.	% do not use one
canonical_uri(URI, Base, Global) :-	% use our generic library
	global_url(URI, Base, Global).


		 /*******************************
		 *	     CONTAINERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that containers are no longer part   of  the definition. We'll keep
the code and call it conditionally if we must.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

container(_, _, _, _) ::=
	_,
	{ \+ current_prolog_flag(rdf_container, true),
	  !, fail
	}.
container(Type, Id, Elements, Options0) ::=
	E0,
	{ modify_state(E0, Options0, E, Options), !,
	  rewrite(\container(Type, Id, Elements, Options), E)
	}.
container(Type, Id, Elements, Options) ::=
	element(\containertype(Type),
		\attrs([ \?idAttr(Id, Options)
		       | \memberAttrs(Elements)
		       ]),
		[]), !.
container(Type, Id, Elements, Options) ::=
	element(\containertype(Type),
		\attrs([ \?idAttr(Id, Options)
		       ]),
		\memberElts(Elements, Options)).

containertype(Type) ::=
	\rdf(Type),
	{ containertype(Type)
	}.

containertype('Bag').
containertype('Seq').
containertype('Alt').

memberElts([], _) ::=
	[].
memberElts([H|T], Options) ::=
	[ \memberElt(H, Options)
	| \memberElts(T, Options)
	].

memberElt(LI, Options) ::=
	\referencedItem(LI, Options).
memberElt(LI, Options) ::=
	\inlineItem(LI, Options).

referencedItem(LI, Options0) ::=
	E0,
	{ modify_state(E0, Options0, E, Options), !,
	  rewrite(\referencedItem(LI, Options), E)
	}.
referencedItem(LI, Options) ::=
	element(\rdf_or_unqualified(li),
		[ \resourceAttr(LI, Options) ],
		[]).

inlineItem(Item, Options0) ::=
	E0,
	{ modify_state(E0, Options0, E, Options), !,
	  rewrite(\inlineItem(Item, Options), E)
	}.
inlineItem(Literal, Options) ::=
	element(\rdf_or_unqualified(li),
		[ \parseLiteral ],
		Value),
	literal_value(Value, Literal, Options).
inlineItem(description(description, _, _, Properties), Options) ::=
	element(\rdf_or_unqualified(li),
		[ \parseResource ],
		\propertyElts(Properties, Options)).
inlineItem(LI, Options) ::=
	element(\rdf_or_unqualified(li),
		[],
		[\nodeElement(LI, Options)]), !.	% inlined object
inlineItem(Literal, Options) ::=
	element(\rdf_or_unqualified(li),
		[],
		[Text]),
	{ mkliteral(Text, Literal, Options)
	}.

memberAttrs([]) ::=
	[].
memberAttrs([H|T]) ::=
	[ \memberAttr(H)
	| \memberAttrs(T)
	].

memberAttr(li(Id, Value)) ::=		% Id should be _<n>
	\rdf(Id) = Value.

parseLiteral    ::= \rdf_or_unqualified(parseType) = 'Literal'.
parseResource   ::= \rdf_or_unqualified(parseType) = 'Resource'.
parseCollection ::= \rdf_or_unqualified(parseType) = 'Collection'.


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

%	\noMoreAttrs
%	
%	Check attribute-list is empty.  Reserved xml: attributes are
%	excluded from this test.

noMoreAttrs ::=
	[], !.
noMoreAttrs ::=
	[ xml:_=_
	| \noMoreAttrs
	].

%	modify_state(+Element0, +Options0, -Element, -Options)
%	
%	If Element0 contains xml:base = Base, strip it from the
%	attributes list and update base_uri(_) in the Options
%	
%	It Element0 contains xml:lang = Lang, strip it from the
%	attributes list and update lang(_) in the Options

modify_state(element(Name, Attrs0, Content), Options0,
	     element(Name, Attrs, Content),  Options) :-
	select(xml:base=Base1, Attrs0, Attrs), !,
	(   select(base_uri(Base0), Options0, Options1)
	->  true
	;   Base0 = [],
	    Options1 = Options0
	),
	remove_fragment(Base1, Base2),
	canonical_uri(Base2, Base0, Base),
	Options = [base_uri(Base)|Options1].
modify_state(element(Name, Attrs0, Content), Options0,
	     element(Name, Attrs, Content),  Options) :-
	select(xml:lang=Lang, Attrs0, Attrs),
	\+ memberchk(ignore_lang(true), Options0), !,
	delete(Options0, lang(_), Options1),
	(   Lang == ''
	->  Options = Options1
	;   Options = [lang(Lang)|Options1]
	).

%	remove_fragment(+URI, -WithoutFragment)
%	
%	When handling xml:base, we must delete the possible fragment.

remove_fragment(URI, Plain) :-
	sub_atom(URI, B, _, _, #), !,
	sub_atom(URI, 0, B, _, Plain).
remove_fragment(URI, URI).
	

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

:- dynamic
	prolog:meta_goal/2.
:- multifile
	prolog:meta_goal/2.

prolog:meta_goal(rewrite(A, _), [A]).
prolog:meta_goal(\A,		[A+1]).
