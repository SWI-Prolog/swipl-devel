/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(xml_convert,
	  [ xml_select/3			% +Element, +Spec, -Part
	  ]).

		 /*******************************
		 *	      FOR NOW		*
		 *******************************/

:- op(500, fx, user:(/)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
xml_select(+DOM, +Spec, -Element)
	Find (select) an element in a DOM structure.  `Spec' is one of the
	following:

		element(Path)
		element(Path, AttributeSpec)

	If `Path' starts with a /, it searches for elements in the toplevel
	of the DOM.  Otherwise it searches for arbitrary nested elements.

	`AttributeSpec' is subset of the attribute list of the target
	element.

	EXAMPLES
	
	xml_select(DOM, element(s1/title), Title)
		Find all title elements embedded in s1 elements

	xml_select(DOM, element(_, [name=_], E)
		Find all elements having an attribute called `name'.

TODO:
	Provide goal_expansion/2 support to do the query translation at
	compile-time rather then runtime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

xml_select(Content, element(Path), element(E, A, C)) :-
	translate_path(Path, E, A, C, Content, Code),
	Code.
xml_select(Content, element(Path, Atts), element(E, A, C)) :-
	translate_path(Path, E, A, C, Content, Code),
	Code,
	subset(Atts, A).

translate_path(E, E, A, C, In, Code) :-
	var(E), !,
	Code = xml_member(element(E, A, C), In).
translate_path(/P0/P, P, A, C, In0,
	       (Code, member(element(P, A, C), In))) :- !,
	translate_path(/P0, _, _, In, In0, Code).
translate_path(/E, E, A, C, In,
	       xml_flat_member(element(E, A, C), In)) :- !.
translate_path(P0/P, P, A, C, In,
	       (Code, xml_flat_member(element(P,A,C), C1))) :- !,
	translate_path(P0, _, _, C1, In, Code).
translate_path(E, E, A, C, In,
	       xml_member(element(E, A, C), In)).
		    
xml_flat_member(E, element(_, _, Content)) :- !,
	member(E, Content).
xml_flat_member(E, List) :-
	member(E, List).

xml_member(E, E).
xml_member(E, element(_, _, C)) :- !,
	xml_member(E, C).
xml_member(E, C) :-
	is_list(C),
	member(H, C),
	xml_member(E, H).

