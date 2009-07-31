/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker <wielemak@science.uva.nl>
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(xpath,
	  [ xpath/3,			% +DOM, +Spec, -Value
	    xpath_chk/3,		% +DOM, +Spec, -Value

	    op(400, fx, //),
	    op(400, fx, /)
	  ]).
:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(debug)).

/** <module> Select nodes in an XML DOM

The library xpath.pl provides predicates to select nodes from an XML DOM
tree as produced by library(sgml) based  on descriptions inspired by the
XPATH language.


@see http://www.w3.org/TR/xpath
*/

:- record
	element(name, attributes, content).

%%	xpath_chk(+DOM, +Spec, ?Content) is semidet.
%
%	Semi-deterministic version of xpath/3.

xpath_chk(DOM, Spec, Content) :-
	xpath(DOM, Spec, Content), !.

%%	xpath(+DOM, +Spec, ?Content) is nondet.
%
%	Find an element in a DOM structure.   The  syntax is inspired by
%	XPath, using () rather than  []   to  select  inside an element.
%	First we can construct paths using / and //:
%	
%	    * //Term
%	    Select any node in the DOM matching term.
%	    * /Term
%	    Match the root against Term
%	    * Term
%	    Select the immediate children of the root matching Term
%	    
%	The Terms above are of type   _callable_.  The functor specifies
%	the  element  name.  Optional    arguments   specify  additional
%	constraints and functions. The arguments are processed from left
%	to right. Defined conditional argument values are:
%	
%	    * Integer
%	    The N-th element with the given name
%	    * last
%	    The last element with the given name.
%	    * last-IntExpr
%	    The IntExpr-th element counting from the last (0-based)
%	    
%	Defined function argument values are:
%	
%	    * text
%	    Evaluates to all text from the sub-tree as an atom
%	    * @Attribute
%	    Evaluates to the value of the given attribute

xpath(DOM, Spec, Content) :-
	in_dom(Spec, DOM, Content).

in_dom(//Spec, DOM, Value) :- !,
	Spec =.. [Name|Modifiers],
	sub_dom(I, Len, Name, E, DOM),
	modifiers(Modifiers, I, Len, E, Value).
in_dom(/Spec, E, Value) :- !,
	Spec =.. [Name|Modifiers],
	(   Name == self
	->  true
	;   element_name(E, Name)
	),
	modifiers(Modifiers, 1, 1, E, Value).
in_dom(A/B, DOM, Value) :- !,
	in_dom(A, DOM, Value0),
	in_dom(B, Value0, Value).
in_dom(A//B, DOM, Value) :- !,
	in_dom(A, DOM, Value0),
	in_dom(//B, Value0, Value).
in_dom(Spec, element(_, _, Content), Value) :- 
	Spec =.. [Name|Modifiers],
	count_named_elements(Content, Name, CLen),
	CLen > 0,
	nth_element(N, Name, E, Content),
	modifiers(Modifiers, N, CLen, E, Value).

%%	sub_dom(-Index, -Count, +Name, -Sub, +DOM) is nondet.
%
%	Sub is a node in DOM with Name.
%	
%	@param Count	is the total number of nodes in the content
%			list Sub appears that have the same name.
%	@param Index	is the 1-based index of Sub of nodes with
%			Name.

sub_dom(1, 1, Name, DOM, DOM) :-
	element_name(DOM, Name).
sub_dom(N, Len, Name, E, element(_,_,Content)) :- !,
	sub_dom_2(N, Len, Name, E, Content).
sub_dom(N, Len, Name, E, Content) :-
	is_list(Content),
	sub_dom_2(N, Len, Name, E, Content).

sub_dom_2(N, Len, Name, Element, Content) :-
	(   count_named_elements(Content, Name, Len),
	    nth_element(N, Name, Element, Content)
	;   member(element(_,_,C2), Content),
	    sub_dom_2(N, Len, Name, Element, C2)
	).


%%	count_named_elements(+Content, +Name, -Count) is det.
%
%	Count is the number of nodes with Name in Content.

count_named_elements(Content, Name, Count) :-
	count_named_elements(Content, Name, 0, Count).

count_named_elements([], _, Count, Count).
count_named_elements([element(Name,_,_)|T], Name, C0, C) :- !,
	C1 is C0+1,
	count_named_elements(T, Name, C1, C).
count_named_elements([_|T], Name, C0, C) :-
	count_named_elements(T, Name, C0, C).


%%	nth_element(?N, +Name, -Element, +Content:list) is nondet.
%
%	True if Element is the N-th element with name in Content.

nth_element(N, Name, Element, Content) :-
	nth_element_(1, N, Name, Element, Content).

nth_element_(I, N, Name, E, [H|T]) :-
	element_name(H, Name), !,
	(   N = I,
	    E = H
	;   I2 is I + 1,
	    (	nonvar(N), I2 > N
	    ->	!, fail
	    ;	true
	    ),
	    nth_element_(I2, N, Name, E, T)
	).
nth_element_(I, N, Name, E, [_|T]) :-
	nth_element_(I, N, Name, E, T).


%%	modifiers(+Modifiers, +I, +Clen, +DOM, -Value)
%
%	

modifiers([], _, _, Value, Value).
modifiers([H|T], I, L, Value0, Value) :-
	modifier(H, I, L, Value0, Value1),
	modifiers(T, I, L, Value1, Value).

modifier(N, I, _, Value, Value) :-				% Integer
	integer(N), !,
	N =:= I.
modifier(last, I, L, Value, Value) :- !,			% last
	I =:= L.
modifier(last-Expr, I, L, Value, Value) :- !,			% last-Expr
	I =:= L-Expr.
modifier(Function, _, _, In, Out) :-
	xpath_function(Function, In, Out).

xpath_function(self, DOM, Value) :- !,				% self
	Value = DOM.
xpath_function(text, DOM, Text) :- !,				% text
	text_of_dom(DOM, Text).
xpath_function(normalize_space, DOM, Text) :- !,		% normalize_space
	text_of_dom(DOM, Text0),
	normalize_space(atom(Text), Text0).
xpath_function(number, DOM, Number) :- !,			% number
	text_of_dom(DOM, Text0),
	normalize_space(string(Text), Text0),
	catch(atom_number(Text, Number), _, fail).
xpath_function(@Name, element(_, Attrs, _), Value) :- !,	% @Name
	memberchk(Name=Value, Attrs).
xpath_function(Left = Right, Value, Value) :- !,		% =
	var_or_function(Left, Value, LeftValue),
	var_or_function(Right, Value, RightValue),
	LeftValue = RightValue.
xpath_function(contains(Haystack, Needle), Value, Value) :- !,	% contains(Haystack, Needle)
	val_or_function(Haystack, Value, HaystackValue),
	val_or_function(Needle, Value, NeedleValue),
	atom(HaystackValue), atom(NeedleValue),
	(   sub_atom(HaystackValue, _, _, _, NeedleValue)
	->  true
	).

var_or_function(Arg, _, Arg) :-
	var(Arg), !.
var_or_function(Func, Value0, Value) :-
	xpath_function(Func, Value0, Value).

val_or_function(Arg, _, Arg) :-
	var(Arg), !,
	instantiation_error(Arg).
val_or_function(Func, Value0, Value) :-				% TBD
	xpath_function(Func, Value0, Value).


%%	text_of_dom(+DOM, -Text:atom) is det.
%
%	Text is the joined textual content of DOM.

text_of_dom(DOM, Text) :-
	phrase(text_of(DOM), Tokens),
	concat_atom(Tokens, Text).

text_of(element(_,_,Content)) -->
	text_of_list(Content).
text_of([]) -->
	[].
text_of([H|T]) -->
	text_of(H),
	text_of(T).


text_of_list([]) -->
	[].
text_of_list([H|T]) -->
	text_of_1(H),
	text_of_list(T).


text_of_1(element(_,_,Content)) --> !,
	text_of_list(Content).
text_of_1(Data) -->
	{ assertion(atom(Data)) },
	[Data].


		 /*******************************
		 *	        KEEP		*
		 *******************************/

end_of_file.

%%	xpath(+DOM, -Node, -Ancestors, -PrecedingSiblings, -FollowingSiblings) is nondet.
%
%	True if Node is a node in DOM.

xpath(DOM, DOM, [], [], []).
xpath(DOM, Node, Ancestors, PrecedingSiblings, FollowingSiblings) :-
	xpath_sub(DOM, Node, Ancestors, PrecedingSiblings, FollowingSiblings).


xpath_sub(DOM, Node, [DOM|Ancestors], PrecedingSiblings, FollowingSiblings) :-
	DOM = element(_,_,Siblings),
	append(Prefix, [SubDOM|Postfix], Siblings),
	(   Node = SubDOM,
	    PrecedingSiblings = Prefix,
	    FollowingSiblings = Postfix,
	    Ancestors = []
	;   xpath_sub(SubDOM, Node, Ancestors, PrecedingSiblings, FollowingSiblings)
	).



