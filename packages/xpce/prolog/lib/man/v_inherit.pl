/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(man_inheritance, []).
:- use_module(library(pce)).
:- require([ get_chain/3
	   , member/2
	   ]).


		 /*******************************
		 *	      TREE		*
		 *******************************/

:- pce_global(@man_inheritance_tree_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@receiver, select)),
				click_gesture(left, '', double,
					      message(@receiver, open))))).

:- pce_begin_class(man_inheritance_tree, tree).

resource(inheritance_font, font,
	 'font(helvetica, bold, 12)').
resource(delegation_attributes, vector,
	 'when(@colour_display,
	       vector(colour := green),
	       vector(texture := dotted))').

variable(showed,		name, get, "Name of ->show'ed class").
variable(delegation_link,	link, get, "Link for delegation relation").

initialise(IT) :->
	"Create inheritance tree"::
	send(IT, send_super, initialise, man_inheritance_node(object)),
	send(IT?link, pen, 2),
	send(IT, slot, delegation_link, new(DL, link(parent, son))),
	get(IT, resource_value, delegation_attributes, Vector),
	send(Vector, for_all, message(DL, @arg1?name, @arg1?value)),
	send(IT, node_handler, @man_inheritance_tree_recogniser),
	send(IT, level_gap, 20).

find_class_node(IT, Class:class, Node:man_inheritance_node) :<-
	"Find node representing class"::
	get(Class, name, ClassName),
	get(IT?root, find, message(@arg1?string, equal, ClassName), Node).

clear(IT) :->
	"Collapse all except for the root"::
	send(IT, selection, @nil),
	send(IT?root?sons, for_all, message(@arg1, delete_tree)).

show_inheritance(IT, Class:class, Font:[font], Node) :<-
	(   get(IT, find_class_node, Class, Node)
	->  true
	;   get(Class, super_class, Super),
	    get(IT, show_inheritance, Super, Font, SuperNode),
	    send(SuperNode, son, new(Node, man_inheritance_node(Class)))
	),
	(   Font \== @default
	->  send(Node, font, Font)
	;   true
	).
show_inheritance(IT, Class:class, Font:[font]) :->
	"Expand tree to display class"::
	get(IT, show_inheritance, Class, Font, _Node).

show_delegation(IT, Class:class, N1) :<-
	get(IT, show_inheritance, Class, N1),
	(   get(Class, delegate, Chain),
	    Chain \== @nil,
	    \+ send(Chain, empty)
	->  get(IT, delegation_link, DL),
	    send(Chain, for_all,
		 if(assign(new(C2, var),
			   ?(@prolog, type_to_class, @arg1?type)),
		    if(not(?(IT, find_class_node, C2)),
		       and(assign(new(N2, var),
				  ?(IT, show_delegation, C2)),
			   create(connection, N2, N1, DL)))))
	;   true
	).
show_delegation(IT, Class:class) :->
	get(IT, show_delegation, Class, _Node).

type_to_class(Type, Class) :-
	get(Type, kind, class),
	get(Type, context, Class).
type_to_class(Type, Class) :-
	get_chain(Type, supers, Supers),
	member(Super, Supers),
	type_to_class(Super, Class).


show(IT, Class:class, Clear:[bool]) :->
	get(IT, resource_value, inheritance_font, Font),
	(   Clear \== @off
	->  send(IT, clear)
	;   true
	),
	get(IT, show_inheritance, Class, Font, Node),
	send(Node, selected, @on),
	send(IT, show_delegation, Class),
	send(IT, slot, showed, Class?name).


scope(IT, Classes:chain) :<-
	"Find classes that must be scanned"::
	get(IT, selection, Grs),
	get(Grs, map, @arg1?node, Nodes),
	get(Nodes, map, @arg1?class, Classes),
	send(IT?root, for_all,
	     and(assign(new(N, var), @arg1),
		 if(?(Nodes, find, message(@arg1, is_son, N)),
		    message(Classes, append, N?class)))).


:- pce_end_class.

		 /*******************************
		 *	      NODE		*
		 *******************************/

:- pce_begin_class(man_inheritance_node, node).

resource(active_font,	font,	'font(helvetica, oblique, 12)').

initialise(N, Class:class) :->
	"Create node for class"::
	get(N, resource_value, active_font, Font),
	get(Class, name, ClassName),
	send(N, send_super, initialise, text(ClassName, left, Font)),
	send(N, name, ClassName).


convert(_, Class:class, N:man_inheritance_node) :<-
	"Convert a class to a man_inheritance_node"::
	new(N, man_inheritance_node(Class)).


son(N, Son:man_inheritance_node) :->
	"Just redefine the type"::
	send(N, send_super, son, Son).


class(N, Class:class) :<-
	"Represented class"::
	get(@pce, convert, N?string, class, Class).


on_main_inheritance_path(N) :->
	"Test if node is on main inheritance path"::
	get(N, tree, Tree),
	get(Tree, showed, ClassName),
	get(Tree, member, ClassName, Text),
	get(Text, node, Node),
	(   N == Node
	;   send(N, is_son, Node)
	).


select(N) :->
	"Select this node"::
	get(N, tree, Tree),
	get(Tree, root, Root),
	(   get(N, selected, @off)
	->  send(N, selected, @on),
	    send(Root, for_all,
		 if(or(message(@arg1, is_son, N),
		       message(N, is_son, @arg1)),
		    message(@arg1, selected, @off)))
	;   send(N, selected, @off)
	),
	(   get(Root, find, @arg1?selected == @on, _)
	->  true
	;   get(Tree, showed, ClassName),
	    get(Tree, member, ClassName, Text),
	    send(Text, selected, @on)
	),
	send(N?frame, activate_apply).

open(N) :->
	"->apply to <-frame"::
	send(N?frame, apply).


:- pce_end_class.
