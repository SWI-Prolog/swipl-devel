/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(man_class_hierarchy,
	  [
	  ]).

:- use_module(library(pce)).
:- use_module(util).
:- require([ send_list/3
	   ]).


:- pce_begin_class(man_class_hierarchy, man_frame,
		   "Display hiearchy of classes").

resource(leaf_font,		font,	normal,
	 "Font for built-in classes without sub-classes").
resource(non_leaf_font,		font,	bold,
	 "Font for built-in classes with sub-classes").
resource(user_defined_leaf_font, font, '@times_roman_12',
	 "Font for user defined classes with sub-classes").
resource(user_defined_non_leaf_font, font, '@times_bold_12',
	 "Font for user defined classes with sub-classes").

variable(selection,		node*,		get,
	 "Currently selected node").
variable(create_message,	code,		get,
	 "Message used to trap new classes").


initialise(CH, Manual:man_manual) :->
	"Create from manual"::
	send(CH, send_super, initialise, Manual, 'Class Hierarchy'),
	send(CH, slot, create_message,
	     new(Msg, message(CH, created_class, @arg2))),
	send(@class_class, created_message, Msg),

	send(CH, append, new(P, picture)),
	new(D, dialog),
	send(D, below, P),
	fill_dialog(D),
	fill_picture(P),
	send(CH, keyboard_focus, D),
	send(CH, expand_node, CH?tree?root).


unlink(CH) :->
	get(CH, create_message, CM),
	for_subclass(@class_class,
		     if(@arg1?created_messages \== @nil,
			message(@arg1?created_messages, delete_all, CM))),
	send(CH, send_super, unlink).


font(Tool, Class:class, Font:font) :<-
	"Get font for class"::
	get(Class, creator, Creator),
	(   Creator == built_in
	->  (   get(Class?sub_classes, size, Size),
	        Size > 0
	    ->  get(Tool, resource_value, non_leaf_font, Font)
	    ;   get(Tool, resource_value, leaf_font, Font)
	    )
	;   (   get(Class?sub_classes, size, Size),
	        Size > 0
	    ->  get(Tool, resource_value, user_defined_non_leaf_font, Font)
	    ;   get(Tool, resource_value, user_defined_leaf_font, Font)
	    )
	).


for_subclass(Class, Msg) :-
	send(Msg, forward, Class),
	(   get(Class, sub_classes, L),  L \== @nil
	->  send(L, for_all, message(@prolog, for_subclass, @arg1, Msg))
	;   true
	).


fill_dialog(D) :-
	new(CH, D?frame),
	send(D, append,
	     new(TI, text_item(class, '', message(CH, focus, @arg1)))),
	send(TI, type, class),
	send(TI, value_set, ?(@prolog, expand_classname, @arg1)),
	send(D, append, button(apply,
			       and(message(D, apply),
				   message(@receiver, active, @off)))),
	send(D, append, button(help, message(CH, help))),
	send(D, append, button(quit, message(CH, quit))),
	
	send(D?apply_member, active, @off),
	send(D, default_button, apply).

expand_classname(Prefix, Classes) :-
	new(Classes, chain),
	send(@classes, for_all,
	     if(message(@arg2?name, prefix, Prefix),
		message(Classes, append, @arg2))).


:- pce_global(@man_hierarchy_popup, make_popup).

make_popup(P) :-
	new(CH, @arg1?frame),
	new(Manual, CH?manual),
	new(Selection, Manual?selection),
	Node = @arg1,
	new(Class, Node?context),
	new(P, popup),
	new(HasSubClasses, when(Class?sub_classes,
				Class?sub_classes?size,
				0) > 0),

	send_list(P, append,
	     [ menu_item(select,
			 message(CH, request_selection, Class, @on),
			 @default, @off,
			 Node?selected == @off)
	     , menu_item(class_browser,
			 message(CH, request_tool_focus, Class),
			 @default, @on)
	     , menu_item(expand,
			 and(message(CH, expand_node, Node),
			     message(CH, normalise_node, Node)),
			 @default, @off,
			 and(message(Node?sons, empty),
			     HasSubClasses))
	     , menu_item(expand_tree,
			 and(message(CH, expand_tree, Node),
			     message(CH, normalise_node, Node)),
			 @default, @off, HasSubClasses)
	     , menu_item(collapse_node,
			 message(CH, collapse_node, Node),
			 @default, @on,
			 not(message(Node?sons, empty)))
	     , menu_item(source,
			 message(CH, request_source, Class),
			 @default, @on)
	     ]),
	ifmaintainer(send_list(P, append,
	     [ menu_item(relate,
			 message(CH, request_relate, Class),
			 @default, @on,
			 and(Manual?edit_mode  == @on,
			     Selection \== @nil,
			     Selection \== Class,
			     not(message(Selection, man_related,
					 see_also, Class))))
	     ])).


fill_picture(P) :-
	(   get(P, selection_feedback, handles)
	->  send(P, selection_feedback, invert)
	;   true
	),
	get(P, frame, Tool),
	create_node(Tool, @object_class, Root),
	send(P, display, new(T, tree(Root))),

	send_list(T, node_handler,
		  [ click_gesture(left, '', single,
				  message(P?frame, request_selection,
					  @receiver?context, @off))
		  , click_gesture(left, '', double,
				  message(P?frame, request_selection,
					  @receiver?context, @on))
		  , popup_gesture(@man_hierarchy_popup)
		  ]).


		/********************************
		*          FIND OBJECTS		*
		********************************/

tree(CH, Tree) :<-
	"Displayed tree"::
	get(CH?picture_member, tree_member, Tree).


node(CH, Class, Node) :<-
	"Node displaying class"::
	get(CH, tree, Tree),
	get(Class, name, Name),
	get(Tree?root, find, message(Name, equal, @arg1?image?string), Node).


		/********************************
		*             FOCUS		*
		********************************/

focus(CH, Class:class) :->
	"Show indicated class"::
	ensure_displayed(CH, Class, Node),
	send(CH, selected, Class),
	send(CH?picture_member, normalise, Node).

ensure_displayed(CH, Class, Node) :-
	get(CH, node, Class, Node), !.
ensure_displayed(CH, Class, Node) :-
	get(Class, super_class, Super),
	ensure_displayed(CH, Super, SuperNode),
	send(CH, expand_node, SuperNode, Class),
	get(CH, node, Class, Node).


		/********************************
		*           SELECTION		*
		********************************/

selected(CH, Obj:object*) :->
	"Set selection to specific class"::
	get(@pce, convert, Obj, 'class*', Class),
	send(CH, slot, selection, @nil),
	send(CH?tree, for_all,
	     if(@arg1?context == Class,
		and(message(@arg1?image, selected, @on),
		    message(CH, slot, selection, @arg1)),
		message(@arg1?image, selected, @off))).


release_selection(CH) :->
	send(CH, selected, @nil).


		/********************************
		*          EXPAND TREE		*
		********************************/

expand_node(_CH, Node:node, _Always:[class]) :->
	"Expand node of the tree"::
	node_to_class(Node, Class),
	(   get(Class, sub_classes, SubClasses)
%	    get(SubClasses0, find_all,
%		or(message(@prolog, in_scope, @arg1),
%		   @arg1 == Always),
%		SubClasses)
	->  new(Subs, chain),
	    send(Subs, merge, SubClasses),
	    send(Subs, sort),
	    send(Subs, for_all,
		 message(@prolog, add_subnode, Node, @arg1))
	;   true
	).


in_scope(Class) :-
	send(@manual, in_scope, Class), !.
in_scope(Class) :-
	get(Class, sub_classes, Subs),
	get(Subs, find, message(@prolog, in_scope, @arg1), _).


add_subnode(Node, Class) :-
	get(Node?sons, find, message(@arg1?string, equal, Class?name), _), !.
add_subnode(Node, Class) :-
	get(Node, frame, Tool),
	create_node(Tool, Class, Sub),
	send(Node, son, Sub).


create_node(Tool, Class, Node) :-
	get(Tool, font, Class, Font),
	new(Node, node(text(Class?name, left, Font))),
	send(Node, attribute, attribute(context, Class)).


expand_tree(CH, Node:node) :->
	"Expand entire subtree"::
	send(CH, expand_node, Node),
	send(Node?sons, for_all, message(CH, expand_tree, @arg1)).
	

expand_selection(CH) :->
	"Expand selected node"::
	get(CH, selection, Node),
	(   Node \== @nil
	->  send(CH, expand_node, Node)
	;   send(@display, inform, 'First make a selection')
	).


collapse_node(_CH, Node:node) :->
	"Destroy subnodes"::
	send(Node?sons, for_all, message(@arg1, delete_tree)).


normalise_node(_CH, Node:node) :->
	"Normalise for the subtree of Node"::
	new(Ch, chain),
	send(Node, for_all, message(Ch, append, @arg1?image)),
	send(Node?window, normalise, Ch),
	send(Ch, done).


		/********************************
		*        TRAP NEW CLASSES	*
		********************************/

created_class(CH, Class) :->
	"Handle a new class"::
	(   get(Class, super_class, Super),
	    get(CH, node, Super, Node)
	->  get(CH, font, Super, Font),
	    send(Node, font, Font),
	    (   send(Node?sons, empty)
	    ->  true
	    ;   add_subnode(Node, Class)		  % TBD: alphabetical
	    )
	;   true
	).


		/********************************
		*        MISCELLANEOUS		*
		********************************/

node_to_class(Node, Class) :-
	get(Node, context, Class).

:- pce_end_class.
