/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(man_class_hierarchy, []).
:- use_module(library(pce)).
:- require([ default/3
	   , send_list/3
	   ]).

resource(builtin_class, image, image('16x16/builtin_class.xpm')).
resource(user_class,    image, image('16x16/user_class.xpm')).

:- pce_autoload(toc_window, library(pce_toc)).

:- pce_begin_class(man_class_hierarchy_window, toc_window,
		   "Visualisation of XPCE class hierarchy").

variable(create_message,	code,		get,
	 "Message used to trap new classes").

class_variable(size, size, size(300, 300)).

initialise(CH, Root:[class]) :->
	default(Root, class(object), TheRoot),
	send(CH, send_super, initialise),
	get(TheRoot, name, Name),
	send(CH, root, toc_folder(Name, TheRoot,
				  resource(builtin_class),
				  resource(builtin_class))),
	send(CH, expand_root),
	send(CH, slot, create_message,
	     new(Msg, message(CH, created_class, @arg2))),
	send(@class_class, created_message, Msg).

unlink(CH) :->
	get(CH, create_message, CM),
	for_subclass(@class_class,
		     if(@arg1?created_messages \== @nil,
			message(@arg1?created_messages, delete_all, CM))),
	send(CH, send_super, unlink).

for_subclass(Class, Msg) :-
	send(Msg, forward, Class),
	(   get(Class, sub_classes, L),  L \== @nil
	->  send(L, for_all, message(@prolog, for_subclass, @arg1, Msg))
	;   true
	).


created_class(CH, Class:class) :->
	"Handle a new class"::
	(   get(Class, super_class, Super),
	    get(CH, node, Super, Node)
	->  (   send(Node, instance_of, toc_folder)
	    ->	send(CH, add_class, Class)
	    ;	get(Node, tree, Tree),
	        (   get(Node?parents?head?sons, previous, Node, Prev)
		->  true
		;   Prev = @nil
		),
	        send(Node, free),
		send(Tree, compute),
		send(CH, add_class, Super),
		get(CH, node, Super, NewNode),
		send(NewNode, move_after, Prev)
	    )
	;   true
	).


select_node(CH, Class:class) :->
	send(CH, request_selection, Class).

open_node(CH, Class:class) :->
	send(CH, request_selection, Class, @on).

expand_node(CH, Class:class) :->
	get(Class, sub_classes, SubClasses),
	send(SubClasses, for_all,
	     message(CH, add_class, @arg1)),
	send(CH, send_super, expand_node, Class).

ensure_class(CH, Class:class) :->
	"Expand the tree to make Class visible"::
	(   get(CH, node, Class, _)
	->  true
	;   get(Class, super_class, Super),
	    send(CH, ensure_class, Super),
	    send(CH, expand_node, Super)
	).

add_class(CH, Class:class) :->
	"Add a class to the viewer, assuming the super-class is there"::
	(   get(CH, node, Class, _)
	->  true
	;   get(Class, super_class, Super),
	    (   get(Class, creator, built_in)
	    ->  Image = resource(builtin_class)
	    ;   Image = resource(user_class)
	    ),
	    (   get(CH, node, Super, SuperNode)
	    ->  true
	    ;   send(CH, add_class, Super),
		get(CH, node, Super, SuperNode)
	    ),
	    (   get(Class, sub_classes, _)
	    ->  new(Son, toc_folder(Class?name, Class, Image, Image))
	    ;   new(Son, toc_file(Class?name, Class, Image))
	    ),
	    send_class(SuperNode, node, collapsed(@off)),
	    send(CH, son, Super, Son)
	).

:- pce_global(@man_class_hierarchy_builtin_popup, make_popup(builtin)).
:- pce_global(@man_class_hierarchy_user_popup, make_popup(user)).

make_popup(Creator, P) :-
	new(CH, @arg1?window),
	new(Class, CH?selection),
	new(HasSubClasses, Class?sub_classes),
	new(P, popup(Creator, message(@arg2?window, @arg1))),
	send_list(P, append,
		  [ menu_item(documentation),
		    menu_item(class_browser),
		    menu_item(expand_tree,
			      condition := HasSubClasses,
			      end_group := @on)
		  ]),
	(   Creator == user
	->  send_list(P, append,
		      [ menu_item(source)
		      ])
	;   true
	),
	send(P, append, gap),
	send(P, append, prune).
		    
popup(_CH, Id:class, Popup:popup) :<-
	"Provide appropriate popup"::
	(   get(Id, creator, built_in)
	->  Popup = @man_class_hierarchy_builtin_popup
	;   Popup = @man_class_hierarchy_user_popup
	).

source(CH) :->
	"Show source of selected class"::
	get(CH?tree?selection, head, Node),
	get(Node, identifier, Class),
	send(CH, request_source, Class).
	
prune(CH) :->
	"Delete from the hierarchy"::
	get(CH?tree?selection, head, Node),
	send(Node, delete_tree).

selection(CH, Class:class) :<-
	"Get the selected class"::
	get(CH?tree?selection, head, Node),
	get(Node, identifier, Class).

class_browser(CH) :->
	"Show class browser on selected class"::
	get(CH, selection, Class),
	send(CH, request_tool_focus, Class).

documentation(CH) :->
	"Show source of selected class"::
	get(CH, selection, Class),
	send(CH, request_selection, Class, @on).

expand_tree(CH) :->
	"Expand everything below the selected node"::
	get(CH, selection, Class),
	expand_tree(CH, Class).

expand_tree(CH, Class) :-
	(   get(Class, sub_classes, Subs)
	->  send(CH, expand_node, Class),
	    send(Subs, for_all, message(@prolog, expand_tree, CH, @arg1))
	;   true
	).

:- pce_end_class.

:- pce_begin_class(man_class_hierarchy, man_frame,
		   "Tool for XPCE class hierarchy").

initialise(F, Manual:man_manual) :->
	send(F, send_super, initialise, Manual, 'Class Hierarchy'),
	send(F, append, new(CH, man_class_hierarchy_window(object))),
	send(new(D, dialog), below, CH),
	send(D, append, button(help)),
	send(D, append, button(quit, message(F, quit))),
	send(D, append, new(TI, text_item(class, '',
					  message(F, show_classes, @arg1))),
	     right),
	send(TI, value_set, ?(F, expand_classname, @arg1)),
	send(F, keyboard_focus, D).

expand_classname(_F, Prefix:name, Classes:chain) :<-
	new(Classes, chain),
	send(@classes, for_all,
	     if(message(@arg1, prefix, Prefix),
		message(Classes, append, @arg1))),
	send(Classes, sort).

show_classes(F, From:name) :->
	get(F, member, man_class_hierarchy_window, CH),
	new(R, regex('')),
	send(R, file_pattern, From),
	new(Nodes, chain),
	send(@classes, for_all,
	     if(message(R, match, @arg1),
		and(message(CH, add_class, @arg2),
		    message(Nodes, append, ?(CH, node, @arg2))))),
	send(CH?tree, selection, Nodes),
	send(CH, normalise, Nodes).

focus(F, Class:class) :->
	get(F, member, man_class_hierarchy_window, CH),
%	send(CH, ensure_class, Class),
	send(CH, add_class, Class),
	get(CH, node, Class, Node),
	send(CH?tree, selection, Node),
	send(CH, normalise, Node).

:- pce_end_class.
