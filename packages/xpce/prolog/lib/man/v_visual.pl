/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_visual, []).

:- use_module(library(pce)).
:- ensure_loaded(library(pce_selection)).
:- require([ concat_atom/2
	   , ignore/1
	   , portray_object/2
	   , send_list/3
	   , shell/1
	   , term_to_atom/2
	   ]).

:- pce_autoload(tile_hierarchy, library('man/v_tile')).

		/********************************
		*        ICON GENERATION	*
		********************************/

:- pce_extend_class(visual).

vis_image(V, Node) :<-
	new(Node, text(V?class_name)).

:- pce_end_class.

		/********************************
		*             NODE		*
		********************************/

:- pce_begin_class(vis_node, node).

variable(visual,	visual*,	get, "Visual object represented").

initialise(N, Gr:graphical, V:visual) :->
	send(N, send_super, initialise, Gr),
	send(N, slot, visual, V).


unlink(N) :->
	ignore(send(N?frame, selection, @nil)),
	ignore(send(N?window?nodes, delete, N?visual)),
	send(N, send_super, unlink).


expand(N) :->
	"Expand to the next level"::
	get(N, visual, Visual),
	get(N, tree, Tree),
	(   get(Visual, frame, VFrame),
	    get(Tree, frame, VFrame)
	->  send(Visual, report, warning, 'Can''t visualise myself')
	;   get(Visual, contains, Sons),
	    send(Sons, for_all, message(N, son, @arg1)),
	    send(Tree?window, normalise, N?sons),
	    send(Sons, done)
	).


tile_hierarchy(N) :->
	"Display a frame's tile hierarchy"::
	get(N, visual, Visual),
	send(Visual, instance_of, frame),
	get(Visual, tile, Root),
	new(_, tile_hierarchy(Root)).


son(N, V:visual) :->
	(    get(N?sons, find, @arg1?visual == V, _)
	->   true
	;    send(N, send_super, son, new(S, vis_node(V?vis_image, V))),
	     send(N?tree?window, prepare, S, V)
	).



expand_tree(N) :->
	"Expand all levels"::
	(   send(N, expand)
	->  send(N?sons, for_some, message(@arg1, expand_tree))
	;   true
	).


collapse(N) :->
	"Destroy all subtrees"::
	send(N?frame, selection, N),
	send(N?sons, for_all, message(@arg1, delete_tree)).

delete(N) :->
	"Delete subtree"::
	send(N?frame, selection, @nil),
	send(N, delete_tree).


:- pce_end_class.

		/********************************
		*            TREE		*
		********************************/

:- pce_global(@vis_node_handler, make_vis_node_handler).

make_vis_node_handler(H) :-
	new(Text, @event?receiver),
	new(Node, Text?node),
	new(Visual, Node?visual),
	new(CanFlash, ?(Visual?class, send_method, flash)),
	new(CanExpand, ?(Visual, contains)),
	new(Tool, Node?frame),

	new(H, handler_group(
		popup_gesture(new(P, popup)),
		click_gesture(left, '', single,
			      and(message(Tool, selection, Node),
				  message(Tool, portray, Visual),
				  if(CanFlash,
				     message(Visual, flash)))),
		click_gesture(left, '', double,
			      and(message(Tool, selection, Node),
				  message(Node, expand))))),
	
	send_list(P, append,
		  [ menu_item(flash,
			      block(message(Tool, selection, Node),
				    message(Visual, flash)),
			      @default, @on,
			      CanFlash)
		  , menu_item(expand,
			      block(message(Tool, selection, Node),
				    message(Node, expand)),
			      @default, @off,
			      CanExpand)
		  , menu_item(expand_tree,
			      block(message(Tool, selection, Node),
				    message(Node, expand_tree)),
			      @default, @off,
			      CanExpand)
		  , menu_item(collapse,
			      message(Node, collapse),
			      @default, @off,
			      not(message(Node?sons, empty)))
		  , menu_item(delete,
			      message(Node, delete),
			      @default, @on,
			      Visual \== @display_manager)
		  , menu_item(source,
			      and(message(Tool, selection, Node),
				  message(@manual, request_source,
					  Visual?class)))
		  , menu_item(inspect,
			      and(message(Tool, selection, Node),
				  message(@manual, inspect, Visual)),
			      @default, @off)
		  , menu_item(tile_hierarchy,
			      and(message(Tool, selection, Node),
				  message(Node, tile_hierarchy)),
			      @default, @off,
			      message(Visual, instance_of, frame))
		  , menu_item(class_browser,
			      and(message(Tool, selection, Node),
				  message(Tool, request_tool_focus,
					  Visual?class)),
			      @default, @on)
		  , menu_item(free,
			      block(message(Tool, selection, Node),
				    message(Visual, free)),
			      @default, @on,
			      Visual?protect == @off)
		  , menu_item(postscript,
			      block(message(Tool, selection, Node),
				    message(Tool, postscript, Visual)),
			      @default, @on)
		  ]).


:- pce_begin_class(vis_window, picture).

variable(freed_message, message,	get, "Message to trap destruction").
variable(nodes,		hash_table,	get, "V --> Node table").

resource(selection_feedback, any,
	 'when(@colour_display, colour(red), invert)').

initialise(W) :->
	send(W, send_super, initialise),
	send(W, display, new(T, tree)),
	send(W, slot, freed_message, message(W, freed, @arg2)),
	send(W, slot, nodes, new(hash_table)),
	send(T, node_handler, @vis_node_handler),
	send(T, root, new(Root, vis_node(@display_manager?vis_image,
					 @display_manager))),
	send(W?nodes, append, @display_manager, Root).
	

unlink(W) :->
	"Remove trap-messages from classes"::
	get(W, freed_message, Msg),
	send(@classes, for_some,
	     if(@arg2?freed_messages \== @nil,
		message(@arg2?freed_messages, delete, Msg))),
	send(W, send_super, unlink).


node(W, V:visual, Node) :<-
	(   get(W?nodes, member, V, Node)
	->  true
	;   get(V, contained_in, Super),
	    get(W, node, Super, SuperNode),
	    send(SuperNode, son, V),
	    get(W?nodes, member, V, Node)
	).


visual(W, V:visual) :->
	"Add Visual to the tree"::
	(   get(W?nodes, member, V, _)
	->  true
	;   get(V, contained_in, Super),
	    get(W, node, Super, SuperNode),
	    send(SuperNode, son, V)
	),
	get(W?nodes, member, V, Node),
	send(W?frame, selection, Node).


visual_atom(W, Text:string) :->
	"->visual from text_item"::
	(   get(Text, scan, '@%[a-zA-Z0-9_]', vector(string(Ref))),
	    (	object(@Ref)
	    ->	Obj = @Ref
	    ;	get(@pce, convert, Ref, int, IRef),
		object(@IRef)
	    ->	Obj = @IRef
	    ),
	    send(Obj, instance_of, visual)
	->  send(W, visual, Obj)
	;   send(@pce, inform, 'No such visual object: %s', Text),
	    fail
	).


prepare(W, N:vis_node, V:visual) :->
	"Prepare for destruction of visual"::
	send(W?nodes, append, V, N),
	get(V, '_class', Class),
	get(W, freed_message, Msg),
	send(Class, freed_message, Msg).


freed(W, V) :->
	get(W?nodes, member, V, Node),
	send(Node, delete_tree).

:- pce_end_class.


		/********************************
		*            MAIN TOOL		*
		********************************/

:- pce_begin_class(vis_frame, man_frame).

variable(selection,	vis_node*,	get, "Currently selected node").
variable(handler,	handler,	get, "Inspect handler used").

resource(postscript_print_command, name, lpr,
	 "Shell command to send a file to the printer").

initialise(F, Manual:man_manual) :->
	send(F, send_super, initialise, Manual, 'PCE Visual'),
	send(F, append, new(V, vis_window)),
	send(new(D, dialog), below, V),

	send(D, append, label(reporter,
			      'META-SHIFT-CONTROL-V adds object to tree')),
	send(D, append, button(help, message(F, help))),
	send(D, append, button(print, message(F, print))),
	send(D, append, button(quit, message(F, quit))),
	send(D, append,
	     new(TI, text_item(visual, '',
			       and(message(V, visual_atom, @arg1),
				   message(@receiver, clear)))),
	     right),
	send(TI, length, 20),
	send(F?display, inspect_handler,
	     new(H, handler('M-\C-v', message(V, visual, @arg1)))),
	send(F, slot, handler, H).


unlink(F) :->
	get(F?display, inspect_handlers, Chain),
	get(F, handler, H),
	send(Chain, delete_all, H),
	send(H, free),
	send(F, send_super, unlink).


window(F, Window) :<-
	get(F, member, vis_window, Window).


tree(F, Tree:tree) :<-
	"Tree displaying hierarchy"::
	get(F?window, member, tree, Tree).


selection(F, Node:vis_node*) :->
	(   get(F, selection, Old), Old \== @nil
	->  send(Old, selected, @off)
	;   true
	),
	send(F, slot, selection, Node),
	(   Node \== @nil
	->  send(Node, selected, @on),
	    get(Node, visual, @Ref),
	    new(Selection, string('@%s', Ref)),
	    send(F?display, copy, Selection),
	    send(F?dialog_member?visual_member, selection, Selection),
	    send(F?window, normalise, Node)
	;   true
	).


print(F) :->
	"Print on Postscript printer"::
	new(File, file),
	send(File, open, write),
	send(File, append, F?tree?postscript),
	send(File, append, showpage),
	send(File, close),
	get(F, resource_value, postscript_print_command, Lpr),
	get(File, name, FileName),
	concat_atom([Lpr, ' ', FileName], Cmd),
	(   shell(Cmd)
	->  send(File, remove),
	    fail
	;   send(F, report, error, 'Command failed: %s', Cmd)
	).


postscript(_F, V:visual) :->
	"Create postscript description of visual on file PostScript"::
	new(File, file('PostScript')),
	send(File, open, write),
	send(File, append, V?postscript),
	send(File, close),
	send(File, done).


portray(F, V:visual) :->
	portray_object(V, Term),
	term_to_atom(Term, Atom),
	send(F, report, inform, Atom).

:- pce_end_class.
