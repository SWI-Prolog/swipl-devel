/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pce_toc, []).
:- use_module(library(pce)).
:- require([ default/3
	   , ignore/1
	   ]).

:- pce_autoload(drag_and_drop_gesture, library(dragdrop)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Status and aim
==============

This is the  first  version  of   an  XPCE/Prolog  library  for managing
hierarchies in a similar fashion as many Windows(tm) tools.

The current version is not well   prepared for modifyable structures. It
is desined for the contents browser of   the SWI-Prolog manual, but with
the intention to grow into a more widely usable library.

The objective is that the   application programmer subclasses toc_window
and (re)defines the virtual methods  there   to  tailure this package to
his/her application. The other classes in   this package should normally
not be affected.

Typical usage
=============

	:- pce_autoload(toc_window, library(pce_toc)).

	:- pce_begin_class(directory_hierarchy, toc_window,
			   "Browser for a directory-hierarchy").

	initialise(FB, Root:directory) :->
		send(FB, send_super, initialise),
		get(Root, name, Name),
		send(FB, root, toc_folder(Name, Root)).

	expand_node(FB, D:directory) :->
		get(D, directories, SubDirsNames),
		get(SubDirsNames, map, ?(D, directory, @arg1), SubDirs),
		send(SubDirs, for_all,
		     message(FB, son, D,
			     create(toc_folder, @arg1?name, @arg1))).

	:- pce_end_class.


	?- send(directory_hierarchy(~), open).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	    TOC-WINDOW		*
		 *******************************/

:- pce_begin_class(toc_window, window,
		   "Window for table-of-contents").

variable(drag_and_drop,	bool := @off, get, "Allow drag-and-drop").

initialise(TW) :->
	"Create window and display empty toc_tree"::
	send(TW, send_super, initialise),
	send(TW, scrollbars, vertical),
	send(TW, hor_shrink, 0),
	send(TW, hor_stretch, 0),
	send(TW, display, new(toc_tree), point(10, 5)).

:- pce_group(parts).

tree(TW, Tree:toc_tree) :<-
	"Get the toc_tree"::
	get(TW, member, toc_tree, Tree).

node(TW, Id:any, Node:toc_node) :<-
	"Find node from identifier"::
	get(TW, member, toc_tree, Tree),
	get(Tree, nodes, Table),
	get(Table, member, Id, Node).

:- pce_group(virtual).

open_node(_TW, _Id:any) :->
	"Called on double-click"::
	true.

select_node(_TW, _Id:any) :->
	"Called on single-click"::
	true.

expand_node(TW, Id:any) :->
	"Define expansion of node 'id'"::
	get(TW, node, Id, Node),
	send(Node, slot, collapsed, @off). % HAXK!!

:- pce_group(build).

root(TW, Root:toc_folder) :->
	"Assign the table a root"::
	get(TW, tree, Tree),
	send(Tree, root, Root).

son(TW, Parent:any, Son:toc_node) :->
	"Add a son to a node"::
	get(TW, node, Parent, Node),
	send(Node, son, Son).

delete(TW, Id:any) :->
	"Delete node (and subnodes)"::
	get(TW, node, Id, Node),
	send(Node?node, delete_tree).

expand_root(T) :->
	"Expand the root-node"::
	get(T?tree, root, Node),
	ignore(send(Node, collapsed, @off)).

:- pce_group(scroll).

scroll_vertical(TW,
		Direction:{forwards,backwards,goto},
		Unit:{page,file,line},
		Amount:int) :->
	"Prevent scrolling too far"::
	get(TW, visible, VA),
	get(TW, bounding_box, BB),
	(   send(VA, inside, BB)
	->  true
	;   Direction == backwards,
	    get(VA, y, Y),
	    Y < 1
	->  true
	;   Direction == forwards,
	    get(BB, bottom_side, BBBottom),
	    get(VA, bottom_side, VABottom),
	    VABottom > BBBottom
	->  true
	;   send(TW, send_super, scroll_vertical, Direction, Unit, Amount),
	    get(TW, visible, area(_, AY, _, _)),
	    (   AY < 0
	    ->  send(TW, scroll_to, point(0,0))
	    ;   true
	    )
	).

:- pce_group(event).

drag_and_drop(TW, Val:bool) :->
	"(dis)allow drag-and-drop"::
	send(TW, slot, drag_and_drop, Val),
	(   Val == @on
	->  (   send(@toc_node_recogniser?members, member,
		     @toc_drag_and_drop_recogniser)
	    ->	true
	    ;	send(@toc_node_recogniser?members, append,
		     @toc_drag_and_drop_recogniser)
	    )
	).

:- pce_end_class.


		 /*******************************
		 *	      TOC-TREE		*
		 *******************************/

:- pce_begin_class(toc_tree, tree,
		   "Tree to display table-of-contents").

variable(nodes,	hash_table, get, "Id --> node mapping").

initialise(TC) :->
	"Create the tree, setting style and geometry"::
	send(TC, slot, nodes, new(hash_table)),
	send(TC, send_super, initialise),
	send(TC, direction, list),
	send(TC, level_gap, 17).

root(TC, Root:toc_folder) :->
	"Assing the root"::
	send(TC, send_super, root, Root),
	send(TC?nodes, append, Root?identifier, Root).

selection(TC, SelectedNodes:chain) :<-
	"Find all toc_nodes that are selected"::
	get(TC?contains, find_all, @arg1?selected == @on, SelectedNodes).
	

:- pce_end_class.
	  

:- pce_begin_class(toc_node, node,
		   "Node for the table-of-contents package").

variable(identifier, any, 		get, "Identification handle").

initialise(TN, Id:any, Image:toc_image) :->
	send(TN, slot, identifier, Id),
	send(TN, send_super, initialise, Image).


son(TN, Son:toc_node) :->
	send(TN, send_super, son, Son),
	get(Son, identifier, Id),
	get(TN?tree, nodes, Nodes),
	send(Nodes, append, Id, Son).


unlink(TN) :->
	(   get(TN, tree, Tree),
	    Tree \== @nil
	->  get(Tree, nodes, Table),
	    get(TN, identifier, Id),
	    send(Table, delete, Id)
	;   true
	),
	send(TN, send_super, unlink).


collapsed(Node, Val:bool*) :->
	"Switch collapsed mode"::
	(   get(Node, collapsed, Val)
	->  true
	;   (	Val == @on
	    ->	send(Node?sons, for_all, message(@arg1, delete_tree))
	    ;	Val == @off
	    ->	get(Node?tree, window, TocWindow),
		get(Node, identifier, Id),
		(   get(TocWindow, display, Display)
		->  send(Display, busy_cursor),
		    ignore(send(TocWindow, expand_node, Id)),
		    send(Display, busy_cursor, @nil)
		;   ignore(send(TocWindow, expand_node, Id))
		)
	    ;	true
	    ),
	    send(Node, send_super, collapsed, Val),
	    send(Node, update_image)
	).

can_expand(TF, Val:bool) :->
	"Whether or not the node can be expanded"::
	(   Val == @off
	->  send(TF, send_super, collapsed, @nil)
	;   send(TF, send_super, collapsed, @on)
	).

:- pce_group(appearance).

image(TF, Img:image) :->
	"Modify image at the left"::
	get(TF, member, bitmap, BM),
	send(BM, image, Img).

font(TF, Font:font) :->
	"Modify the font"::
	get(TF, member, text, Text),
	send(Text, font, Font).

update_image(_) :->
	true.

:- pce_group(action).

select(Node, Modified:[bool]) :->
	(   Modified == @on
	->  send(Node, toggle_selected)
	;   get(Node, tree, Tree),
	    send(Tree, compute),
	    send(Tree, selection, Node?image),
	    send(Node, flush),
	    send(Tree?window, select_node, Node?identifier)
	).


open(Node) :->
	send(Node?window, open_node, Node?identifier).

:- pce_end_class.


		 /*******************************
		 *      FOLDERS AND FILES	*
		 *******************************/

:- pce_global(@toc_node_format, make_toc_node_format).
:- pce_global(@toc_node, new(@receiver?node)).
:- pce_global(@toc_node_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@toc_node, select)),
				click_gesture(left, c, single,
					      message(@toc_node, select, @on)),
				click_gesture(left, '', double,
					      message(@toc_node, open))))).


:- pce_global(@toc_drag_and_drop_recogniser,
	      make_toc_drag_and_drop_recogniser).

make_toc_drag_and_drop_recogniser(G) :-
	new(G, drag_and_drop_gesture(left, '', @default,
				     @arg1?drop_target)),
	send(G, condition, @event?window?drag_and_drop == @on).
	
make_toc_node_format(F) :-
	new(F, format(vertical, 1, @on)),
	send(F, row_sep, 5).

		 /*******************************
		 *	     TOC-IMAGE		*
		 *******************************/

:- pce_begin_class(toc_image, device, "TOC node object").

initialise(TF, Label:char_array, Img:image) :->
	send(TF, send_super, initialise),
	send(TF, format, @toc_node_format),
	send(TF, display, bitmap(Img)),
	send(TF, display, text(Label, left, normal)).

selected(TF, Sel:bool) :->
	get(TF, member, text, Text),
	send(Text, selected, Sel).
selected(TF, Sel:bool) :<-
	get(TF, member, text, Text),
	get(Text, selected, Sel).

label(TF, Label:char_array) :->
	"Modify the textual label"::
	get(TF, member, text, Text),
	send(Text, string, Label).
label(TF, Label:char_array) :<-
	"Get the textual label"::
	get(TF, member, text, Text),
	get(Text, string, Label).

image(TF, Image:image) :->
	"Modify the icon"::
	get(TF, member, bitmap, BM),
	send(BM, image, Image).
image(TF, Image:image) :->
	"Get the icon"::
	get(TF, member, bitmap, BM),
	get(BM, image, Image).

:- pce_group(event).

event(TF, Ev:event) :->
	(   send(TF, send_super, event, Ev)
	;   send(@toc_node_recogniser, event, Ev)
	).

:- pce_group(drop).

drop_target(TF, DTG:'chain|any') :<-
	(   get(TF, selected, @on)
	->  get(TF?device, selection, Nodes),
	    get(Nodes, map, @arg1?identifier, DTG)
	;   get(TF?node, identifier, DTG)
	).

:- pce_end_class.

image(folder, _, 'dir.bm') :-
	get(@display, visual_type, monochrome), !.
image(folder, @off, 'opendir.xpm') :- !.
image(folder, _,    'closedir.xpm').


		 /*******************************
		 *	    TOC-FOLDER		*
		 *******************************/

:- pce_begin_class(toc_folder, toc_node, "TOC folder object").

variable(collapsed_image,	[image], get, "Icon if collapsed [+]").
variable(expanded_image,	[image], get, "Icon if expanded [-]").

initialise(TF,
	   Label:label=char_array,
	   Id:identifier=[any],
	   CollapsedImg:image=[image],
	   ExpandedImg:image=[image],
	   CanExpand:can_expand=[bool]) :->
	send(TF, slot, collapsed_image, CollapsedImg),
	send(TF, slot, expanded_image, ExpandedImg),
	(   CollapsedImg == @default
	->  image(folder, closed, I)
	;   I = CollapsedImg
	),
	send(TF, send_super, initialise, Id, toc_image(Label, I)),
	(   CanExpand == @off
	->  send(TF, collapsed, @nil)
	;   send(TF, collapsed, @on)
	).

:- pce_group(appearance).

collapsed_image(TF, Img:image) :->
	"Image in collapsed state"::
	send(TF, slot, collapsed_image, Img),
	send(TF, update_image).

expanded_image(TF, Img:image) :->
	"Image in expanded state"::
	send(TF, slot, expanded_image, Img),
	send(TF, update_image).


:- pce_group(open).

update_image(TF) :->
	"Update image after status change"::
	get(TF, collapsed, Val),
	(   Val == @off
	->  get(TF, expanded_image, Img0)
	;   get(TF, collapsed_image, Img0)
	),
	(   Img0 == @default
	->  image(folder, Val, Img)
	;   Img = Img0
	),
	send(TF, image, Img).

:- pce_group(action).

open(TF) :->
	get(TF, node, Node),
	get(Node, collapsed, Collapsed),
	(   Collapsed == @on
	->  send(Node, collapsed, @off)
	;   Collapsed == @off
	->  send(Node, collapsed, @on)
	;   send(Node, send_super, open)
	).

:- pce_end_class.

		 /*******************************
		 *	      TOC-FILE		*
		 *******************************/

:- pce_begin_class(toc_file, toc_node, "TOC file object").

initialise(TF, Label:char_array, Id:[any], Img:[image]) :->
	default(Img, 'file.bm', I),
	send(TF, send_super, initialise, Id, toc_image(Label, I)),
	send(TF, collapsed, @nil).

:- pce_group(build).

son(TF, _Son:toc_node) :->
	send(TF, report, error, 'Cannot add sons to a file'),
	fail.

:- pce_end_class.
