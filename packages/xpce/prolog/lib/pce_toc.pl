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

variable(nodes,	hash_table, get, "Id --> node mapping").

initialise(TW) :->
	"Create window and display empty toc_tree"::
	send(TW, send_super, initialise),
	send(TW, scrollbars, vertical),
	send(TW, hor_shrink, 0),
	send(TW, hor_stretch, 0),
	send(TW, slot, nodes, new(hash_table)),
	send(TW, display, new(toc_tree)).

:- pce_group(parts).

tree(TW, Tree:toc_tree) :<-
	"Get the toc_tree"::
	get(TW, member, toc_tree, Tree).

node(TW, Id:any, Node:toc_node) :<-
	"Find node from identifier"::
	get(TW, nodes, Table),
	get(Table, member, Id, Node).

:- pce_group(virtual).

open_node(_TW, _Id:any) :->
	"Called on double-click"::
	true.

select_node(_TW, _Id:any) :->
	"Called on single-click"::
	true.

expand_node(_TW, _Id:any) :->
	"Define expansion of node 'id'"::
	true.

:- pce_group(build).

root(TW, Root:toc_folder) :->
	"Assing the table a root"::
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

:- pce_end_class.


		 /*******************************
		 *	      TOC-TREE		*
		 *******************************/

:- pce_begin_class(toc_tree, tree,
		   "Tree to display table-of-contents").

initialise(TC) :->
	"Create the tree, setting style and geometry"::
	send(TC, send_super, initialise),
	send(TC, direction, list),
	send(TC, level_gap, 12).

root(TC, Root:toc_folder) :->
	"Assing the root"::
	send(TC, send_super, root, Root),
	send(TC?window?nodes, append, Root?identifier, Root).

:- pce_end_class.
	  

		 /*******************************
		 *      FOLDERS AND FILES	*
		 *******************************/

:- pce_global(@toc_node_format, make_toc_node_format).
:- pce_global(@toc_node_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@receiver, select)),
				click_gesture(left, c, single,
					      message(@receiver, select, @on)),
				click_gesture(left, '', double,
					      message(@receiver, open)),
				drag_and_drop_gesture(left, '', @default,
						      @arg1?drop_target)))).

make_toc_node_format(F) :-
	new(F, format(vertical, 1, @on)),
	send(F, row_sep, 5).

		 /*******************************
		 *	      TOC-NODE		*
		 *******************************/

:- pce_begin_class(toc_node, device, "TOC node object").

variable(identifier, any, 		get, "Identification handle").

initialise(TF, Label:char_array, Id:[any], Img:image) :->
	default(Id, Label, Identifier),
	send(TF, slot, identifier, Identifier),
	send(TF, send_super, initialise),
	send(TF, format, @toc_node_format),
	send(TF, display, bitmap(Img)),
	send(TF, display, text(Label, left, normal)).

unlink(TF) :->
	(   get(TF, window, Window)
	->  send(Window?nodes, delete, TF?identifier)
	;   true
	),
	send(TF, send_super, unlink).

:- pce_group(appearance).

image(TF, Img:image) :->
	"Modify image at the left"::
	get(TF, member, bitmap, BM),
	send(BM, image, Img).

font(TF, Font:font) :->
	"Modify the font"::
	get(TF, member, text, Text),
	send(Text, font, Font).

:- pce_group(event).

event(TF, Ev:event) :->
	(   send(TF, send_super, event, Ev)
	;   send(@toc_node_recogniser, event, Ev)
	).

:- pce_group(action).

select(TF, Modified:[bool]) :->
	(   Modified == @on
	->  send(TF, toggle_selected)
	;   send(TF?device, selection, TF)
	).

open(TF) :->
	send(TF?window, open_node, TF?identifier).

:- pce_group(drop).

drop_target(TF, DTG:'chain|any') :<-
	(   get(TF, selected, @on)
	->  get(TF?device, selection, Nodes),
	    get(Nodes, map, @arg1?identifier, DTG)
	;   get(TF, identifier, DTG)
	).

:- pce_end_class.

		 /*******************************
		 *	    TOC-FOLDER		*
		 *******************************/

:- pce_begin_class(toc_folder, toc_node, "TOC folder object").

variable(status,     {open,closed},	get, "Folder is opened?").

initialise(TF, Label:char_array, Id:[any], Img:[image]) :->
	default(Img, 'dir.bm', I),
	send(TF, send_super, initialise, Label, Id, I).

:- pce_group(build).

son(TF, Son:toc_node) :->
	send(TF?window?nodes, append, Son?identifier, Son),
	send(TF?node, son, Son).

status(TF, Status:{open,closed}) :->
	(   get(TF, status, Status)
	->  true
	;   send(TF, slot, status, Status),
	    (	Status == closed
	    ->	send(TF?node?sons, for_all, message(@arg1, delete_tree))
	    ;	send(TF?display, busy_cursor),
	        ignore(send(TF?window, expand_node, TF?identifier)),
		send(TF?display, busy_cursor, @nil)
	    )
	).

:- pce_group(action).

select(TF, Modified:[bool]) :->
	(   Modified \== @on
	->  send(TF?device, selection, @nil),
	    (   get(TF, status, open)
	    ->  send(TF, status, closed)
	    ;   send(TF, status, open)
	    )
	;   send(TF, send_super, select, Modified)
	).


:- pce_end_class.

		 /*******************************
		 *	      TOC-FILE		*
		 *******************************/

:- pce_begin_class(toc_file, toc_node, "TOC file object").

initialise(TF, Label:char_array, Id:[any], Img:[image]) :->
	default(Img, 'file.bm', I),
	send(TF, send_super, initialise, Label, Id, I).

:- pce_group(build).

son(TF, _Son:toc_node) :->
	send(TF, report, error, 'Cannot add sons to a file'),
	fail.

:- pce_group(action).

select(TF, Modified:[bool]) :->
	send(TF, send_super, select, Modified),
	send(TF?window, select_node, TF?identifier).

:- pce_end_class.
