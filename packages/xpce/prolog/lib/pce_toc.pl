/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(pce_toc, []).
:- use_module(library(pce)).
:- use_module(library(pce_unclip)).
:- require([ default/3
	   , ignore/1
	   ]).

:- pce_autoload(drag_and_drop_gesture, library(dragdrop)).

resource(file,		image, image('16x16/doc.xpm')).
resource(opendir,	image, image('opendir.xpm')).
resource(closedir,	image, image('closedir.xpm')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Status and aim
==============

This is the  first  version  of   an  XPCE/Prolog  library  for managing
hierarchies in a similar fashion as many Windows(tm) tools.

The current version is not well   prepared for modifyable structures. It
is designed for the contents browser of  the SWI-Prolog manual, but with
the intention to grow into a more widely usable library.

The objective is that the   application programmer subclasses toc_window
and (re)defines the virtual methods  there   to  tailor  this package to
his/her application. The other classes in   this package should normally
not be affected.

Typical usage
=============

	:- pce_autoload(toc_window, library(pce_toc)).

	:- pce_begin_class(directory_hierarchy, toc_window,
			   "Browser for a directory-hierarchy").

	initialise(FB, Root:directory) :->
		send_super(FB, initialise),
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

:- pce_begin_class(toc_window(name), window,
		   "Window for table-of-contents").

variable(drag_and_drop,	bool := @off, get, "Allow drag-and-drop").

initialise(TW) :->
	"Create window and display empty toc_tree"::
	send(TW, send_super, initialise),
	send(TW, scrollbars, both),
	send(TW, hor_shrink, 0),
	send(TW, hor_stretch, 1),
	send(TW, display, new(toc_tree), point(10, 5)).

:- pce_group(parts).

tree(TW, Tree:toc_tree) :<-
	"Get the toc_tree"::
	get(TW, member, toc_tree, Tree).


root(TW, Root:node) :<-
	"Get the root-node of the tree"::
	get(TW, member, toc_tree, Tree),
	get(Tree, root, Root).


selection(TW, Nodes:chain) :<-
	"Return	new chain holding selected nodes"::
	get(TW, member, toc_tree, Tree),
	get(Tree, selection, Nodes).

selection(TW, Nodes:'any|chain*') :->
	"Set selected nodes"::
	get(TW, member, toc_tree, Tree),
	send(Tree, selection, Nodes).

node(TW, Id:any, Node:toc_node) :<-
	"Find node from identifier"::
	get(TW, member, toc_tree, Tree),
	get(Tree, nodes, Table),
	(   get(Table, member, Id, Node)
	->  true
	;   send(Id, instance_of, toc_node),
	    Node = Id
	).

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
	send(Node, slot, collapsed, @off).

popup(_TW, _Id:any, _Popup:popup) :<-
	"Return a menu for this node"::
	fail.

:- pce_group(build).

root(TW, Root:toc_folder, Relink:[bool]) :->
	"Assign the table a root"::
	get(TW, tree, Tree),
	send(Tree, root, Root, Relink).

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

normalise_tree(TW, Id:any) :->
	"Make as much as possible of the subtree visible"::
	get(TW, node, Id, Node),
	(   get(Node, sons, Sons),
	    Sons \== @nil
	->  send(TW, compute),		% ensure proper layout
	    get(Sons, map, @arg1?image, Grs),
	    send(Grs, append, Node?image),
	    send(TW, normalise, Grs, y)	% class-variable?
	;   true
	).

:- pce_group(event).

:- pce_global(@toc_window_recogniser,
	      make_toc_window_recogniser).

make_toc_window_recogniser(G) :-
	new(C, click_gesture(left, '', single,
			     message(@receiver, selection, @nil))),
	new(KB, key_binding(toc_window)),
	send_list(KB,
		  [ function(page_up,
			     message(@receiver, scroll_vertical, backwards,
				     page, 900)),
		    function(page_down,
			     message(@receiver, scroll_vertical, forwards,
				     page, 900)),
		    function(cursor_home,
			     message(@receiver, scroll_vertical, goto,
				     file, 0)),
		    function(end,
			     message(@receiver, scroll_vertical, goto,
				     file, 1000))
		  ]),
	new(G, handler_group(C, KB)).

event(TW, Ev:event) :->
	"Handle key-bindings"::
	(   send_super(TW, event, Ev)
	;   send(@toc_window_recogniser, event, Ev)
	).


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

:- pce_end_class(toc_window).


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

root(TC, Root:toc_node, Relink:[bool]) :->
	"Assign the root"::
	send_super(TC, root, Root, Relink),
	send(TC?nodes, append, Root?identifier, Root).

selection(TC, SelectedNodes:chain) :<-
	"Find all toc_nodes that are selected"::
	get(TC?contains, find_all, @arg1?selected == @on, SelectedNodes).

selection(TC, Selection:'any|graphical|chain*') :->
	"Select the given nodes"::
	send(TC, compute),
	(   send(Selection, instance_of, chain)
	->  get(Selection, map, ?(TC, node_image, @arg1), Graphicals),
	    send_super(TC, selection, Graphicals)
	;   Selection == @nil
	->  send_super(TC, selection, Selection)
	;   get(TC, node_image, Selection, Gr)
	->  send_super(TC, selection, Gr)
	).
	    
node(TC, From:any, Node:toc_node) :<-
	"Get node from node or ID"::
	(   send(From, instance_of, toc_node)
	->  Node = From
	;   get(TC?nodes, member, From, Node)
	).

node_image(TC, From:any, Gr:graphical) :<-
	"Get node image from graphical, node or ID"::
	(   send(From, instance_of, graphical)
	->  Gr = From
	;   send(From, instance_of, toc_node)
	->  get(From, image, Gr)
	;   get(TC?nodes, member, From, Node),
	    get(Node, image, Gr)
	).

:- pce_end_class(toc_tree).
	  

:- pce_begin_class(toc_node, node,
		   "Node for the table-of-contents package").

variable(identifier, [any], 		none, "Identification handle").

initialise(TN, Id:any, Image:toc_image) :->
	send(TN, slot, identifier, Id),
	send(TN, send_super, initialise, Image).


identifier(TN, Id:any) :<-
	"Get given identifier or <-self"::
	get(TN, slot, identifier, Id0),
	(   Id0 == @default
	->  Id = TN
	;   Id = Id0
	).


son(TN, Son:toc_node) :->
	"Add a son below this node"::
	send(TN, send_super, son, Son),
	get(Son, identifier, Id),
	get(TN?tree, nodes, Nodes),
	send(Nodes, append, Id, Son).


unlink(TN) :->
	(   get(TN, tree, Tree),
	    Tree \== @nil,
	    get(Tree, nodes, Table),
	    get(TN, identifier, Id),
	    send(Table, delete, Id)
	->  true
	;   true
	),
	send(TN, send_super, unlink).


collapsed(Node, Val:bool*) :->
	"Switch collapsed mode"::
	(   get(Node, collapsed, Val)
	->  true
	;   (	Val == @on
	    ->	send(Node, hide_sons)
	    ;	Val == @off
	    ->	get(Node?tree, window, TocWindow),
		get(Node, identifier, Id),
		(   get(TocWindow, display, Display)
		->  send(Display, busy_cursor),
		    ignore(send(TocWindow, expand_node, Id)),
		    send(Display, busy_cursor, @nil)
		;   ignore(send(TocWindow, expand_node, Id))
		),
		Normalise = true
	    ;	true
	    ),
	    (	object(Node)
	    ->  send_super(Node, collapsed, Val),
		send(Node, update_image),
		(   Normalise == true
		->  send(TocWindow, normalise_tree, Node)
		;   true
		)
	    ;	true
	    )
	).

hide_sons(Node) :->
	"Hide (delete) sons on a collapse"::
	send(Node?sons, for_all, message(@arg1, delete_tree)).

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
	send(TF?image?graphicals, for_all,
	     if(message(@arg1, has_send_method, font),
		message(@arg1, font, Font))).

update_image(_) :->
	true.

:- pce_group(action).

select(Node, Modified:[bool]) :->
	(   Modified == @on
	->  send(Node, toggle_selected)
	;   get(Node, tree, Tree),
	    send(Tree, selection, Node?image),
	    send(Node, flush),
	    send(Tree?window, select_node, Node?identifier)
	).


open(Node) :->
	send(Node?window, open_node, Node?identifier).

:- pce_end_class(toc_node).


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
					      message(@toc_node, open)),
				handler(ms_right_down,
					and(message(@toc_node, select),
					    new(or))),
				popup_gesture(?(@receiver?window, popup,
						@toc_node?identifier)),
				handler(area_enter,
					message(@receiver, entered, @on)),
				handler(area_exit,
					message(@receiver, entered, @off))))).


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

initialise(TF, Label:'char_array|graphical', Img:image) :->
	send(TF, send_super, initialise),
	send(TF, format, @toc_node_format),
	send(TF, display, bitmap(Img)),
	(   send(Label, instance_of, char_array)
	->  new(Gr, text(Label, left, normal))
	;   Gr = Label
	),
	send(Gr, name, label),
	send(TF, display, Gr).

selected(TF, Sel:bool) :->
	get(TF, member, label, Text),
	send(Text, selected, Sel).
selected(TF, Sel:bool) :<-
	get(TF, member, label, Text),
	get(Text, selected, Sel).

label_text(TF, Text:graphical) :<-
	"Get graphical rendering the <-label"::
	get(TF, member, label, Text).

label(TF, Label:'char_array|graphical') :->
	"Modify the textual label"::
	get(TF, label_text, Text),
	(   send(Label, instance_of, char_array)
	->  send(Text, string, Label)
	;   free(Text),
	    send(TF, display, Label),
	    send(Label, name, label)
	).
label(TF, Label:'char_array|graphical') :<-
	"Get the textual label"::
	get(TF, label_text, Text),
	(   send(Text, has_get_method, string)
	->  get(Text, string, Label)
	;   Label = Text
	).

image(TF, Image:image) :->
	"Modify the icon"::
	get(TF, member, bitmap, BM),
	send(BM, image, Image).
image(TF, Image:image) :<-
	"Get the icon"::
	get(TF, member, bitmap, BM),
	get(BM, image, Image).

:- pce_group(event).

event(TF, Ev:event) :->
	(   send_super(TF, event, Ev)
	;   send(@toc_node_recogniser, event, Ev)
	).

:- pce_group(window).

entered(TF, Val:bool) :->
	(   Val == @on,
	    (	send(TF, clipped_by_window)
	    ->	send(@unclip_window, attach, TF)
	    ;	true
	    )
	;   true
	).

:- pce_group(drop).

drop_target(TF, DTG:'chain|any') :<-
	(   get(TF, selected, @on)
	->  get(TF?device, selection, Nodes),
	    get(Nodes, map, @arg1?identifier, DTG)
	;   get(TF?node, identifier, DTG)
	).

:- pce_end_class(toc_image).

image(folder, @off, resource(opendir)) :- !.
image(folder, _,    resource(closedir)).


		 /*******************************
		 *	    TOC-FOLDER		*
		 *******************************/

:- pce_begin_class(toc_folder, toc_node, "TOC folder object").

variable(collapsed_image,	[image], get, "Icon if collapsed [+]").
variable(expanded_image,	[image], get, "Icon if expanded [-]").

initialise(TF,
	   Label:label='char_array|graphical',
	   Id:identifier=[any],
	   CollapsedImg:collapsed_image=[image],
	   ExpandedImg:expanded_image=[image],
	   CanExpand:can_expand=[bool]) :->
	send(TF, slot, collapsed_image, CollapsedImg),
	default(ExpandedImg, CollapsedImg, TheExpandedImg),
	send(TF, slot, expanded_image, TheExpandedImg),
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

collapsed_image(TF, Img:[image]) :->
	"Image in collapsed state"::
	send(TF, slot, collapsed_image, Img),
	send(TF, update_image).

expanded_image(TF, Img:[image]) :->
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

initialise(TF, Label:'char_array|graphical', Id:[any], Img:[image]) :->
	default(Img, resource(file), I),
	send(TF, send_super, initialise, Id, toc_image(Label, I)),
	send(TF, collapsed, @nil).

:- pce_group(build).

son(TF, _Son:toc_node) :->
	send(TF, report, error, 'Cannot add sons to a file'),
	fail.

:- pce_end_class.
