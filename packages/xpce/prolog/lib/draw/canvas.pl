/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(draw_canvas, []).

:- use_module(library(pce)).
:- use_module(align).
:- require([ add_config/2
	   , chain_list/2
	   , default/3
	   , file_name_extension/3
	   , forall/2
	   , get_config/2
	   , ignore/1
	   , pce_shell_command/1
	   , send_list/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `draw_canvas' defines  the actual drawing  area.  Representing a
collection  of graphicals, the  closest PCE  class is   `picture'.  In
addition  to pictures, class   draw_canvas  takes care of the  current
mode, the  current prototype, the  file (for  loading  and saving  the
image) and an editor for changing attributes of graphical objects.

For editing  the drawing,  two  variables have been added:  `mode' and
`proto'.   `Mode' is an indication of  the current  mode.  The various
gestures defined in  the file `gesture'  are only active in predefined
modes.  They can access the current mode with:

	@event?receiver?window?mode

For  modes that  create  objects,   the variable `proto'   contains  a
prototype of the object to be created.  Instances of the prototype are
created   using `Object <-clone',     except   for  links,  which  are
instantiated by creating a connection from them.

The variables <->file and <-modified are used to implement  ->save and
->load.

NOTE:	Modified   is a difficult  issue.  It   should  be set  by all
	operations  that   change  anything  to the    contents of the
	diagram.  Maybe it is better to implement  a modified variable
	at the level of window, or implement a message that allows the
	programmer to keep track of actions on the picture.

The attribute_editor is a reference to  an editor that allows the user
to change the attributes of the graphicals in the selection.

NOTE:	Should we  define  the  type  of the attribute_editor   to  be
	`draw_attribute_editor*' or rather `object*' and just rely the
	attribute editor has the appropriate methods to facilitate the
	communication?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_canvas, picture, "Drawing plane of PceDraw").

variable(mode,		   name,			get,
	 "Current mode of operation").
variable(proto,		   object*,			both,
	 "Current create prototype (graphical/link)").
variable(file,		   file*,			get,
	 "Current save/load file").
variable(modified,	   bool,			get,
	 "Has the contents been modified?").
variable(auto_align_mode,   bool,			both,
	 "Autoalign graphicals after create/resize").
variable(attribute_editor, draw_attribute_editor*,	both,
	 "Editor handling attributes").
variable(undo_buffer,	   draw_undo_manager,	        get,
	 "Records undo actions").



		/********************************
		*           INITIALISE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->initialise initialises the picture and custom  slots that should not
be @nil.  It also attaches  an event recogniser  to the picture.  Note
that there are two ways to attach an event recogniser to a picture.

The first  is to  attach a recogniser  using the `Object ->recogniser'
method. In this case, the object is  extended with  an interceptor and
the recogniser is attached to this interceptor.   Recognisers attached
to an interceptor are activated by the `Graphical ->event'.

The second is  to  define a  method ->event.   This method may  either
decide to  decode the events itself,  or  leave this to  a recogniser.
These    approaches are used  in the   file   shapes.pl to make shapes
sensitive to user actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(Canvas) :->
	"Create a drawing canvas"::
	send(Canvas, send_super, initialise, 'Canvas'),
	send(Canvas, selection_feedback, handles),
	send(Canvas, slot, undo_buffer, draw_undo_manager(Canvas, 100)),
	send(Canvas, slot, modified, @off),
	send(Canvas, auto_align_mode, @off),
	send(Canvas, mode, select, @nil),
	send(Canvas, recogniser, @draw_canvas_recogniser).

		 /*******************************
		 *        EVENT HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The recogniser itself is  a  reusable object  (which   implies   other
instances of draw_canvas can use the same instance of the recogniser).
For this reason, it  is declared using pce_global/2.   The  first time
the recogniser reference is passed to PCE, the  interface will trap an
exception and create the object  using  the declaration in  this file.
This approach will delay the creation of the reusable object  until it
is really  necessary  and avoids conditions  in  the  code  (i.e.  `if
object does not exist then create it' just before it is used).

NOTE:	I'm  not sure whether  or not  it is better  to a) Declare the
	global objects in gesture.pl and just refer to them here or b)
	Just declare the classes there and create instances here.

	Both  approaches have their advantages.    The first  approach
	guarantees maximal reuse.  Actually there is only one instance
	of each gesture class and one may advocate it is better to use
	object-level programming to  create  this sole instance.   PCE
	should offer

		:- pce_begin_object(NewTerm).
		...
		:- pce_end_object.

	similar to defining classes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_canvas_popup, make_draw_canvas_popup).

make_draw_canvas_popup(P) :-
	new(Canvas, @event?receiver),
	new(P, popup),
	send_list(P, append,
		  [ menu_item(select_mode,
			      message(Canvas?frame?menu, activate_select),
			      end_group := @on),
		    menu_item(paste,
			      message(Canvas, paste,
				      Canvas?current_event?position))
		  ]).


:- pce_global(@draw_canvas_keybinding, make_draw_canvas_keybinding).

canvas_keybinding('DEL',   cut_selection).
canvas_keybinding('\\C-a', select_all).
canvas_keybinding('\\C-h', hide_selection).
canvas_keybinding('\\C-e', expose_selection).
canvas_keybinding('\\C-c', copy_selection).
canvas_keybinding('\\C-v', paste).
canvas_keybinding('\\C-_', simple_undo).
canvas_keybinding('\\C-z', simple_undo).
canvas_keybinding('\\C-s', save).
canvas_keybinding('\\C-p', print).
canvas_keybinding('TAB',   start_typing).

make_draw_canvas_keybinding(B) :-
	new(B, key_binding),
	forall(canvas_keybinding(Key, Method),
	       send(B, function, Key, Method)).


:- pce_global(@draw_canvas_recogniser, make_draw_canvas_recogniser).

make_draw_canvas_recogniser(G) :-
	new(KBFocus, @event?window?keyboard_focus),
	new(ST, handler(keyboard,
			message(@receiver, start_typing, @event))),
	new(EX, handler(area_exit,
			message(@event?window, keyboard_focus, @nil))),
	new(G, handler_group(@draw_create_resize_gesture,
			     @draw_create_line_gesture,
			     @draw_create_bezier_gesture,
			     @draw_create_path_gesture,
			     @draw_create_text_recogniser,
			     @draw_create_proto_recogniser,
			     @draw_warp_select_gesture,
			     @draw_canvas_keybinding,
			     new(P, click_gesture(middle, '', single,
						  message(KBFocus, paste))),
			     popup_gesture(@draw_canvas_popup),
			     ST, EX)),
	send(P, condition, KBFocus \== @nil).


start_typing(C, Id:event_id) :->
	"Start typing if the selection wants the keyboard"::
	get(C, keyboard_focus, @nil),
	(   get(C, selection, Selection),
	    get(Selection, size, 1),
	    get(Selection, head, Obj),
	    send(Obj, '_wants_keyboard_focus')
	->  send(C, selection, @nil),
	    send(C, keyboard_focus, Obj),
	    (   Id \== 9
	    ->  send(Selection?head, generate_event, Id)
	    ;   true
	    )
	;   Id == 9
	->  send(C, advance)
	).


		/********************************
		*            UNLINK		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->unlink behaviour  is called when  an object is  removed from the
PCE object base, either initiated through  `Object ->free', or through
the garbage collector.  `Object ->unlink' is responsible for unlinking
the object   from its environment.   For  example,  when  a  window is
unlinked it should inform X-windows; when a  graphical is unlinked, it
should  inform its device.  Removing an  object entails the  following
steps:

	1) Call ->unlink
	2) Reset all slots that have objects in them to @nil
	3) Reclaim the memory

Like ->initialise,   ->unlink    should invoke   the method   of   the
super-class.  Normally, it will  first do its own  part of the job and
then starts the ->unlink of the superclass.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unlink(Canvas) :->
	(   get(Canvas, attribute_editor, Editor),
	    Editor \== @nil
	->  send(Editor, quit)
	;   true
	),
	get(Canvas, undo_buffer, UB),
	free(UB),
	send(Canvas, send_super, unlink).


		/********************************
		*         MODIFICATIONS		*
		********************************/

:- pce_group(modified).

modified(C) :->
	send(C, slot, modified, @on).

open_undo_group(C) :->
	"Open a new undo group"::
	send(C?undo_buffer, open_undo_group).

close_undo_group(C) :->
	"Close the undo group"::
	send(C?undo_buffer, close_undo_group).

undo_action(C, Action:code) :->
	"Record an action for undo"::
	send(C?undo_buffer, undo_action, Action).

clear_undo_group(C) :->
	"Empty the current undo-group"::
	send(C?undo_buffer, clear_group).

simple_undo(C) :->
	"Just undo the last action"::
	get(C, undo_buffer, UB),
	send(UB, start_undo),
	send(UB, undo),
	send(UB, end_undo).

undo(C) :->
	"Start undo dialog"::
	send(C?undo_buffer, open, C?frame).

reset(C) :->
	"Trap abort"::
	send(C, send_super, reset),
	send(C?undo_buffer, reset).

		/********************************
		*           SELECTION		*
		********************************/
:- pce_group(selection).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Managing the  selection.    This is  no  different  than for  standard
picture, except that we  have to update the  attribute-editor if it is
attached.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

selection(C, Sel:'graphical|chain*') :->
	"Set the selection shape or chain"::
	send(C, send_super, selection, Sel),
	send(C, keyboard_focus, @nil),
	send(C, update_attribute_editor).


toggle_select(C, Shape:graphical) :->
	"(Un)select a shape"::
	send(Shape, toggle_selected),
	send(C, update_attribute_editor).
	
select_all(C) :->
	"Select all displayed objects"::
	send(C, selection, C?graphicals).


		/********************************
		*            IMPORTS		*
		********************************/
:- pce_group(imports).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Import a  named X11 image (bitmap) file  into the  drawing.  This code
implements a simple   modal dialog  window that prompts  for an image.
The  `text_item  ->type'  attribute  describes the  (PCE) type  of the
object requested.  After the user has entered a  name  and type return
or pressed the  `ok' button, PCE  will try to  convert the typed value
into an  PCE image  object.  See `image  <-convert' for the conversion
rules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

import_image(C) :->
	"Import an image at location (0,0)"::
	new(D, dialog('Import Image')),
	send(D, append, new(TI, text_item(image, ''))),
	send(TI, type, image),
	send(D, append, button(ok, message(D, return, TI?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm_centered, Image),
	send(D, destroy),
	Image \== @nil,
	send(C, display, draw_bitmap(Image)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Import another PCE frame as a bitmap.  The user  may select a frame to
be imported by clicking on it.  There  are two ways to implement this.
The first is to grab the mouse-focus,  wait for a left-click  and then
locate the  frame on  which the  user  clicked.  The second is  to use
PCE's `inspect-handlers'.   If an event happens that  satisfies one of
the `display <-inspect_handlers', PCE  will  locate  the graphical  on
which    the  event  occurred   and    execute  the  message  of   the
inspect-handler with @arg1  bound to the  graphical on which the event
occurred.  This mechanism is  exploited by the `Inspector' and `Visual
Hierarchy' tools of the manual.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

import_frame(C) :->
	"Import image of a frame"::
	get(C, display, Display),
	new(D, dialog('Import Frame')),
	send(D, append,
	     label(prompt, 'Please left-click inside PCE frame to import')),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(Display, inspect_handler, 
	     new(G, handler(ms_left_up, message(D, return, @arg1?frame)))),
	get(D, confirm, Frame),
	send(Display?inspect_handlers, delete, G),
	send(D, destroy),
	Frame \== @nil,
 	object(Frame),			% not yet deleted?
	send(C, display, draw_bitmap(Frame?image)).


		/********************************
		*             EDIT		*
		********************************/
:- pce_group(edit).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Selection-edit operations.  Most of them are rather trivial.  Note the
use of `Chain ->for_all'  to perform operations on  all members of the
selection.   This  method  is  a lot    faster then transferring   the
selection to a Prolog list and than operating on it:

	get(Canvas, selection, Selection),
	chain_list(Selection, List),
	forall(member(Gr, List), send(Gr, free)).

The `Chain ->for_all' operation first makes an array of objects in the
chain, than invokes  the message consequtively on  each  member of the
list.   Before  sending the message,  it  validates  the object  still
exists.  This makes the  method  safe for cases  were  destroying  one
object  destroyes  related objects  that  may be  in   the chain  too.
Connections  are  an example: destroying a graphical  destroys all its
connections and therefore leaves `dangling' references.

One could   generalise from the code   below   by introducing a method
->for_selection: message, but the advantages are very small.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

edit(Canvas, Msg, Grs:'[graphical|chain]') :->
	"Perform operation on graphicals or selection"::
	default(Grs, Canvas?selection, G0),
	(   send(G0, instance_of, chain)
	->  send(G0, for_all, Msg)
	;   send(Msg, forward, G0)
	),
	send(Canvas, modified).


expose_selection(Canvas, Grs:'[graphical|chain]') :->
	"Expose selected graphicals"::
	send(Canvas, edit, message(@arg1, restack, 1), Grs).


hide_selection(Canvas, Grs:'[graphical|chain]') :->
	"Hide selected graphicals"::
	send(Canvas, edit, message(@arg1, restack, -1), Grs).


cut_selection(Canvas, Grs:[graphical|chain]) :->
	"Erase all members of the selection"::
	send(Canvas, copy_selection, Grs),
	send(Canvas, edit, message(@arg1, cut), Grs).

:- pce_global(@draw_clipboard, new(device)).

copy_selection(Canvas, Grs:[graphical|chain]) :->
	"Copy all members of the selection to @draw_clipboard"::
	default(Grs, Canvas?selection, ToCopy),
	get(ToCopy, clone, Copy),
	send(@draw_clipboard, clear),
	send(Copy, for_all,
	     message(@draw_clipboard, display, @arg1)),
	clean_clipboard(@draw_clipboard),
	send(@draw_clipboard, reference).


clean_clipboard(Device) :-
	new(Done, hash_table),
	send(Device?graphicals, for_some,
	     message(@prolog, clean_clipboard_connections,
		     @arg1, Device, Done)),
	send(Done, done).


clean_clipboard_connections(Gr, _CB, Done) :-
	get(Done, member, Gr, @on), !.
clean_clipboard_connections(Gr, CB, _) :-
	\+ get(Gr, is_displayed, CB, @on), !,
	send(Gr, destroy).
clean_clipboard_connections(Gr, CB, Done) :-
	send(Done, append, Gr, @on),
	get(Gr, connections, AllConnections),
	send(AllConnections, for_all,
	     message(@prolog, clean_clipboard_connections,
		     ?(@arg1, opposite, Gr), CB, Done)).
	

paste(Canvas, At:[point]) :->
	"Paste @draw_clipboard"::
	(   object(@draw_clipboard)
	->  (   At == @default
	    ->  (   get(@event, window, Canvas)
		->  get(@event, position, Canvas, Pos)
		;   Pos = point(0,0)
		)
	    ;   Pos = At
	    ),
	    get(@draw_clipboard?graphicals, clone, Clone),
	    send(Canvas, open_undo_group),
	    send(Clone, for_all,
		 and(message(@arg1, relative_move, Pos),
		     message(Canvas, display, @arg1),
		     message(Canvas, undo_action,
			     create(message, @arg1, cut)))),
	    send(Canvas, close_undo_group),
	    send(Canvas, selection, Clone),
	    send(Clone, done),
	    send(Canvas, modified)
	;   send(Canvas, report, warning, 'Draw Clipboard is empty')
	).

		 /*******************************
		 *	 WINDOWS CLIPBOARD	*
		 *******************************/

map_format(aldus, wmf) :- !.
map_format(Fmt, Fmt).

export_win_metafile(Canvas, What:[{selection,drawing}], Format:[{wmf,emf}]) :->
	"Export to the Windows clipboard"::
	send(Canvas, keyboard_focus, @nil),
	default(What, selection, TheWhat),
	(   Format == @default
	->  get_config(draw_config:file/meta_file_format, TheFormat0),
	    map_format(TheFormat0, TheFormat)
	;   TheFormat = Format
	),
	get(Canvas, selection, OldSelection),
	send(Canvas, selection, @nil),
	(   TheWhat == selection
	->  Graphs = OldSelection
	;   get(Canvas, graphicals, Graphs)
	),
	new(MF, win_metafile),
	send(MF, draw_in, Graphs),
	send(@display, selection_owner, MF,
	     primary,			% which
	     @receiver,			% fetch object
	     message(@receiver, free),	% loose selection
	     TheFormat),
	send(Canvas, selection, OldSelection),
	send(Canvas, report, status, 'Put %s on clipboard', TheWhat).

import_win_metafile(Canvas) :->
	"Get selection as picture and import it"::
	(   get(Canvas?display, selection,
		primary, win_metafile, win_metafile, MF)
	->  new(DMF, draw_metafile),
	    send(DMF, copy, MF),
	    send(Canvas, display, DMF),
	    free(MF),
	    send(Canvas, report, status, 'Imported metafile')
	;   send(Canvas, report, warning, 'Could not get clipboard data')
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method below  duplicates the selection  and displays the duplicate
at  an   optionally specified  offset.   There  are  various difficult
operations in  this  predicate.  The  `if-then-else'  illustrates  how
default arguments are handled inside a method.

Next, the selection, which is  a chain  with  the selected shapes,  is
cloned.  `Object <-clone'  creates  a  recursive clone. Note that  the
selection as  a   whole is cloned   rather  than each member    of  it
seperately.   This guarantees  proper kloning of relations  inside the
selection (such as connections).

NOTE:	Connections to objects outside the selection are not handled
	properly.  Kloning objects has to be based on semantics
	attached to slot-relations rather than classes.

Finally  ->done is sent  to  the clone  of  the selection chain.  This
indicates PCE that Prolog is no  longer interrested  in the object and
that, if there  are  no references to  it, it may  be removed.   Using
`Object ->done' is generally advocated over using  ->free after Prolog
has finished   with the  result  of a   get operation.   Consider  the
following cases:

	get(Graphical, position, Pos),
	...
	send(Pos, free).

and

	get(Graphical, area, Area),
	...
	send(Area, free).

In the first example, `Pos' is a point created by  the method, but not
referred to by  the `Graphical'.    Using ->free  is  correct.  In the
second case however the method <-area returns  the <-area attribute of
`Graphical' and destroying this would make `Graphical' an inconsistent
object.  Using ->done, the point will be removed in the first example,
but the area will remain in the second.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

duplicate_selection(Canvas, Offset:[point]) :->
	"Duplicate the selection"::
	default(Offset, point(10, 10), Off),
	get(Canvas?selection, clone, Duplicate),
	send(Canvas, open_undo_group),
	send(Duplicate, for_all,
	     and(message(Canvas, display, @arg1),
		 message(@arg1, relative_move, Off))),
	clean_duplicates(Duplicate),
	send(Canvas, selection, Duplicate),
	send(Canvas, clear_undo_group),
	send(Duplicate, for_all,
	     message(Canvas, undo_action,
		     create(message, @arg1, cut))),
	send(Canvas, close_undo_group),
	send(Duplicate, done),
	send(Canvas, modified).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method `object <-clone' makes a recursive  copy of an  object.  If
an  object with connection is  cloned the connections   as well as the
`other  side' of  the  connections will  be  cloned   as  well.   This
predicate removes all  graphical   objects that  are related  to   the
duplicated object but not displayed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

clean_duplicates(Chain) :-
	new(Done, hash_table),
	send(Chain, for_some,
	     message(@prolog, clean_duplicate_connections, @arg1, Done)),
	send(Done, free).

clean_duplicate_connections(Gr, Done) :-
	get(Done, member, Gr, @on), !.
clean_duplicate_connections(Gr, _) :-
	\+ get(Gr, window, _), !,
	send(Gr, destroy).
clean_duplicate_connections(Gr, Done) :-
	send(Done, append, Gr, @on),
	get(Gr, connections, AllConnections),
	send(AllConnections, for_all,
	     message(@prolog, clean_duplicate_connections,
		     ?(@arg1, opposite, Gr), Done)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Start the attribute editor on the current selection.  The  first time,
we need to create the editor.   If the user hits  `quit' on the button
of  the editor, the editor is  just  removed  from   the display using
`Frame ->show: @off' and this function can make it visible again using
->show: @on.  This approach has several advantages.  First  of all, it
is a lot faster and second, the attribute editor will be  at  the same
location on the display as were the user left it last time.

See also ->unlink in this class and 'draw_attribute_editor ->quit'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

edit_selection(Canvas) :->
	"Start attribute editor on selection"::
	get(Canvas, attribute_editor, Editor),
	(    Editor == @nil    
	->   send(Canvas, slot, attribute_editor,
		  new(A, draw_attribute_editor(Canvas))),
	     send(A, open)
	;    A = Editor,
	     send(A, show, @on),
	     send(A, expose)
	),
	send(A, client, Canvas?selection).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update  the setting   of   the attribute editor   because  either  the
selection has  changed, or the attributes of  members of the selection
has changed.

NOTE:	The move and resize gestures should invoke this behaviour too.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

update_attribute_editor(Canvas) :->
	"Update values in attribute editor"::
	get(Canvas, attribute_editor, Editor),
	(   Editor \== @nil
	->  send(Editor, client, Canvas?selection)
	;   true
	).


clear(Canvas, Confirm:[bool]) :->
	"Clear and reset <->file attribute"::
	(   Confirm == @on,
	    \+ send(Canvas?graphicals, empty)
	->  send(@display, confirm, 'Clear drawing?')
	;   true
	),
	send(Canvas, send_super, clear),
	send(Canvas, file, @nil),
	send(Canvas, slot, modified, @off),
	send(Canvas?undo_buffer, clear),
	send(Canvas, update_attribute_editor).
	

		/********************************
		*           ALIGNMENT		*
		********************************/
:- pce_group(alignment).

align_with_selection(Canvas, Gr:graphical) :->
	"Align graphical (with selection)"::
	(   get(Canvas, selection, G0),
	    send(G0, delete_all, Gr),
	    \+ send(G0, empty)
	->  true
	;   get(Canvas?graphicals, copy, G0),
	    send(G0, delete_all, Gr)
	),
	get(G0, find_all, not(message(@arg1, instance_of, line)), G1),
	chain_list(G1, L1),
	align_graphical(Gr, L1).


align_selection(Canvas) :->
	"Align all elements of the selection"::
	send(Canvas, edit, message(Canvas, align_graphical, @arg1)).


align_graphical(Canvas, Gr:graphical) :->
	"Align a single graphical"::
	get(Canvas?graphicals, find_all,
	    and(not(message(@arg1, instance_of, line)),
		not(?(@arg1, hypered, supports))),
	    G0),
	send(G0, delete_all, Gr),
	chain_list(G0, L0),
	auto_adjust(resize, Gr, L0),
	align_graphical(Gr, L0).
	

auto_align(Canvas, Gr:graphical, How:{create,resize,move}) :->
	"Align graphical if auto_align_mode is @on"::
	(   get(Canvas, auto_align_mode, @on)
	->  ignore(auto_align(Canvas, Gr, How))
	;   true
	).


auto_align(Canvas, Gr, How) :-
	get(Canvas?graphicals, find_all,
	    and(not(message(@arg1, instance_of, line)),
		not(?(@arg1, hypered, supports))),
	    G0),
	send(G0, delete_all, Gr),
	chain_list(G0, L0),
	auto_adjust(How, Gr, L0),
	align_graphical(Gr, L0).


auto_adjust(How, Gr, L0) :-
	(How == create ; How == resize),
	\+ send(Gr, instance_of, text),
	adjust_graphical(Gr, L0), !.
auto_adjust(_, _, _).
	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Distribute spaces between graphicals evenly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

distribute_selection(Canvas) :->
	"Distribute selected objects"::
	get(Canvas, selection, Selection),
	send(Selection, for_all,
	     if(or(message(@arg1, instance_of, connection),
		   ?(@arg1, hypered, supports)),
		message(Selection, delete, @arg1))),
	get(Selection, size, Size),
	(   Size < 3
	->  send(Canvas, report, warning, 'Need at least 3 selected objects')
	;   side_selector(Dir, Top, Bottom, Height),
	    sequence_in_dir(Selection, Dir)
	->  get(Selection?head, Top, Y0),
	    get(Selection?tail, Bottom, Y1),
	    H is Y1 - Y0,
	    new(HTotal, number(0)),
	    send(Selection, for_all, message(HTotal, plus, @arg1?Height)),
	    get(HTotal, value, HT),
	    Sep is (H - HT)/(Size-1),
	    chain_list(Selection, List),
	    send(Canvas, open_undo_group),
	    distribute(List, Y0, Dir, 0, Sep),
	    send(Canvas, close_undo_group),
	    send(Canvas, report, status, 'Distributed in %s-direction', Dir)
	;   send(Canvas, report, warning, 'Cannot determine direction')
	).

sequence_in_dir(Chain, Dir) :-
	side_selector(Dir, Sel, _, _),
	send(Chain, sort, ?(@arg1?Sel, compare, @arg2?Sel)),
	chain_list(Chain, List),
	sequence(List, Dir).

side_selector(y, top_side, bottom_side, height).
side_selector(x, left_side, right_side, width).


sequence([_], _) :- !.
sequence([H1,H2|T], y) :- !,
	get(H1, bottom_side, Bottom),
	get(H2, top_side, Top),
	Top >= Bottom,
	sequence([H2|T], y).
sequence([H1,H2|T], x) :-
	get(H1, right_side, R),
	get(H2, left_side, L),
	L >= R,
	sequence([H2|T], x).

distribute([], _, _, _, _).
distribute([H|T], V0, Sel, N, Sep) :-
	send(H, Sel, V0+N*Sep),
	(   Sel == y
	->  get(H?area, height, Me)
	;   get(H?area, width, Me)
	),
	V1 is V0+Me,
	NN is N + 1,
	distribute(T, V1, Sel, NN, Sep).
	

	      /********************************
	      *           LOAD/SAVE	      *
	      ********************************/
:- pce_group(file).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Saving and loading is  currently performed  by saving the  PCE objects
using  PCE's binary  saving   algorithm.  This approach   has  several
advantages and disadvantages.  The advantages:

	* Using `Object ->save_in_file', applications whose database
	  consists of a collection of PCE objects can easily save
	  their data.
	* The PCE built-in loading and saving is fast.

The disadvantages

	* Binary format.  Currently no provisions for byte-order
	  differences.
	* It is difficult to control what exactly will be stored on
	  file.  See also the discussion on kloning with ->duplicate.
	* Significant changes to the representation of PCE-classes
	  make reloading impossible.  This is notably a problem for
	  loading and storing graphical information.

An alternative is to write  an  application-specific save/load that is
more robust against changes  to PCE, but may be  slow.   This kind  of
saved version can be used to convert to later versions of PCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->save_as requests a filename and  then invokes `Object ->save'.   The
filename is requested   via  @finder, an instance of  the user-defined
class   `finder', defined   in  the  PCE library file  `find_file.pl'.
Linking the library is declared in the file draw.pl.

This   addresses the  general case  of  asking  for information  using
dialog-boxes.  In  earlier PCE applications it  was normal to  build a
dialog-box, display it, read the information and destroy it again.

For the file-finder,  the  reusable   object @finder is created  using
pce_global/2 construct.   Once  created,  @finder   is mapped   on and
removed-from   the display    using `Frame ->show:   @on/@off'.   This
approach is fast and allow us to remember status (such as the selected
directory) from the last time the finder was used.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

save_as(Canvas) :->
	"Save in user-specified file"::
	get(@finder, file, @off, '.pd', FileName),
	new(File, file(FileName)),
	send(Canvas, save, File),
	get(File, absolute_path, Path),
	register_recent_file(Path).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actual saving to file.  The  toplevel-object  saved  is a sheet.  This
way we can easily add new  attributes without affecting compatibility.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

save(Canvas, File:[file]) :->
	"Save canvas in named file"::
	(   File == @default
	->  (   get(Canvas, file, SaveFile),
	        SaveFile \== @nil
	    ->	true
	    ;	get(@finder, file, @off, '.pd', SaveFileName),
		send(Canvas, file, new(SaveFile, file(SaveFileName)))
	    )
	;   send(Canvas, file, File),
	    SaveFile = File
	),
	send(SaveFile, backup),
	send(Canvas, keyboard_focus, @nil),
	get(Canvas?frame, version, Version),
	new(Sheet, sheet(attribute(graphicals, Canvas?graphicals),
			 attribute(version, Version))),
	(   get_config(draw_config:file/save_prototypes, @on)
	->  get(Canvas?frame?menu, proto_sheet, ProtoTypes),
	    send(Sheet, value, prototypes, ProtoTypes)
	;   true
	),
	send(Sheet, save_in_file, SaveFile),
	send(Canvas, slot, modified, @off),
	send(Sheet, free),
	new(Which, string('pd')),
	(   get_config(draw_config:file/save_postscript_on_save, @on)
	->  send(Canvas, postscript),
	    send(Which, append, '+ps')
	;   true
	),
	(   get_config(draw_config:file/save_metafile_on_save, @on)
	->  send(Canvas, save_default_windows_metafile),
	    send(Which, append, '+mf')
	;   true
	),
	send(Canvas, report, status, 'Saved (%s) %s',
	     Which, SaveFile?base_name).

save_if_modified(Canvas, AllowQuit:[bool]) :->
	"Save if the drawing is modified"::
	(   get(Canvas, modified, @on)
	->  get(Canvas, frame, Draw),
	    new(D, dialog),
	    send(D, transient_for, Draw),
	    send(D, append, label(message, 'Drawing has changed')),
	    (	AllowQuit == @on
	    ->  send(D, append, button('Save & Quit',
				       message(D, return, save_and_quit))),
		send(D, append, button(quit,
				       message(D, return, quit)))
	    ;	send(D, append, button(save,
				       message(D, return, save)))
	    ),
	    send(D, append, button(cancel,
				   message(D, return, cancel))),
	    get(D, confirm_centered, Rval),
	    send(D, destroy),
	    (   Rval == save_and_quit
	    ->  send(Canvas, save),
	        send(Draw, destroy)
	    ;   Rval == quit
	    ->  send(Draw, destroy)
	    ;	Rval == save
	    ->	send(Canvas, save)
	    )
	;   true
	).


load_from(Canvas) :->
	"Load from user-specified file"::
	get(@finder, file, @on, '.pd', File),
	send(Canvas, load, File, @on).


import(Canvas) :->
	"Add contents of user-requested file"::
	get(@finder, file, @on, '.pd', File),
	send(Canvas, load, File, @off).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load specified file  and set the  file  attribute.  The PCE object  is
loaded from the file using the `File <-object: method.

NOTE:	Currently PCE provides no way for the programmer to specify
	what should happen on file errors.  This will be fixed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

load(Canvas, File:file, Clear:[bool]) :->
	"Load from named file and [clear]"::
	(    Clear == @on,
	     \+ send(Canvas?graphicals, empty)
	->   send(Canvas, save_if_modified),
	     send(Canvas, clear)
	;    true
	),
	get(File, object, Sheet),
	get(Sheet, graphicals, Grs),
	send(Grs, for_all,
	     and(message(Canvas, display, @arg1),
		 message(@arg1, selected, @off))),
	send(Canvas, normalise, Grs),
	(   Clear == @on
	->  send(Canvas, file, File)
	;   true
	),
	(   get(Sheet, value, version, SaveVersion)
	->  true
	;   SaveVersion = @nil
	),
	(   Clear == @on,
	    get(Sheet, value, prototypes, ProtoTypes)
	->  send(Canvas?frame?menu, proto_sheet, ProtoTypes, @on)
	;   true
	),
	send(Canvas, convert_old_drawing, SaveVersion),
	send(Canvas, slot, modified, @off),
	send(Canvas, report, status, 'Loaded %s', File?base_name),
	get(File, absolute_path, Path),
	register_recent_file(Path),
	send(Sheet, done).

%	register_recent_file(+Path)
%
%	Register a file with the recent file list.  Allows the list to
%	grow up to 20.

register_recent_file(Path) :-
	Key = draw_config:history/recent_files,
	(   get_config(Key, Set0)
	;   Set0 = []
	),
	delete(Set0, Path, Set1),
	first_n(10, [Path|Set1], Set),
	set_config(Key, Set).

first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	NN is N - 1,
	first_n(NN, T0, T).

file(Canvas, File:file*) :->
	"Set file attribute"::
	send(Canvas, slot, file, File),
        get(Canvas, frame, Draw),
	send(Draw, update_label).


		 /*******************************
		 *	       VERSION		*
		 *******************************/

convert_old_drawing(Canvas, SaveVersion:real*) :->
	(   convert_old_drawing(SaveVersion, Canvas),
	    fail
	;   true
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Before versions were attached to saved  figures, class draw_compound was
a subclass of class device. Now, it is   a subclass of class figure. The
code below turns the draw_compounds into valid figures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

convert_old_drawing(@nil, Canvas) :-
	get(Canvas, find, @default,
	    if(message(@arg1, instance_of, draw_compound),
	       and(if(@arg1?radius == @nil,
		      and(message(@arg1, slot, status, all_active),
			  message(@arg1, slot, radius, 0),
			  message(@arg1, slot, border, 0),
			  message(@arg1, slot, pen,    0))),
		   new(or)),
	       new(or)),
	    _).
convert_old_drawing(_, Canvas) :-
	object(@win_ansi_var),
	get(Canvas, find, @default,
	    and(if(and(message(@arg1, instance_of, text),
		       @arg1?font == @win_ansi_var),
		   message(@arg1, font, normal)),
		new(or)),
	    _).


		/********************************
		*           POSTSCRIPT		*
		********************************/
:- pce_group(postscript).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a PostScript description of the contents of the picture.

NOTE:	This should ask for options such as landscape and scaling
	factor, which can be applied to the Graphical <-postscript
	method.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

postscript(Canvas) :->
	"Write PostScript to default file"::
	get(Canvas, default_file, eps, File),
	send(Canvas, generate_postscript, File).


postscript_as(Canvas) :->
	"Write PostScript to file"::
	get(Canvas, default_file, eps, DefFile),
	get_config(draw_config:file/postscript_file_extension, Ext),
	get(@finder, file, @off, Ext, @default, DefFile, FileName),
	send(Canvas, generate_postscript, FileName).


generate_postscript(Canvas, PsFile:file) :->
	"Write PostScript to named file"::
	send(PsFile, open, write),
	send(PsFile, append, Canvas?postscript),
	send(PsFile, append, string('showpage\n')),
	send(PsFile, close),
	send(Canvas, report, status,
	     'Written PostScript to `%s''', PsFile?base_name).


default_file(Canvas, Ext:[name], DefName:name) :<-
	"Default name for PostScript file"::
	(   get(Canvas, file, File),
	    File \== @nil
	->  get(File, name, Name),
	    file_name_extension(Base, pd, Name)
	;   Base = scratch
	),
	(   Ext == @default
	->  DefName = Base
	;   file_name_extension(Base, Ext, DefName)
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows MetaFile generation
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

save_default_windows_metafile(Canvas) :->
	"Save default format to default file"::
	get_config(draw_config:file/meta_file_format, Fmt),
 	wmf_extension(Fmt, Ext),
	get(Canvas, default_file, Ext, DefFile),
	send(Canvas, generate_metafile, DefFile, Fmt).

wmf_extension(aldus, wmf) :- !.
wmf_extension(Fmt, Fmt).

windows_metafile(Canvas, File:[file], Format:[{emf,wmf,aldus}]) :->
	"Write Windows Metafile"::
	(   Format == @default
	->  get_config(draw_config:file/meta_file_format, Fmt)
	;   Fmt = Format
	),
	wmf_extension(Fmt, Ext),
	(   File == @default
	->  get(Canvas, default_file, Ext, DefFile),
	    get(@finder, file, @off, Ext, @default, DefFile, TheFile)
	;   TheFile = File
	),
	send(Canvas, generate_metafile, TheFile, Fmt).
	

generate_metafile(Canvas, File:file, Format:{emf,wmf,aldus}) :->
	"Write document to file as meta-file"::
	new(MF, win_metafile),
	send(MF, draw_in, Canvas?graphicals),
	send(MF, save, File, Format),
	send(Canvas, report, status,
	     'Saved as "%s" file to %s', Format, File?absolute_path).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Print  the image to the default  printer.  Also this  method should be
extended by requesting additional parameters from the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print(Canvas) :->
	"Send to default printer"::
	print_canvas(Canvas).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are two routes to print.  On   MS-Windows  printing is achieved by
drawing on a GDI representing a printer, after which the Windows printer
driver creates printer-codes and sends them to the printer. The standard
Windows print dialog is shown by   win_printer->setup. Next we need some
calculation effort to place our diagram reasonably on the page.

In the Unix world, things go different. In general you make a PostScript
file and hand this  to  the   print-spooler,  which  will  translate the
device-independant PostScript to whatever the printer needs.

XPCE doesn't (yet)  try  to  hide   the  difference  between  these  two
approaches.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_canvas(Canvas) :-
	get(@pce, convert, win_printer, class, _), !,
	get(Canvas, default_file, Job),
	new(Prt, win_printer(Job)),
	send(Prt, setup, Canvas),
	send(Prt, open),
	get(Canvas, bounding_box, area(X, Y, W, H)),
	get(@display, dots_per_inch, size(DX, DY)),
	InchW is W/DX,
	InchH is H/DY,

	get(Prt, size, size(PW0, PH0)),
	get(Prt, dots_per_inch, size(RX, RY)),
	MarX is RX,			% default 1 inch margins
	MarY is RY,
	PrInchW is (PW0-MarX*2)/RX,
	PrInchH is (PH0-MarY*2)/RY,

	send(Prt, map_mode, isotropic),
	(   InchW < PrInchW,
	    InchH < PrInchH		% it fits on the page
	->  OX is MarX + ((PrInchW-InchW)/2)*RX,
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(OX, MarY, RX, RY))
	;   Aspect is min(PrInchW/InchW, PrInchH/InchH),
	    ARX is integer(Aspect*RX),
	    ARY is integer(Aspect*RY),
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(MarX, MarY, ARX, ARY))
	),
	send(Prt, draw_in, Canvas?graphicals),
	send(Prt, close),
	free(Prt).
print_canvas(Canvas) :-
	get(Canvas, default_printer, Printer),
	get(Canvas, frame, Draw),
	default_printer(Draw, Printer),
	new(PsFile, file),
	send(PsFile, open, write),
	send(PsFile, append, Canvas?postscript),
	send(PsFile, append, 'showpage\n'),
	send(PsFile, close),
	get(PsFile, absolute_path, File),
	get_config(draw_config:print/print_command, CmdTempl),
	print_cmd(CmdTempl, Printer, File, Cmd),
	pce_shell_command('/bin/sh'('-c', Cmd)),
	send(PsFile, remove),
	send(PsFile, done),
	send(Canvas, report, status, 'Sent to printer `%s''', Printer).
	

default_printer(Canvas, Printer:name) :<-
	"Get name of the printer"::
	get(Canvas, frame, Draw),
	default_printer(Draw, DefPrinter),
	new(D, dialog('PceDraw: printer?')),
	send(D, append, new(P, text_item(printer, DefPrinter))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, append, button(ok, message(D, return, P?selection))),
	send(D, default_button, ok),
	send(D, transient_for, Draw),
	send(D, modal, transient),
	get(D, confirm_centered, Canvas?frame?area?center, Answer),
	send(D, destroy),
	Answer \== @nil,
	Printer = Answer.

default_printer(_, Printer) :-
	get_config(draw_config:print/printer, Printer0),
	Printer0 \== @default, !,
	(   get(Printer0, scan, '$%[a-zA-Z0-9_]', vector(VarName)),
	    get(@pce, environment_variable, VarName, Printer)
	->  true
	;   Printer = Printer0
	).
default_printer(_, Printer) :-
	get(@pce, environment_variable, 'PRINTER', Printer), !.
default_printer(_, postscript).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print_cmd(+Template, +Printer, +File,  -Command)   determines  the shell
command to execute in order to get `File' printed on `Printer' using the
given template. The substitutions are handled by a regex object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_cmd(Template, Printer, File, Cmd) :-
	new(S, string('%s', Template)),
	substitute(S, '%p', Printer),
	substitute(S, '%f', File),
	get(S, value, Cmd),
	free(S).

substitute(S, F, T) :-
	new(R, regex(F)),
	send(R, for_all, S,
	     message(@arg1, replace, @arg2, T)),
	free(R).


		/********************************
		*             MODES		*
		********************************/
:- pce_group(mode).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Switch the mode of the editor.  The mode determines which gestures are
active  (see `gesture.pl') and  therefore what  happens on some event.
For each mode, a cursor is defined to indicate the mode to the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

mode(Canvas, Mode:name, Cursor:cursor*) :->
	"Set the mode of the canvas"::
	send(Canvas, cursor, Cursor),
	send(Canvas, slot, mode, Mode),
	send(Canvas, keyboard_focus, @nil),
	send(Canvas, selection, @nil).

:- pce_end_class.
