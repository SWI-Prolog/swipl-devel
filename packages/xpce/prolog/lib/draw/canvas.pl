/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(draw_canvas, []).

:- use_module(library(pce)).
:- use_module(align).
:- require([ chain_list/2
	   , concat/3
	   , concat_atom/2
	   , ignore/1
	   , shell/1
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

:- pce_begin_class(draw_canvas, picture).

resource(size, size, 'size(500,500)',	'Default size of drawing area').

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
	send(Canvas, slot, modified, @off),
	send(Canvas, auto_align_mode, @off),
	send(Canvas, mode, select, @nil),
	send(Canvas, recogniser, @draw_canvas_recogniser).

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


:- pce_global(@draw_canvas_recogniser,
      new(handler_group(@draw_create_resize_gesture,
			@draw_create_line_gesture,
			@draw_create_path_gesture,
			@draw_create_text_recogniser,
			@draw_create_proto_recogniser,
			@draw_warp_select_gesture,
			popup_gesture(@draw_canvas_popup)))).


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
	send(Canvas, send_super, unlink).


		/********************************
		*         MODIFICATIONS		*
		********************************/

modified(C) :->
	send(C, slot, modified, @on).


		/********************************
		*           SELECTION		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Managing the  selection.    This is  no  different  than for  standard
picture, except that we  have to update the  attribute-editor if it is
attached.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

selection(C, Sel:'graphical|chain*') :->
	"Set the selection shape or chain"::
	send(C, send_super, selection, Sel),
	send(C, update_attribute_editor).


toggle_select(C, Shape:graphical) :->
	"(Un)select a shape"::
	send(Shape, toggle_selected),
	send(C, update_attribute_editor).
	


		/********************************
		*            IMPORTS		*
		********************************/

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
	send(C, display, draw_bitmap(Frame?image)).


		/********************************
		*             EDIT		*
		********************************/

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
	send(Canvas, edit, message(@arg1, expose), Grs).


hide_selection(Canvas, Grs:'[graphical|chain]') :->
	"Hide selected graphicals"::
	send(Canvas, edit, message(@arg1, hide), Grs).


cut_selection(Canvas, Grs:[graphical|chain]) :->
	"Erase all members of the selection"::
	send(Canvas, copy_selection, Grs),
	send(Canvas, edit, message(@arg1, free), Grs).

:- pce_global(@draw_clipboard, new(device)).

%))))

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
	send(Done, free).


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
	    send(Clone, for_all,
		 and(message(@arg1, relative_move, Pos),
		     message(Canvas, display, @arg1))),
	    send(Canvas, selection, Clone),
	    send(Clone, done),
	    send(Canvas, modified)
	;   send(Canvas, report, warning, 'Draw Clipboard is empty')
	).


		/********************************
		*           ALIGNMENT		*
		********************************/

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
	    not(message(@arg1, instance_of, line)), G0),
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
	    not(message(@arg1, instance_of, line)), G0),
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
	send(Duplicate, for_all,
	     and(message(Canvas, display, @arg1),
		 message(@arg1, relative_move, Off))),
	clean_duplicates(Duplicate),
	send(Canvas, selection, Duplicate),
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
	send(Canvas, update_attribute_editor).
	
	      /********************************
	      *           LOAD/SAVE	      *
	      ********************************/

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
	get(@finder, file, @off, '.pd', File),
	send(Canvas, save, File).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actual saving to file.  The  toplevel-object  saved  is a sheet.  This
way we can easily add new  attributes without affecting compatibility.
Future versions will probably also save the  name of the file on which
the   prototypes  were stored, so  we   can   reload the corresponding
prototypes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

save(Canvas, File:[file]) :->
	"Save canvas in named file"::
	(   File == @default
	->  get(Canvas, file, SaveFile),
	    (   SaveFile == @nil
	    ->  send(@display, inform, 'No current file'),
	        fail
	    ;   true
	    )
	;   send(Canvas, file, File),
	    SaveFile = File
	),
	send(SaveFile, backup),
	new(Sheet, sheet(attribute(graphicals, Canvas?graphicals))),
	send(Sheet, save_in_file, SaveFile),
	send(Canvas, slot, modified, @off),
	send(Canvas?frame, feedback,
	     string('Saved %s', SaveFile?base_name)),
	send(Sheet, free).


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
	(    Clear == @on
	->   send(Canvas, clear, @on)
	;    true
	),
	get(File, object, Sheet),
	send(Sheet?graphicals, for_all,
	     block(message(Canvas, display, @arg1),
		   message(@arg1, selected, @off))),
	send(Canvas, file, File),
	send(Canvas?frame, feedback, string('Loaded %s', File?base_name)),
	send(Sheet, done).


file(Canvas, File:file*) :->
	"Set file attribute"::
	send(Canvas, slot, file, File),
	(   File \== @nil
	->  send(Canvas?frame, label,
		 string('PceDraw: %s', File?name),
		 string('PceDraw: %s', File?base_name))
	;   send(Canvas?frame, label, 'PceDraw')
	).


		/********************************
		*           POSTSCRIPT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a PostScript description of the contents of the picture.

NOTE:	This should ask for options such as landscape and scaling
	factor, which can be applied to the Graphical <-postscript
	method.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

postscript(Canvas) :->
	"Write PostScript to default file"::
	get(Canvas, default_psfile, File),
	send(Canvas, generate_postscript, File).


postscript_as(Canvas) :->
	"Write PostScript to file"::
	get(Canvas, default_psfile, DefFile),
	get(@finder, file, @off, '.ps', @default, DefFile, FileName),
	send(Canvas, generate_postscript, FileName).


generate_postscript(Canvas, PsFile:file) :->
	"Write PostScript to named file"::
	send(PsFile, open, write),
	send(PsFile, append, Canvas?postscript),
	send(PsFile, close),
	send(Canvas?frame, feedback,
	     string('Written PostScript to `%s''', PsFile?base_name)).


default_psfile(Canvas, DefName) :<-
	"Default name for PostScript file"::
	(   get(Canvas, file, File),
	    File \== @nil,
	    get(File, name, Name),
	    concat(Base, '.pd', Name)
	->  concat(Base, '.ps', DefName)
	;   DefName = 'scratch.ps'
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Print  the image to the default  printer.  Also this  method should be
extended by resquesting additional parameters from the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print(Canvas) :->
	"Send to default printer"::
	default_printer(Printer),
	temp_file(File),
	new(PsFile, file(File)),
	send(PsFile, open, write),
	send(PsFile, append, Canvas?postscript),
	send(PsFile, append, 'showpage\n'),
	send(PsFile, close),
	concat_atom(['lpr -P', Printer, ' ', File], Cmd),
	shell(Cmd),
	send(PsFile, remove),
	send(PsFile, done),
	send(Canvas?frame, feedback,
	     string('Sent to printer `%s''', Printer)).
	
default_printer(Printer) :-
	get(@pce, environment_variable, 'PRINTER', Printer), !.
default_printer(postscript).

temp_file(Name) :-
	get(@pce, pid, Pid),
	concat('/tmp/xpce_', Pid, Name).

		/********************************
		*             MODES		*
		********************************/

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
