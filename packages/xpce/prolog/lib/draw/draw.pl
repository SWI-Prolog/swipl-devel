/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(draw,
	[ draw/0				  % Start drawing tool
	, draw/1				  % Start editing file
	]).


		/********************************
		*      LINKING OTHER FILES	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is the toplevel module  of PceDraw.  It  loads the various
other modules and defines class  `draw', of which  the drawing tool is
an instance.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PCE/Prolog modules that should run on SICStus Prolog must  include the
library pce,    which  defines the  basic  interface  predicates.  The
require/1    directive loads    the    requested predicates  from  the
(PCE-)library.  None of these declarations  are needed  for SWI-Prolog
as SWI-Prolog will  inherit the PCE system  predicates from the module
`user' and load the other predicates using the autoloader.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(pce)).

:- require([ concat/3
	   , send_list/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With this declaration we load the other Prolog modules of PceDraw.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(
	[ gesture				  % Gestures
	, shapes				  % Drawable shapes
	, canvas				  % Drawing plain
	, menu				  	  % Icon Menu
	]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The additional  file declarations  below are not  always needed.   For
this reason  they are  defined  using pce_autoload/2.  This keeps  the
initial  image small, reducing  startup time.  Whenever  an attempt is
made to create an instance or subclass of  a  class that is defined as
an autoload class, PCE  will activate  the `undefined_class' member of
`@pce <-exception_handlers'.  Using the standard interface setup, this
will cause  Prolog to examine the  autoload  declarations and load the
specified file.

The library file find_file.pl defines   class finder, an  instance  of
which can be used to ask the  user for a Unix file.   One instance can
be used for finding   any file that is   needed by PceDraw.  For  this
reason we use the pce_global/2 construct.  Whenever  @finder is passed
via one  of the interface  predicates and @finder does not  exist, the
database of global declarations is searched.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_autoload(draw_attribute_editor, library('draw/attribute')).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

		/********************************
		*           ENTRY POINT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Toplevel goals:

	# draw
	  Create a drawing tool and display it.

	# draw(+File)
	  As draw/0, but immediately loads a file.

One could choose not to define these  predicate  and declare the class
`draw' to be  the toplevel  or  public functionality.    This actually
might be a cleaner solution than the one choosen here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

draw :-
	new(Draw, draw),
	send(Draw, open).

draw(File) :-
	add_extension(File, '.pd', PdFile),
	new(Draw, draw),
	send(Draw, open),
	get(Draw, canvas, Canvas),
	(   send(file(PdFile), exists)
	->  send(Canvas, load, PdFile, @on)
	;   send(Canvas, file, PdFile)
	).


add_extension(Base, Ext, Base) :-
	concat(_, Ext, Base), !.
add_extension(Base, Ext, File) :-
	concat(Base, Ext, File).


		/********************************
		*           CLASS DRAW		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `draw' defines and manages  the entire tool.  Its initialisation
builds  the  entire  tool and the  resulting instance provide means of
communication between the various parts.  The call
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw, frame).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
starts  the definition of  a new class  `draw' that  is a subclass  of
class  frame.  Classes should always  be  a subclass of  some existing
class.  If  there is  no particular PCE  class  to inherit  from, this
should be class `object', the root of the PCE class hierarchy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The term  resource/4 is expanded  by  the PCE/Prolog class  loader.  A
resource provides  access to  the   X-window resource  database.   The
PceDraw user may specify a value in ~/.Xdefaults:

	Pce.Draw.auto_align_mode:	@off
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resource(auto_align_mode,	bool,	'@on',
	 "Automatically align graphicals").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If  the initialisation of an instance  of  this class differs from the
initialisation of its super-class, a method called `->initialise' must
be defined.  It's task  is to initialise the  new instance.  When  PCE
creates an instance (with new/2,   @pce <-instance or  otherwise),  it
allocates memory for  it,  resets all  slots  to @nil  and  calls  the
->initialise method.  The arguments to this method may differ from the
initialisation arguments of the super class.  In this  case, frame has
three (optional) initialisation arguments, while class draw has none.

Somewhere in the initialise method, there should be a call

	send(Self, send_super, initialise, ...)

To invoke the initialisation method of  the superclass.  The arguments
should be  valid arguments  for  the initialisation   method   of this
superclass.  The normal schema is:

	1) Check the arguments and compute defaults from them.
	2) send(Self, send_super, initialise, ...)
	3) Do class specific initialisation.

In  our case,  the various windows  that make up the drawing  tool are
created and attached to the frame.

To avoid a giant clause, a call to the sub-predicate  fill_dialog/1 is
made.  It is a difficult decision whether or not this should have been
realised using   `send(Draw,  fill_dialog,   D)' and  the   subsequent
declaration    of this  method.     In  general,  use  send/[2-12] and
get/[3-13] for communication  between classes, or communication within
a   class if type-checking   or  type-conversion associated  with  PCE
methods is useful.

For  PCE-3  users, note the  use of the term new/2  in the  second and
further  sends to create  the windows  inline   and get the reference.
This approach is preferred over a separate new/2  and ->append.  It is
shorter but -more important- it attaches the canvas immediately to the
frame, making the frame responsible for its destruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(Draw) :->
	send(Draw, send_super, initialise, 'PceDraw'),
	send(Draw, done_message, message(Draw, quit)),
	send(Draw, append, new(Canvas, draw_canvas)),
	send(new(Menu, draw_menu), left, Canvas),
	send(new(D, dialog), above, Menu),
	fill_dialog(D),
	fill_menu(Menu),
	send(Menu, activate_select).


		/********************************
		*     COMMAND AREA (DIALOG)	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unlike the icon menu and the canvas, the dialog is just an instance of
the PCE class `dialog'.   This approach is taken  because the menus in
the  dialog can  easily  find the references  to  the various parts of
PceDraw they want  to activate.  It  is  cumbersome and unnecessary to
send  the  messages first  to  the   dialog and   from   there  to the
appropriate part of the system.

In  a sense, it would be  cleaner to  send the message  to the overall
drawing tool first and from there to the appropriate part.  This would
provide all the functionality  of the tool menus  with  the tool as  a
whole.  As a drawback, it  implies the code  to actually get something
done will be spread over three places instead of two:

	* The menu
	* class draw
	* The part that takes care of the actual function.

First of all, a number of obtainers and messages that can be reused in
the remainder of  the  menu  are  created.  This  approach   has   two
advantages over doing it `in-place':

	* By giving it a name, it becomes clear which part of the
	  system is referred to or what function the message realises
	* It exploits the reusability of messages and obtainers: only
	  one such object is used for all the menus.

Next, the various dialog_items are attached to the dialog.  Note again
the  use of the  new/2 construct  in send  to get the  references.  By
using   `Dialog   ->append'   the   dialog_items  are  placed   in   a
two-dimensional grid.   They are given  a  position when the dialog is
created using the `Dialog ->layout' method.

Finally,    the (popup) menus of    the   menu_bar are filled.     The
initialisation arguments of class menu_item are:

	# Value
	  Used to refer to the item.  When the <->message of the menu_item
	  is @default and there is a message attached to the menu,
	  this value is forwarded via the message as @arg1.
	# Message
	  This message is sent when the item is selected.
        # Label
	  This is a name or image.  When @default, a default label
	  will be computed from the value. See `menu_item <-default_label'.
	# Condition
	  This message will be evaluated just before the menu is
	  shown.  When it succeeds the item will be active, otherwise
	  it will be inactive (greyed).  The evaluation of all condition
	  messages in a menu should be fast for good interactive response.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(D) :-
	new(Draw, D?frame),
	new(Canvas, Draw?canvas),
	new(Menu, Draw?menu),
	new(Selection, Canvas?selection),
	new(NonEmptySelection, not(message(Selection, empty))),
	new(NonEmptyDrawing, not(message(Canvas?graphicals, empty))),
	new(HasCurrentFile, Canvas?file \== @nil),

	send(D, append, new(MB, menu_bar(actions))),
	send(D, append, label(feedback, 'Welcome to PceDraw'), right),

	send(MB, append, new(F, popup(file))),
	send(MB, append, new(P, popup(proto))),
	send(MB, append, new(E, popup(edit))),
	send(MB, append, new(S, popup(settings))),

	send_list(F, append,
		  [ menu_item(about,
			      message(Draw, about))
		  , menu_item(help,
			      message(Draw, help),
			      @default, @on)
		  , menu_item(load,
			      message(Canvas, load_from))
		  , menu_item(import,
			      message(Canvas, import),
			      @default, @on,
			      NonEmptyDrawing)
		  , menu_item(save,
			      message(Canvas, save),
			      @default, @off,
			      and(NonEmptyDrawing,
				  Canvas?modified == @on,
				  HasCurrentFile))
		  , menu_item(save_as,
			      message(Canvas, save_as),
			      @default, @on,
			      NonEmptyDrawing)
		  , menu_item(postscript,
			      message(Canvas, postscript),
			      @default, @off,
			      HasCurrentFile)
		  , menu_item(postscript_as,
			      message(Canvas, postscript_as),
			      @default, @off,
			      NonEmptyDrawing)
		  , menu_item(print,
			      message(Canvas, print),
			      @default, @on,
			      NonEmptyDrawing)
		  , menu_item(quit,
			      message(Draw, quit),
			      @default, @off)
		  ]),
	send_list(P, append,
		  [ menu_item(create,
			      message(Menu, create_proto, Selection),
			      @default, @off,
			      NonEmptySelection)
		  , menu_item(delete,
			      message(Menu, delete),
			      @default, @on,
			      message(Menu, can_delete))
		  , menu_item(load,
			      message(Menu, load_from),
			      @default, @off)
		  , menu_item(save,
			      message(Menu, save),
			      @default, @off,
			      Menu?modified == @on)
		  , menu_item(save_as,
			      message(Menu, save_as),
			      @default, @on,
			      Menu?modified == @on)
		  ]),
	send_list(E, append,
		  [ menu_item(expose,
			      message(Canvas, expose_selection),
			      @default, @off,
			      NonEmptySelection)
		  , menu_item(hide,
			      message(Canvas, hide_selection),
			      @default, @on,
			      NonEmptySelection)
		  , menu_item(align,
			      message(Canvas, align_selection),
			      @default, @on,
			      Selection?size > 1)
		  , menu_item(edit_attributes,
			      message(Canvas, edit_selection),
			      @default, @on,
			      NonEmptySelection)
		  , menu_item(duplicate,
			      message(Canvas, duplicate_selection),
			      @default, @off,
			      NonEmptySelection)
		  , menu_item(cut,
			      message(Canvas, cut_selection),
			      @default, @off,
			      NonEmptySelection)
		  , menu_item(copy,
			      message(Canvas, copy_selection),
			      @default, @off,
			      NonEmptySelection)
		  , menu_item(paste,
			      message(Canvas, paste),
			      @default, @on,
			      message(@prolog, exists_clipboard))
		  , menu_item(import_image,
			      message(Canvas, import_image),
			      @default, @on)
		  , menu_item(import_frame,
			      message(Canvas, import_frame),
			      @default, @on)
		  , menu_item(clear,
			      message(Canvas, clear, @on),
			      @default, @off,
			      NonEmptyDrawing)
		  ]),

	send(S, multiple_selection, @on),
	send(S, on_image, @mark_image),
	send_list(S, append,
		  [ menu_item(auto_align,
			      message(Canvas, auto_align_mode, @arg1))
		  ]),

	(   get(Draw, resource_value, auto_align_mode, @on)
	->  send(S, selected, auto_align, @on),
	    send(Canvas, auto_align_mode, @on)
	;   true
	).


exists_clipboard :-
	object(@draw_clipboard).


		/********************************
		*      INITIAL PROTOTYPES	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the menu of  the drawing tool with  the  standard options.  After
initialising the  menu,  its <->modified  status  is  set to  @off  to
indicate saving is not necessary.  See the file `menu.pl' for details.

Class draw_menu  defines  `draw_menu ->proto'.   The first argument is
the prototype, the second the associated mode and the third the cursor
that should be displayed in this mode.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_menu(M) :-
	send(M, proto, @nil,		   select,	   top_left_arrow),
	send(M, proto, @nil,		   draw_edit,	   xterm),
	send(M, proto, draw_text(''),	   draw_text,	   xterm),
	send(M, proto, draw_box(0,0), 	   draw_resize,    crosshair),
	send(M, proto, draw_ellipse(0,0),  draw_resize,    crosshair),
	send(M, proto, draw_line(0,0,0,0), draw_line,      crosshair),
	send(M, proto, new(draw_path),	   draw_path,      cross),
	send(M, proto, link(link),	   draw_connect,   plus),
	send(M, proto, link(unique),	   draw_cconnect,  plus),
	send(M, modified, @off).


		/********************************
		*        FINDING PARTS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The methods below provide  access to the  various parts of the drawing
tool.  It makes  it  easier to remember  how to  access the  parts and
allows for changing the classnames without affecting too much code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

dialog(Draw, D) :<-
	"Find the dialog of the tool"::
	get(Draw, member, dialog, D).

canvas(Draw, C) :<-
	"Find the drawing canvas"::
	get(Draw, member, draw_canvas, C).

menu(Draw, C) :<-
	"Find the icon menu"::
	get(Draw, member, draw_menu, C).


		/********************************
		*             MODES		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceDraw can operate in various modes.   A mode defines what happens on
a left-button-down event  (ms_left_down).  The various recognisers for
left-button events are only sensative when  the  draw_canvas is in the
appropriate modes.

->mode and ->proto pass messages from the menu to the appropriate part
of PceDraw (the canvas).  As discussed above, it as  ok for the dialog
to send messages directly to the parts.  Why  is it not  ok to do this
from the menu?  The answer is that the  menu is defined in a different
module of the system.   It could be reusable  in a  different  context
(for example in a prototype  editor), where the  overall tool wants to
implement mode switches differently.  Note  that  through <-frame  the
menu has generic access to the tool it is part of.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

mode(Draw, Mode:name, Cursor:cursor) :->
	"Switch the mode"::
	send(Draw?canvas, mode, Mode, Cursor).


proto(Draw, Proto:'graphical|link*') :->
	"Switch the current prototype"::
	send(Draw?canvas, proto, Proto).


		/********************************
		*            FEEDBACK		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method ->feedback as defined here provides a general mechanism for
any part of PceDraw to print a (short) feedback message:

	send(MySelf?frame, feedback, string('%s: No such file', File))

NOTE:	This mechanism should be exploited further in PCE itself by
	providing sensible defaults for feedback handling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

feedback(Draw, Str:string) :->
	"Print feedback message in dialog"::
	send(Draw?dialog?feedback_member, selection, Str).


about(_Draw) :->
	"Print `about' message"::
	send(@display, inform, '%s\n\n%s\n%s\n%s\n',
	     'PceDraw version 1.1',
	     'By',
	     'Jan Wielemaker',
	     'E-mail: jan@swi.psy.uva.nl').


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->help method  opens a view  with the help-text.  Currently, there
are   no provisions  for    PCE to find   the   help-file.   Using the
library_directory/1  predicate   should  be  considered  a   temporary
solution.

The  code below  is  dubious.   In  a larger application  with various
possibilities  for getting help  one should introduce a separate  help
system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

help(Draw) :->
	"Show window with help-text"::
	(   library_directory(Dir),
	    concat(Dir, '/draw/draw.hlp', HelpText),
	    new(File, file(HelpText)),
	    send(File, exists)
	->  new(V, view('PceDraw: help')),
	    new(D, dialog),
	    send(D, append, button(quit, message(V, free))),
	    send(D, below, V),
	    send(V, load, File),
	    (   send(File, access, write)
	    ->  send(V, editable, @on)
	    ;   send(V, editable, @off)
	    ),
	    send(V, open),
	    send(V, confirm_done, @off)
	;   send(Draw, report, error, 'Can''t find help file `draw.hlp''')
	).


		/********************************
		*              QUIT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Quit PceDraw.  This is rather simplistic.   The code should both check
for modifications in the prototype  database and for  the drawing.  If
one or  both of  them has changed  a  window indicating what has  been
modified  should be  displayed, allowing the  user to save and/or quit
PceDraw.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

quit(Draw) :->
	"Leave draw"::
	get(Draw, canvas, Canvas),
	(   get(Canvas, modified, @on)
	->  new(D, dialog),
	    send(D, transient_for, Draw),
	    send(D, append, label(message, 'Drawing has changed')),
	    send(D, append, button('Save & Quit',
				   message(D, return, save_and_quit))),
	    send(D, append, button(quit,
				   message(D, return, quit))),
	    send(D, append, button(cancel,
				   message(D, return, cancel))),
	    get(D, confirm_centered, Rval),
	    send(D, destroy),
	    (   Rval == save_and_quit
	    ->  send(Canvas, save),
	        send(Draw, destroy)
	    ;   Rval == quit
	    ->  send(Draw, destroy)
	    )
	;   (   send(@display, confirm, 'Quit PceDraw?')
	    ->  send(Draw, destroy)
	    ;   fail
	    )
	).

:- pce_end_class.	
