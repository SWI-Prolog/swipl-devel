/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(draw,
	[ draw/0				  % Start drawing tool
	, draw/1				  % Start editing file
	]).

draw_version(4.2).

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
:- use_module(library(pce_config)).
:- ensure_loaded(library(help_message)).
:- require([ default/3
	   , file_base_name/2
	   , file_name_extension/3
	   , listen/3
	   , pce_help_file/2
	   , send_list/3
	   , unlisten/1
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With this declaration we load the other Prolog modules of PceDraw.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- consult([ config			% Configuration declarations
	   , gesture			% Gestures
	   , shapes			% Drawable shapes
	   , canvas			% Drawing plain
	   , menu			% Icon Menu
	   , undo			% Undo Recording
	   , exportpl			% Export using drag-and-drop
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

%:- pce_image_directory(library('draw/bitmaps')).

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
	send(Draw, open, normalise := @on).

draw(File) :-
	file_name_extension(File, pd, PdFile),
	new(Draw, draw),
	send(Draw, open),
	get(Draw, canvas, Canvas),
	(   send(file(PdFile), exists)
	->  send(Canvas, load, PdFile, @on)
	;   send(Canvas, file, PdFile)
	).


		/********************************
		*           CLASS DRAW		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `draw' defines and manages  the entire tool.  Its initialisation
builds  the  entire  tool and the  resulting instance provide means of
communication between the various parts.  The call
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw, frame,
		   "The PceDraw application class").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
starts  the definition of  a new class  `draw' that  is a subclass  of
class  frame.  Classes should always  be  a subclass of  some existing
class.  If  there is  no particular PCE  class  to inherit  from, this
should be class `object', the root of the PCE class hierarchy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(title,		name,	get, "Base-name of the program").
variable(version,	real,   get, "Current version").


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

To avoid a giant clause and improve   the  possibilities to refine the
drawing tool using subclassing, the method  ->fill_dialog is called to
fill the dialog rather than putting this code in ->initialise.

For  PCE-3  users, note the  use of the term new/2  in the  second and
further  sends to create  the windows  inline   and get the reference.
This approach is preferred over a separate new/2  and ->append.  It is
shorter but -more important- it attaches the canvas immediately to the
frame, making the frame responsible for its destruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(Draw, Title:[name]) :->
	fix_fonts,
	ensure_loaded_config(draw_config:_ConfigFile),
	default(Title, 'PceDraw', TheTitle),
	send(Draw, send_super, initialise, TheTitle),
	send(Draw, slot, title, TheTitle),
	draw_version(Version),
	send(Draw, slot, version, Version),
	send(Draw, done_message, message(Draw, quit)),
	send(Draw, append, new(Canvas, draw_canvas)),
	send(new(Menu, draw_menu), left, Canvas),
	send(new(D, dialog), above, Menu),
	send(Draw, fill_dialog, D),
	send(Draw, fill_menu),
	send(Menu, activate_select),
	(   get_config(draw_config:history/geometry/main_window, Geometry)
	->  send(Draw, geometry, Geometry)
	;   true
	),
	listen(Draw,
	       set_config(draw_config:resources/default_font, Font),
	       send(Draw, default_font, Font)).


unlink(Draw) :->
	unlisten(Draw),
	get(Draw, geometry, Geometry),
	send(Draw, send_super, unlink),
	set_config(draw_config:history/geometry/main_window, Geometry).


%  The Windows win_ansi font reproduces poorly in exported metafiles, hence
%  we force the usage of a normal font.

fix_fonts :-
	send(@display, open),
	get(@pce, convert, normal, font, Font),
	get(Font, family, win_ansi), !,
	send(@display, font_alias, normal, font(helvetica, roman, 12), @on).
fix_fonts.

resize(Draw) :->
	send(Draw, send_super, resize),
	get(Draw, menu, MenuWindow),
	send(MenuWindow, adjust).


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

fill_dialog(Draw, D:dialog) :->
	"Fill the top-dialog window"::
	new(Canvas, Draw?canvas),
	new(Menu, Draw?menu),
	new(Selection, Canvas?selection),
	new(NonEmptySelection, not(message(Selection, empty))),
	new(OneSelected, Selection?size == 1),
	new(SelectionIsConnection,
	    and(OneSelected,
		message(Selection?head, instance_of, connection))),
	new(NonEmptyDrawing, not(message(Canvas?graphicals, empty))),
	new(HasCurrentFile, Canvas?file \== @nil),
	new(HasMetaFile, ?(@pce, convert, win_metafile, class)),

	send(D, append, new(MB, menu_bar(actions))),
	send(D, append, new(RL, label(reporter, 'Welcome to PceDraw')), right),
	send(RL, alignment, left),

	send(MB, append, new(F, popup(file))),
	send(MB, append, new(E, popup(edit))),
	send(MB, append, new(P, popup(proto))),
	send(MB, append, new(S, popup(settings))),

	new(UM, message(@arg1?frame, select_mode)),
	send_list([F, E, S], update_message, UM),

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
		  , new(SaveAsMetaFile, popup(save_as_metafile))
		  , menu_item(print,
			      message(Canvas, print),
			      @default, @on,
			      NonEmptyDrawing)
		  , menu_item(new_window,
			      message(Draw, new_window),
			      end_group := @on)
		  , new(RF, menu_item(recent_file,
			      end_group := @on))
		  , menu_item(quit,
			      message(Draw, quit),
			      @default, @off)
		  ]),
	send(RF, popup,
	     new(RFP, popup(RF?value,
			    message(Draw, load_recent_file, @arg1)))),
	send(RFP, update_message, message(Draw, fill_recent_files, RFP)),
	send_list(P, append,
		  [ new(CreateProto, popup(create))
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
	send_list(CreateProto, append,
		  [ menu_item(as_is,
			      message(Menu, create_proto, Selection, as_is),
			      @default, @off,
			      and(NonEmptySelection,
				  not(SelectionIsConnection))),
		    menu_item(virgin,
			      message(Menu, create_proto, Selection, virgin),
			      @default, @off,
			      OneSelected)
		  ]),
	new(UndoBuffer, Canvas?undo_buffer),
	send_list(E, append,
		  [ menu_item(undo,
			      message(UndoBuffer, open, Draw),
			      condition := message(UndoBuffer, can_undo))
		  , menu_item(send_to_foreground,
			      message(Canvas, expose_selection),
			      @default, @off,
			      NonEmptySelection)
		  , menu_item(send_to_background,
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
		  , menu_item(select_all,
			      message(Canvas, select_all),
			      condition := NonEmptyDrawing)
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
		  , new(ClipBoard, popup(clipboard))
		  , menu_item(import_image,
			      message(Canvas, import_image),
			      @default, @on)
		  , menu_item(import_frame,
			      message(Canvas, import_frame),
			      @default, @on)
		  , menu_item(clear,
			      and(message(Draw, select_mode),
				  message(Canvas, clear, @on)),
			      @default, @off,
			      NonEmptyDrawing)
		  ]),

	send(ClipBoard, end_group, @on),
	send(ClipBoard?context, condition, HasMetaFile),
	send_list(ClipBoard, append,
		  [ menu_item(clip_drawing,
			      message(Canvas, export_win_metafile, drawing),
			      condition := NonEmptyDrawing),
		    menu_item(clip_selection,
			      message(Canvas, export_win_metafile, selection),
			      condition := NonEmptySelection,
			      end_group := @on),
		    menu_item(paste,
			      message(Canvas, import_win_metafile))
		  ]),

	send(SaveAsMetaFile, end_group, @on),
	send(SaveAsMetaFile?context, condition, and(NonEmptyDrawing,
						    HasMetaFile)),
	send_list(SaveAsMetaFile, append,
		  [ menu_item(current_file,
			      message(Canvas, save_default_windows_metafile),
			      condition := HasCurrentFile),
		    menu_item(ask_file,
			      message(Canvas, windows_metafile))
		  ]),

	send(S, multiple_selection, @on),
	send(S, show_current, @on),
	send_list(S, append,
		  [ menu_item(auto_align,
			      message(Canvas, auto_align_mode, @arg1),
			      end_group := @on),
		    menu_item(preferences,
			      and(message(Draw, preferences),
				  message(S, selected, preferences, @off)),
			      'Preferences ...')
		  ]),

	(   get_config(draw_config:edit/auto_align, @on)
	->  send(S, selected, auto_align, @on),
	    send(Canvas, auto_align_mode, @on)
	;   true
	),
	send(D, append, new(DDD, draw_drag_drawing), right),
	send(DDD, reference, point(0, 10)),
	send(DDD, alignment, right),
	send(D, resize_message, message(D, layout, @arg2)).


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

fill_menu(Draw) :->
	"Fill <-menu with standard prototypes"::
	get(Draw, menu, M),
	send(M, proto, @nil,
	     select, top_left_arrow, tag:='Select mode'),
	send(M, proto, draw_text(''),
	     draw_text, xterm, tag:='Add text'),
	send(M, proto, draw_box(0,0),
	     draw_resize, crosshair, tag:='Add box'),
	send(M, proto, draw_ellipse(0,0),
	     draw_resize, crosshair, tag:='Add ellipse'),
	send(M, proto, draw_line(0,0,0,0),
	     draw_line, crosshair, tag:='Add line'),
	send(M, proto, new(draw_path),
	     draw_path, cross, tag:='Add multi-part/smooth line'),
	send(M, proto, draw_bezier(point(0,0), point(0,0), point(0,0)),
	     draw_bezier, crosshair, tag:='Add Bezier curve'),
	send(M, proto, link(link),
	     draw_connect, plus, tag:='Link two objects'),
	send(M, proto, link(unique),
	     draw_cconnect, plus, tag:='Link at choosen position'),
	send(M, modified, @off).


default_font(Draw, Font:font) :->
	"Change the default font"::
	(   get(Draw, menu, Menu),
	    get(Menu, find_icon,
		message(@arg1?proto, instance_of, text),
		Icon)
	->  get(Icon, proto, Text),
	    send(Text, font, Font),
	    send(Icon, paint_proto)
	;   true
	).
	

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

canvas(Draw, C:draw_canvas) :<-
	"Find the drawing canvas"::
	get(Draw, member, draw_canvas, C).

menu(Draw, C:draw_menu) :<-
	"Find the icon menu window"::
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
<menu has generic access to the tool it is part of.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

select_mode(Draw) :->
	"Switch to select mode"::
	(   get(Draw?canvas, mode, select)
	->  true
	;   send(Draw?menu, activate_select)
	).


mode(Draw, Mode:name, Cursor:cursor) :->
	"Switch the mode"::
	send(Draw?canvas, mode, Mode, Cursor).


proto(Draw, Proto:'graphical|link*') :->
	"Switch the current prototype"::
	send(Draw?canvas, proto, Proto).


		/********************************
		*            ABOUT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
About simply sends a message  to  @display   to  inform  the user of the
package.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

about(_Draw) :->
	"Print `about' message"::
	draw_version(Version),
	send(@display, inform, '%s%.1f\n\n%s\n%s\n',
	     'PceDraw version ',
	     Version,
	     'Author: Jan Wielemaker',
	     'E-mail: jan@swi.psy.uva.nl').


update_label(Draw) :->
	"Update the label of the window"::
	get(Draw?canvas, file, File),
	get(Draw, title, Title),
	(   File \== @nil
	->  send(Draw, label,
		 string('%s: %s', Title, File?name),
		 string('%s', File?base_name))
	;   send(Draw, label, Title)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->help method  opens a view  with the help-text.  Currently, there
are   no provisions  for    PCE to find   the   help-file.   Using the
library_directory/1  predicate   should  be  considered  a   temporary
solution.

The  code below  is  dubious.   In  a larger application  with various
possibilities  for getting help  one should introduce a separate  help
system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_help_file(pcedraw, pce_help('pcedraw.hlp')).

help(_Draw) :->
	"Show window with help-text"::
	send(@helper, give_help, pcedraw, main).


		 /*******************************
		 *	    PREFERENCES		*
		 *******************************/

preferences(Draw) :->
	"Edit user preferences"::
	edit_config(draw_config:Draw).


		 /*******************************
		 *	    NEW WINDOW		*
		 *******************************/

new_window(Draw) :->
	"Create a new window"::
	get(Draw, class_name, Class),
	send(new(Class), open).


		 /*******************************
		 *	   RECENT FILES		*
		 *******************************/

fill_recent_files(_Draw, Popup:popup) :->
	"Fill `recent-file' pull-right menu"::
	send(Popup, clear),
	(   get_config(draw_config:history/recent_files, Files)
	->  append_files_to_menu(Files, Popup)
	;   true
	).

append_files_to_menu([], _).
append_files_to_menu([H|T], Menu) :-
	file_base_name(H, Base),
	send(Menu, append, menu_item(H, @default, Base)),
	append_files_to_menu(T, Menu).


load_recent_file(Draw, File:file) :->
	"Load file from recent file menu"::
	get(Draw, canvas, Canvas),
	(   send(File, exists)
	->  send(Canvas, load, File, @on)
	;   send(Draw, report, error, 'No such file'),
	    get(File, name, Path),
	    delete_recent_file(Path)
	).

delete_recent_file(Path) :-
	Key = draw_config:history/recent_files,
	(   get_config(Key, Set0)
	;   Set0 = []
	),
	delete(Set0, Path, Set),
	set_config(Key, Set).


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
	send(Canvas, save_if_modified, @on),
	(   object(Draw)
	->  send(Draw, destroy)
	;   true
	).

:- pce_end_class.	
