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

:- module(pce_image_browser, []).
:- use_module(library(pce)).
:- require([ concat_atom/3
	   , default/3
	   , file_name_extension/3
	   , ignore/1
	   ]).

resource(dir,	image, image('16x16/closedir.xpm')).

:- pce_begin_class(image_browser, window,
		   "Browser for image files").

variable(extensions,	     chain*,	   get,  "List of extensions").
variable(directory,	     directory,	   get,  "Current directory").
variable(show_file_labels,   bool := @off, get,  "Show names of the files").
variable(select_message,     code*,	   both, "Message executed on select").
variable(open_message,	     code*,	   both, "Message executed on open").
variable(multiple_selection, bool := @off, get,  "Can select multiple?").


		 /*******************************
		 *	  EVENT HANDLING	*
		 *******************************/

:- pce_global(@imgbrowser_dir_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver?window, selected_dir,
					@receiver?directory)))).
:- pce_global(@imgbrowser_file_recogniser, make_imgbrowser_file_recogniser).
:- pce_global(@imgbrowser_icon_format, make_imgbrowser_icon_format).

make_imgbrowser_icon_format(Fmt) :-
	new(Fmt, format(horizontal, 1, @on)),
	send(Fmt, adjustment, vector(center)),
	send(Fmt, row_sep, 0).

make_imgbrowser_file_recogniser(G) :-
	new(Image, ?(@receiver, member, bitmap)?image),
	new(IB, @receiver?window),
	new(LC, click_gesture(left, '', single,
			      message(IB, selected_image, Image))),
	new(LD, click_gesture(left, '', double,
			      message(IB, open_image, Image))),
	new(A, handler(area_enter,
		       message(IB, report_image, Image))),
	new(Z, handler(area_exit,
		       message(IB, report_image, @nil))),
	new(G, handler_group(LC, LD, A, Z)).

		 /*******************************
		 *	      BUILD		*
		 *******************************/

initialise(IB, Dir:[directory], Exts:[chain]) :->
	default(Exts, @nil, TheExts),
	send(IB, send_super, initialise),
	send(IB, scrollbars, vertical),
	send(IB, slot, extensions, TheExts),
	(   Dir \== @default
	->  send(IB, directory, Dir)
	;   true
	),
	send(IB, resize_message,
	     message(IB, format,
		     create(format, horizontal, @arg2?width, @off))).

:- pce_group(selection).

selection(IB, Sel:'image|chain*') :->
	"Set selected image(s)"::
	send(IB?graphicals, for_all,
	     if(message(@arg1, instance_of, image_browser_file_item),
		message(@arg1, adjust_selected, Sel))).
selection(IB, Sel:'image|chain') :<-
	"Fetch selection"::
	(   get(IB, multiple_selection, @on)
	->  new(Sel, chain),
	    send(IB?graphicals, for_all,
		 if(and(message(@arg1, instance_of, image_browser_file_item),
			message(@arg1, selected)),
		    message(Sel, append, @arg1?image)))
	;   get(IB?graphicals, find,
		and(message(@arg1, instance_of, image_browser_file_item),
		    message(@arg1, selected)),
		Item),
	    get(Item, image, Sel)
	).


:- pce_group(event).

%	busy(+Visual, +Goal)
%
%	Run goal while showing busy cursor.

busy(B, Goal) :-
	get(B, display, Display), !,
	send(Display, busy_cursor),
	(   catch(Goal, E, true)
	->  send(Display, busy_cursor, @nil),
	    (	var(E)
	    ->	true
	    ;	throw(E)
	    )
	;   send(Display, busy_cursor, @nil),
	    fail
	).
busy(_, Goal) :-
	Goal.				% no display

selected_image(IB, Img:image) :->
	"User selected an image"::
	send(IB, selection, Img),
	(   get(IB, select_message, Msg),
	    Msg \== @nil
	->  busy(IB, send(Msg, forward, Img))
	;   true
	).

open_image(IB, Img:image) :->
	"User double-clicked an image"::
	(   get(IB, open_message, Msg),
	    Msg \== @nil
	->  busy(IB, send(Msg, forward, Img))
	;   true
	).

selected_dir(IB, Dir:directory) :->
	"User selected a sub-directory: open it"::
	busy(IB, send(IB, directory, Dir)).

report_image(IB, Image:image*) :->
	"Report on the status of an image"::
	(   Image == @nil
	->  send(IB, report, status, '')
	;   image_name(Image, Name),
	    get(Image, kind, Kind),
	    get(Image, size, size(W, H)),
	    (	get(Image, mask, Mask),
		Mask \== @nil
	    ->	Comment = ' (masked)'
	    ;	Comment = ''
	    ),
	    send(IB, report, status,
		 '%s containing a %d * %d %s%s',
		 Name, W, H, Kind, Comment)
	).


image_name(Image, Name) :-
	get(Image, name, Name),
	Name \== @nil, !.
image_name(Image, Name) :-
	get(Image, file, File),
	(   send(File, instance_of, file)
	->  get(File, base_name, Name)
	;   get(File, name, Name)
	).

:- pce_group(render).

directory(IB, Dir:directory) :->
	"Read a directory, adding images and sub-dirs"::
	send(IB, slot, directory, Dir),
	send(IB, clear),
	new(Files, chain),
	new(Dirs, chain),
	send(Dir, scan, Files, Dirs),
	get(Dir, directory, '..', Up),
	send(IB, show_dir, Up, '..'),
	send(IB, show_dirs, Dirs),
	send(IB, show_files, Files),
	send(IB, scroll_to, point(-10,-10)),
	ignore(send(IB, report, done)).

refresh(IB) :->
	"Update contents"::
	(   get(IB, directory, Dir),
	    send(Dir, instance_of, directory)
	->  send(IB, directory, Dir)
	;   true
	).

show_dirs(IB, Dirs:chain) :->
	get(IB, directory, CWD),
	send(Dirs, for_all, message(IB, show_dir, ?(CWD, directory, @arg1))).


show_files(IB, Files:chain) :->
	get(IB, directory, CWD),
	send(Files, for_all, message(IB, check_file, ?(CWD, file, @arg1))).


show_dir(IB, Dir:directory, Label:[name]) :->
	"Add icon for sub-directory"::
	(   Label == @default
	->  get(Dir, name, BaseName)
	;   BaseName = Label
	),
	new(F, device),
	send(F, format, @imgbrowser_icon_format),
	send(F, display, bitmap(resource(dir))),
	send(F, recogniser, @imgbrowser_dir_recogniser),
	send(F, display, text(BaseName)),
	send(F, attribute, directory, Dir),
	send(IB, display, F).


check_file(IB, File:file) :->
	"->show_file if it matches the filter"::
	get(File, base_name, BaseName),
	(   (   get(IB, extensions, Exts),
		Exts \== @nil
	    ->  file_name_extension(_Base, Ext, BaseName),
		send(Exts, member, Ext)
	    ;   true
	    )
	->  (   pce_catch_error(bad_file, send(IB, append_file, File))
	    ->	send(IB, synchronise)
	    ;	send(IB, report, warning,
		     '%s is not an image file', BaseName)
	    )
	;   true
	).


append_file(IB, File:file) :->
	"Actually display a file"::
	get(File, base_name, BaseName),
	ignore(send(IB, report, progress, 'Checking %s ...', BaseName)),
	new(Img, image),
	send(Img, load, File),
	send(IB, append, Img).


append(IB, Img:image) :->
	"Append a plain image"::
	new(I, image_browser_file_item(Img)),
	send(I, show_file_label, IB?show_file_labels),
	send(IB, display, I).


show_file_labels(IB, Show:bool) :->
	"Show (base) names of the file below the images?"::
	(   get(IB, show_file_labels, Show)
	->  true
	;   send(IB, slot, show_file_labels, Show),
	    send(IB?graphicals, for_all,
		 if(message(@arg1, instance_of, image_browser_file_item),
		    message(@arg1, show_file_label, Show)))
	).


clear(IB) :->
	send_super(IB, clear),
	send(IB, scroll_to, point(0,0)).


:- pce_end_class.

:- pce_begin_class(image_browser_file_item, device).

initialise(D, Img:image) :->
	send(D, send_super, initialise),
	send(D, format, @imgbrowser_icon_format),
	send(D, display, bitmap(Img)).
	
image(D, Img:image) :<-
	"Fetch the image"::
	get(D, member, bitmap, BM),
	get(BM, image, Img).

show_file_label(D, Show:bool) :->
	(   Show == @on
	->  (   get(D, member, text, _)
	    ->	true
	    ;   get(D, image, Img),
		get(Img, file, File),
		(   File \== @nil
		->  get(File, base_name, Name),
		    send(D, display, text(Name))
		;   true
		)
	    )
	;   (   get(D, member, text, Text)
	    ->	free(Text)
	    ;	true
	    )
	).

adjust_selected(D, Sel:'image|chain*') :->
	"(de)select according to selection"::
	get(D, member, bitmap, BM),
	get(BM, image, Image),
	(   (   send(Sel, instance_of, chain)
	    ->  send(Sel, member, Image)
	    ;   Sel == Image
	    )
	->  Val = @on
	;   Val = @off
	),
	(   Val == @on
	->  send(BM, pen, 2),
	    send(BM, colour, green)
	;   send(BM, pen, 0),
	    send(BM, colour, @default)
	).
	       
selected(D) :->
	"Test of object is selected"::
	get(D, member, bitmap, BM),
	get(BM, selected, @on).

event(D, Ev:event) :->
	(   send(D, send_super, event, Ev)
	;   send(@imgbrowser_file_recogniser, event, Ev)
	).

:- pce_end_class.


		 /*******************************
		 *	    TEST (DEMO)		*
		 *******************************/
/*

test :-
	pce_autoload(directory_item, library(file_item)),
	pce_autoload(tick_box, library(pce_tick_box)),
	get(@pce, home, Home),
	concat_atom([Home, bitmaps], /, StartDir),
	new(IB, image_browser(StartDir)),
	send(new(D1, dialog), above, IB),
	send(D1, append, tick_box(show_file_labels, @off,
				  message(IB, show_file_labels, @arg1))),
	send(D1, append,
	     directory_item(directory, StartDir,
			    message(IB, directory, @arg1)), right),
	send(new(D2, dialog), below, IB),
	send(D2, append, label(reporter)),
	send(IB, open).

*/
