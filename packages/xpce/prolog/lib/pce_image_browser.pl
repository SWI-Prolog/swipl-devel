/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
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

selected_image(IB, Img:image) :->
	"User selected an image"::
	send(IB, selection, Img),
	(   get(IB, select_message, Msg),
	    Msg \== @nil
	->  send(Msg, forward, Img)
	;   true
	).

open_image(IB, Img:image) :->
	"User double-clicked an image"::
	(   get(IB, open_message, Msg),
	    Msg \== @nil
	->  send(Msg, forward, Img)
	;   true
	).

selected_dir(IB, Dir:directory) :->
	(   get(IB, display, Display)
	->  send(Display, busy_cursor),
	    ignore(send(IB, directory, Dir)),
	    send(Display, busy_cursor, @nil)
	;   send(IB, directory, Dir)
	).

report_image(IB, Image:image*) :->
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


show_dirs(IB, Dirs:chain) :->
	get(IB, directory, CWD),
	send(Dirs, for_all, message(IB, show_dir, ?(CWD, directory, @arg1))).


show_files(IB, Files:chain) :->
	get(IB, directory, CWD),
	send(Files, for_all, message(IB, show_file, ?(CWD, file, @arg1))).


show_dir(IB, Dir:directory, Label:[name]) :->
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


show_file(IB, File:file) :->
	get(File, base_name, BaseName),
	(   get(IB, extensions, Exts),
	    Exts \== @nil
	->  file_name_extension(_Base, Ext, BaseName),
	    send(Exts, member, Ext)
	;   true
	),
	ignore(send(IB, report, progress, 'Checking %s ...', BaseName)),
	new(Img, image),
	(   pce_catch_error(bad_file, send(Img, load, File))
	->  new(I, image_browser_file_item(Img)),
	    send(I, show_file_label, IB?show_file_labels),
	    send(IB, display, I),
	    send(IB, flush)
	;   true
	).


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
		get(File, base_name, Name),
		send(D, display, text(Name))
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
	send(BM, selected, Val).
	       
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
