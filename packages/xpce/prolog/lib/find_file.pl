/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Find a file
    History:

	# Oct 4 1995
	Updated under Windows-'95 to use automatic dialog layout rather
	than the fixed position-layout used in the original version. This
	ensures this library cooperates better with different look-and-feel
	styles.  Exploited new `graphical ->right_side' method to fix the
	size of the browser in the dialog.

	Automatically ajusts to case-insensitive completion if necessary.

	# May 28 1998
	Changed required images to use resources.

	# August 13, 1998 
	Complete rewrite to use multiple dialog windows.  This acheives
	a resizeable version of the finder without hacky layout code.
	The external interface remains the same.
*/


:- module(pce_finder, []).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a simple file-finder.  The   normal  way to use this
finder in an application is:

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

	...,
	get(@finder, file, FileName),
	...,
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(pce)).
:- use_module(library(file_item)).
:- require([ concat/3
	   , default/3
	   , ignore/1
	   , send_list/3
	   ]).

resource(dir,	image,	image('16x16/closedir.xpm')).
resource(file,	image,	image('16x16/doc.xpm')).
resource(drive,	image,	image('16x16/drive.xpm')).

		 /*******************************
		 *         CLASS FINDER		*
		 *******************************/

:- pce_begin_class(finder, frame, "Library dialog for finding a file").

variable(exists,	bool,		both,
	 "Should files exist?").
variable(extension,	name,		get,
	 "Extension of the requested file").
variable(directory,	directory,	get,
	 "Current directory").


		/********************************
		*           CREATION		*
		********************************/

initialise(F) :->
	send(F, send_super, initialise, finder, transient),
	send(F, slot, exists, @off),
	send(F, slot, extension, ''),
	send(F, slot, directory, new(directory('.'))),

	send(new(ButtonDialog, dialog), left,
	     new(L, browser(size := size(30, 15)))),
	send(new(ReportDialog, dialog), below, ButtonDialog),
	send(new(ItemDialog, dialog), above, ButtonDialog),
	send(ItemDialog, name, item_dialog),
	send(F, append, L),
	send_list([ItemDialog, ButtonDialog, ReportDialog], pen, 0),

	send(ButtonDialog, resize_message,
	     message(ButtonDialog, layout_dialog,
		     size(5, 0), @arg2, size(0,0))),
	send(ButtonDialog, ver_stretch, 100),
	send(ButtonDialog, ver_shrink, 100),
	send(L, hor_stretch, 100),
	send(L, hor_shrink, 100),
	send(ItemDialog, gap, size(0, 8)),
	send(ItemDialog, resize_message,
	     message(ItemDialog, layout, @arg2)),

	send(ReportDialog, append, new(R, label(reporter))),
	send(ReportDialog, gap, size(0,0)),
	send(R, attribute, hor_stretch, 100),
	send(R, elevation, -1),
	send(ReportDialog, resize_message,
	     message(ReportDialog, layout, @arg2)),

	send(ButtonDialog, append,
	     button(up, message(F, up))),
	send(ButtonDialog, append,
	     new(OK, button(ok, message(F, ok))), next_row),
	send(ButtonDialog, append,
	     button(cancel, message(F, cancel)), next_row),
	send(OK, default_button, @on),
	new(DoOK, message(OK, execute)),

	send(ItemDialog, append,
	     new(DI, finder_directory_item(directory, '',
					   message(F, directory, @arg1)))),
	send(ItemDialog, append,
	     new(FI, text_item(file, ''))),
	send(DI, style, normal),

	send(L, style, directory, style(icon := resource(dir))),
	send(L, style, file,      style(icon := resource(file))),
	send(L, style, drive,     style(icon := resource(drive))),
	send(L, select_message, message(F, select, @arg1?key, @arg1?style)),
	send(L, open_message, DoOK),
	send(L, recogniser,
	     handler(area_enter, message(F, keyboard_focus, L))),
	send(FI, message, DoOK),

	send(F, create),
	send(F, center).


file_item(F, FI:text_item) :<-
	"Item showing the current file"::
	get(F, member, item_dialog, ID),
	get(ID, member, file, FI).

directory_item(F, DI:text_item) :<-
	"Item showing the current directory"::
	get(F, member, item_dialog, ID),
	get(ID, member, directory, DI).


complete_file(_F,
	      Dir:char_array, Prefix:char_array, Extension:name,
	      Files:chain) :<-
	extension_regex(Extension, Ext),
	new(Re, regex(string('^%s.*%s$', Prefix, Ext))),
	(   send(class(file), has_feature, case_sensitive, @off)
	->  send(Re, ignore_case, @on)
	;   true
	),	     
	get(directory(Dir), files, Re, Files).


extension_regex('', '') :- !.
extension_regex(Ext, Re) :-
	get(regex(''), quote, Ext, Re).


select(F, Name:string, Type:{directory,file,drive}) :->
	"Handle selection from browser"::
	get(F, file_item, FI),
	(   Type == directory
	->  get(F, directory_item, DI),
	    send(F, directory, string('%s/%s', DI?selection, Name))
	;   Type == drive
	->  send(F, directory, Name)
	;   send(FI, selection, Name)
	).


up(F) :->
	"Goto parent directory"::
	get(F, directory, Dir),
	(   send(F, directory, Dir?parent)
	->  true
	;   send(F, select_drive)
	).


selection(F, File:file) :<-
	"Get the currently selected file"::
	get(F?file_item, selection, FileName),
	FileName \== '',
	get(F?directory_item, selection, DirName),
	get(F, extension, Ext),
	clean_name(DirName, /, CleanDir),
	clean_name(FileName, Ext, CleanFile),
	send(F, slot, directory, DirName),
	new(File, file(string('%s%s', CleanDir, CleanFile))).


clean_name(Name, Ext, Clean) :-
	new(S, string('%s', Name)),
	send(S, strip),
        (   send(class(file), has_feature, case_sensitive, @off)
	->  send(S, ensure_suffix, Ext, @on)
	;   send(S, ensure_suffix, Ext)
	),
	get(S, value, Clean),
	send(S, free).


ok(F) :->
	"User pressed ok"::
	send(F, report, status, ''),
	(   get(F, selection, File)
	->  (   get(F, exists, @on)
	    ->  (   send(File, exists)
		->  send(F, return, File?name)
		;   send(F, report, error, '%s: No such file', File?name)
		)
	    ;   send(F, return, File?name)
	    )
	;   get(F, directory_item, DI),
	    get(DI, modified, @on)
	->  send(DI, apply)
	;   send(F, report, warning, 'Please select or type a file name'),
	    fail
	).


cancel(F) :->
	"User pressed cancel"::
	send(F, return, @nil),
	send(F, show, @off),			  % savety to get rid of it
	send(F, transient_for, @nil).

ensure_dot('', '') :- !.
ensure_dot(Ext, Ext) :-
	concat('.', _, Ext), !.
ensure_dot(Ext0, Ext) :-
	concat('.', Ext0, Ext).

directory(F, Dir:[directory], Ext0:[name]) :->
	"Set current directory and fill browser"::
	(   Dir \== @default -> send(F, slot, directory, Dir) ; true ),
	(   Ext0 \== @default
	->  ensure_dot(Ext0, Ext),
	    send(F, slot, extension, Ext)
	;   true
	),
	get(F, directory_item, DI),
	get(F, member, browser, B),
	get(F, directory, D),

	send(DI, selection, D?path),
	send(B, clear),
	(   send(D, exists)
	->  true
	;   send(F, report, error, 'No such directory'),
	    fail
	),

	new(AllFiles, chain),
	new(Dirs, chain),
	send(D, scan, AllFiles, Dirs),

	get(F, extension, Extension),
	(   Extension == ''
	->  Files = AllFiles
	;   new(R, regex('')),
	    send(R, pattern, string('.*%s$', ?(R, quote, Extension))),
	    (	send(class(file), has_feature, case_sensitive, @off)
	    ->	send(R, ignore_case, @on)
	    ;	true
	    ),
	    get(AllFiles, find_all, message(R, match, @arg1), Files),
	    send(AllFiles, done)
	),
	send(Dirs, for_all,
	     message(B, append,
		     create(dict_item, @arg1, @default, @nil, directory))),
	send(Files, for_all,
	     message(B, append,
		     create(dict_item, @arg1, @default, @nil, file))),
	send(Dirs, done),
	send(Files, done),
	
	send(F?file_item, clear).


select_drive(F) :->
	"Prepare for drive selection"::
	get(directory('.'), roots, Roots),
	(   get(Roots, size, 1)
	->  send(F, directory, Roots?head)
	;   get(F, member, browser, B),
	    get(F, directory_item, DI),
	    send(DI, clear),
	    send(B, clear),
	    send(Roots, for_all,
		 message(B, append,
		     create(dict_item, @arg1, @default, @nil, drive)))
	).


file(F, Exists:exists=[bool], Ext0:extension=[name],
     Dir:directory=[directory], Default:default=[file], File:name) :<-
 	"Get [existing] file with [extension]"::
	send(F, report, status, ''),
	(   Exists \== @default -> send(F, exists, Exists) ; true ),
	default(Ext0, '', Ext1),
	ignore(send(F, directory, Dir, Ext1)),
	(   Default \== @default
	->  send(F?file_item, selection, Default?base_name)
	;   true
	),
	send(F, show, @on),
 	(   get(@event, '_value', @nil)		% no current event
	->  send(F, application, @nil)
	;   get(@event?window, frame, MainFrame),
	    (	get(MainFrame, application, App),
		App \== @nil
	    ->  send(F, application, App),
	        send(F, modal, application)
	    ;	send(F, modal, transient)
	    ),
	    send(F, transient_for, MainFrame)
	),
	get(F, confirm, File),
	send(F, transient_for, @nil),
	send(F, application, @nil),
	send(F, show, @off),
	File \== @nil.

:- pce_end_class.

:- pce_begin_class(finder_directory_item, directory_item).

selected_completion(DI, Item:char_array, _Apply:[bool]) :->
	send(DI, send_super, selected_completion, Item, @off),
	send(DI, apply, @on).

apply(DI, IfModified:[bool]) :->
	(   (   IfModified == @on
	    ;   get(DI, modified, @on)
	    )
	->  get(DI, selection, Text),
	    get(Text, strip, Stripped),
	    (	send(Stripped, equal, '')
	    ->	send(DI?frame, select_drive)
	    ;	send(DI?frame, directory, Stripped)
	    )
	).

:- pce_end_class.
