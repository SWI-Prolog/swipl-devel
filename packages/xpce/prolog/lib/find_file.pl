/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Find a file
*/


:- module(pce_file, []).

:- use_module(library(pce)).
:- use_module(file_item).
:- require([ concat/3
	   , ignore/1
	   , send_list/3
	   ]).


		 /*******************************
		 *         CLASS FINDER		*
		 *******************************/

:- pce_begin_class(finder, frame).

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

	send(F, append, new(P, dialog)),
	fill_window(P),
	send(F, create),
	send(F, center).


window(F, W:window) :<-
	"Return main window"::
	get(F, member, dialog, W).


fill_window(P) :-
	new(F, P?frame),
	new(OK, ?(P, member, ok)),

	send(P, cursor, top_left_arrow),

	send(P, display, label(reporter), point(10, 5)),

	send(P, display,
	     new(DI, directory_item(directory, '',
				    message(F, directory, @arg1))),
	     point(10, 25)),
	send(P, display,
	     new(FI, text_item(file, '', message(OK, execute))),
	     point(10, 45)),
	send(FI, value_set, ?(@prolog, complete_file, DI?selection, @arg1)),
	send_list([DI, FI], width, 33),
	send(FI, label_width, DI?label_width),

	send(P, display, button(up, message(F, up)), point(10, 70)),
	send(P, display, button(ok, message(F, ok)), point(10, 130)),
	send(P, display, button(cancel, message(F, cancel)), point(10,190)),

	send(P, display, new(L, list_browser), point(100, 70)),
        send(L, do_set, @default, @default, 220, 143),
	send(L, select_message, message(F, select, @arg1?key)),
	send(L, open_message, message(OK, execute)),
	send(L, recogniser, handler(area_enter,
				    message(L?window, keyboard_focus, L))),

	get(P, bounding_box, BB),
        send(P, size, size(BB?width + 20, BB?height + 10)).


complete_file(Dir, Prefix, Files) :-
	get(directory(Dir), files, string('^%s', Prefix), Files).


select(F, Name:string) :->
	"Handle selection from browser"::
	get(F?window, file_member, FI),
	(   send(Name, suffix, /),
	    get(Name, scan, '%[^/]/', vector(string(DirName)))
	->  get(F?window, directory_member, DI),
	    send(F, directory, string('%s/%s', DI?selection, DirName))
	;   send(FI, selection, Name)
	).


up(F) :->
	"Goto parent directory"::
	(   send(F, directory, F?directory?parent)
	->  true
	;   send(F, report, error, 'Can''t find parent directory')
	).


selection(F, File:file) :<-
	"Get the currently selected file"::
	get(F, window, P),
	get(P?directory_member, selection, DirName),
	get(P?file_member, selection, FileName),
	FileName \== '',
	get(F, extension, Ext),
	(Ext == '' -> Extension = '' ; concat(',', Ext, Extension)),
	clean_name(DirName, /, CleanDir),
	clean_name(FileName, Ext, CleanFile),
	send(F, slot, directory, DirName),
	new(File, file(string('%s%s', CleanDir, CleanFile))).


clean_name(Name, Ext, Clean) :-
	new(S, string(Name)),
	send(S, strip),
	(   send(S, suffix, Ext)
	->  true
	;   send(S, append, Ext)
	),
	get(S, value, Clean),
	send(S, free).


ok(F) :->
	"User pressed ok"::
	get(F, selection, File),
	(   get(F, exists, @on)
	->  (   send(File, exists)
	    ->  send(F, return, File?name)
	    ;   send(F, report, error, '%s: No such file', File?name)
	    )
	;   send(F, return, File?name)
	).


cancel(F) :->
	"User pressed cancel"::
	send(F, return, @nil),
	send(F, show, @off),			  % savety to get rid of it
	send(F, transient_for, @nil).


directory(F, Dir:[directory], Ext:[name]) :->
	"Set current directory and fill browser"::
	(   Dir \== @default -> send(F, slot, directory, Dir) ; true ),
	(   Ext \== @default -> send(F, slot, extension, Ext) ; true ),
	get(F, window, P),
	get(P, member, list_browser, B),
	get(F, directory, D),

	send(P?directory_member, selection, D?path),
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
	;   get(AllFiles, find_all, message(@arg1, suffix, Extension), Files),
	    send(AllFiles, done)
	),
	send(Dirs, for_all, message(B, append,
				       ?(@pce, instance,
					 string, '%s/', @arg1))),
	send(Files, for_all, message(B, append, @arg1)),
	send(Dirs, done),
	send(Files, done),
	
	send(P?file_member, clear).


file(F, Exists:[bool], Ext:[name],
     Dir:[directory], Default:[file], File:name) :<-
 	"Get [existing] file with [extension]"::
	send(F, report, status, ''),
	(   Exists \== @default -> send(F, exists, Exists) ; true ),
	default(Ext, '', Extension),
	ignore(send(F, directory, Dir, Extension)),
	(   Default \== @default
	->  send(F?window?file_member, selection, Default?base_name)
	;   true
	),
	send(F, show, @on),
 	(   get(@event, '_value', @nil)		% no current event
	->  true
	;   send(F, transient_for, @event?window)
	),
	get(F, confirm, File),
	send(F, transient_for, @nil),
	send(F, show, @off),
	File \== @nil.

:- pce_end_class.


test :-
	get(@finder, file, File),
	format('File = ~w~n', File).
