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

	send(F, append, new(D, dialog)),
	send(F, fill_dialog, D),
	send(F, create),
	send(F, center).


dialog(F, W:dialog) :<-
	"Return main dialog"::
	get(F, member, dialog, W).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  is  a  difficult  layout.  The   original  version  used  explicit
positions. In the  meanwhile  the  layout   system  of  XPCE  has become
powerful enough to use automatic layout and a few tricks:

To get the three buttons stacked next to the dialog, we first place them
in a device. As we want them to  be far apart, we use explicit positions
here. This isn't too bad as we only   assume  a distance of 60 pixels is
enough to separate two buttons vertically. Next  we append all the items
the normal way to the dialog. This will do the job, but the browser will
not be nicely alligned. Therefore we align the right and bottom sides of
the browser with the text-items and the buttons.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(F, D) :->
	send(D, append, label(reporter)),

	send(D, append,
	     new(DI, directory_item(directory, '',
				    message(F, directory, @arg1)))),
	send(D, append, new(FI, text_item(file, ''))),
	send_list([DI, FI], width, 50),

	send(FI, value_set,
	     ?(F, complete_file, DI?selection, F?extension, @arg1)),
	send(FI, style, normal),	% just use it for completion

	new(Sub, device),
	send(Sub, display, button(up, message(F, up)), point(0, 0)),
	send(Sub, display, new(OK, button(ok, message(F, ok))), point(0, 60)),
	send(Sub, display, button(cancel, message(F, cancel)), point(0,120)),
	send(OK, default_button, @on),
	send(D, append, Sub),

	new(DoOK, message(OK, execute)),
	send(D, append, new(L, list_browser), right),
	send(L, style, directory, style(icon := 'dir.bm')),
	send(L, style, file,      style(icon := 'file.bm')),
	send(L, select_message, message(F, select, @arg1?key, @arg1?style)),
	send(L, open_message, DoOK),
	send(L, recogniser,
	     handler(area_enter, message(D, keyboard_focus, L))),
	send(FI, message, DoOK),
	send(D, layout),
	send(L, right_side, DI?right_side),
	send(L, bottom_side, Sub?bottom_side).


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
	     concat('.', _, Ext), !,
	     concat(\, Ext, Re).
extension_regex(Ext, Re) :-
	     concat('\.', Ext, Re).


select(F, Name:string, Type:{directory,file}) :->
	"Handle selection from browser"::
	get(F?dialog, file_member, FI),
	(   Type == directory
	->  get(F?dialog, directory_member, DI),
	    send(F, directory, string('%s/%s', DI?selection, Name))
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
	get(F, dialog, P),
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
	get(F, dialog, P),
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
	;   new(R, regex('')),
	    send(R, pattern, string('.*%s', ?(R, quote, Extension))),
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
	
	send(P?file_member, clear).


file(F, Exists:exists=[bool], Ext:extension=[name],
     Dir:directory=[directory], Default:default=[file], File:name) :<-
 	"Get [existing] file with [extension]"::
	send(F, report, status, ''),
	(   Exists \== @default -> send(F, exists, Exists) ; true ),
	default(Ext, '', Extension),
	ignore(send(F, directory, Dir, Extension)),
	(   Default \== @default
	->  send(F?dialog?file_member, selection, Default?base_name)
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

/*
test :-
	get(@finder, file, File),
	format('File = ~w~n', File).
*/
