/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
variable(extensions,	chain*,		get,
	 "Extension of the requested file").
variable(directory,	directory,	get,
	 "Current directory").


		/********************************
		*           CREATION		*
		********************************/

initialise(F) :->
	send(F, send_super, initialise, finder, transient),
	send(F, slot, exists, @off),
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

	send(ButtonDialog, name, button_dialog),
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
	send(L, open_message, message(F, open_entry, @arg1?key, @arg1?style)),
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


select(F, Name:string, _Type:{directory,file,drive}) :->
	"Handle selection from browser"::
	get(F, file_item, FI),
	send(FI, selection, Name).

open_entry(F, Name:string, Type:{directory,file,drive}) :->
	"Hangle double-click from browser"::
	(   Type == directory
	->  get(F, directory_item, DI),
	    send(F, directory, string('%s/%s', DI?selection, Name))
	;   Type == drive
	->  send(F, directory, Name)
	;   get(F, member, button_dialog, BD),
	    get(BD, member, ok, Ok),
	    send(Ok, execute)
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
	clean_name(DirName, /, CleanDir),
	clean_name(FileName, '', CleanFile0),
	get(F, extension_regex, Re),
	(   Re == @nil
	->  CleanFile = CleanFile0
	;   (   send(Re, match, CleanFile0)
	    ->  CleanFile = CleanFile0
	    ;	get(F, extensions, Exts),
		get(Exts, head, Ext),
		ensure_dot(Ext, TheExt),
		clean_name(FileName, TheExt, CleanFile)
	    )
	),
	send(F, slot, directory, DirName),
	new(File, file(string('%s%s', CleanDir, CleanFile))).

ensure_dot(Ext, Ext) :-
	sub_atom(Ext, 0, _, _, '.'), !.
ensure_dot(Ext, DotExt) :-
	atom_concat('.', Ext, DotExt).

clean_name(Name, Ext, Clean) :-
	new(S, string('%s', Name)),
	send(S, strip),
        (   send(class(file), has_feature, case_sensitive, @off)
	->  send(S, ensure_suffix, Ext, @on)
	;   send(S, ensure_suffix, Ext)
	),
	get(S, value, Clean),
	free(S).


ok(F) :->
	"User pressed ok"::
	send(F, report, status, ''),
	(   get(F, selection, File)
	->  get(File, name, Name),
	    (   get(F, exists, @on)
	    ->  (   send(File, exists)
		->  send(F, return, Name)
		;   send(F, report, error, '%s: No such file', File?name)
		)
	    ;   send(F, return, Name)
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

extensions(F, Ext:'name|chain*') :->
	"Set <-extensions to chain of dot-less names"::
	(   Ext == @nil
	->  send(F, slot, extensions, Ext)
	;   send(F, slot, extensions, new(E, chain)),
	    (   atom(Ext)
	    ->  remove_dot(Ext, E2),
		send(E, append, E2)
	    ;   chain_list(Ext, List),
		maplist(remove_dot, List, Plain),
		send_list(E, append, Plain)
	    )
	).
	
remove_dot('', '') :- !.
remove_dot(Ext0, Ext) :-
	atom_concat('.', Ext, Ext0), !.
remove_dot(Ext, Ext).

extension_regex(F, Re:regex*) :<-
	"Return regex from <-extensions"::
	(   get(F, extensions, Exts),
	    Exts \== @nil,
	    \+ send(Exts, member, '')
	->  new(Re, regex('')),
	    new(P, string('.*\\\\.\\\\(')),
	    chain_list(Exts, List),
	    maplist(re_quote, List, QList),
	    concat_atom(QList, '\\|', P0),
	    send(P, append, P0),
	    send(P, append, '\\)$'),
	    send(Re, pattern, P),
	    (   send(class(file), has_feature, case_sensitive, @off)
	    ->  send(Re, ignore_case, @on)
	    ;   true
	    )
	;   Re = @nil			% pattern should accept anything
	).

:- pce_global(@finder_quote_regex, new(regex(''))).

re_quote(Ext, Q) :-
	get(@finder_quote_regex, quote, Ext, QS),
	get(QS, value, Q),
	free(QS).


directory(F, Dir:[directory], Ext:[name|chain]*) :->
	"Set current directory and fill browser"::
	(   Dir \== @default
	->  send(F, slot, directory, Dir)
	;   true
	),
	(   Ext \== @default
	->  send(F, extensions, Ext)
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

	get(F, extension_regex, Re),
	(   Re == @nil
	->  Files = AllFiles
	;   get(AllFiles, find_all, message(Re, match, @arg1), Files),
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


file(F, Exists:exists=[bool], Ext0:extension=[name|chain],
     Dir:directory=[directory], Default:default=[file], File:name) :<-
 	"Get [existing] file with [extension]"::
	get_file(F, Exists, Ext0, Dir, Default, File).

get_file(F, Exists, Ext, Dir, Default, File) :-
	send(@display, has_get_method, win_file_name), !,
	(   Exists == @on
	->  Mode = open
	;   Mode = save
	),
	win_filter(Ext, Filters),
	(   Dir == @default
	->  get(F, directory, DefDir)
	;   DefDir = Dir
	),
	(   Default == @default
	->  DefFile = Default
	;   get(Default, name, DefFile)
	),
	get(@display, win_file_name, Mode, Filters,
	    @default, DefFile, DefDir, File),
	file_directory_name(File, NewDir),
	send(F, slot, directory, NewDir).
get_file(F, Exists, Ext0, Dir, Default, File) :-
	send(F, report, status, ''),
	(   Exists \== @default
	->  send(F, exists, Exists)
	;   true
	),
	default(Ext0, @nil, Ext),
	ignore(send(F, directory, Dir, Ext)),
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

win_filter(@default, @default) :- !.
win_filter(Atom, Filter) :-
	atom(Atom), !,
	new(C, chain(Atom)),
	win_filter(C, Filter).
win_filter(Chain, Filter) :-
	get(Chain, map, ?(@prolog, file_filter, @arg1), Filter).

file_filter(Ext0, Filter) :-
	atom_concat('.', Ext, Ext0), !,
	file_filter(Ext, Filter).
file_filter(Ext, tuple(Name, Pattern)) :-
	file_type(Ext, Name), !,
	atom_concat('*.', Ext, Pattern).
file_filter(Ext, Pattern) :-
	atom_concat('*.', Ext, Pattern).

%	Allow the user to add rules to this predicate, showing proper
%	names to the user rather than patterns.  The collection here
%	is rather arbitrary ...  Maybe we should read the registery
%	for defined filetypes ...

:- multifile
	file_type/2.

file_type(pl,	'Prolog files').
file_type(pd,	'PceDraw files').
file_type(ps,	'PostScript files').
file_type(eps,	'Encapsulated PostScript files').
file_type(txt,	'Text files').
file_type(jpeg,	'JPEG images').
file_type(gif,	'GIF images').
file_type(xpm,	'XPM images').
file_type(ico,	'Icon files').
file_type(cur,	'Cursor files').
file_type(html,	'HTML documents').
file_type(htm,	'HTML documents').
file_type(xml,	'XML documents').
file_type(sgml,	'SGML documents').

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
