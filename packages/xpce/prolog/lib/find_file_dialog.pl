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


:- module(find_file_dialog, []).
:- use_module(library(pce)).
:- use_module(library(file_item)).

:- multifile
	pce_finder:file_type/2.

:- pce_begin_class(find_file_dialog, dialog,
		   "Browse for a file").

resource(up, image, image('16x16/up.xpm')).
resource(newdir, image, image('16x16/newdir.xpm')).

variable(directory,    directory*,  get,  "Current directory").
variable(message,      [code]*,     both, "Message executed on ok").
variable(mode,	       {save,open}, get,  "Mode of operation").
variable(default_file, name*,       both, "Default file to open/save").
variable(confirm_overwrite, bool := @on, both, "Confirm on overwrite").

:- pce_global(@finder_directory_popup, make_directory_popup).
:- pce_global(@finder_file_popup, make_file_popup).

make_directory_popup(P) :-
	new(Finder, @event?window),
	new(P, popup(directory)),
	send(P, update_message, message(@event?receiver, selection, @arg1)),
	send_list(P, append,
		  [ menu_item(rename, message(Finder, rename_dir, @arg1?key)),
		    menu_item(delete, message(Finder, delete_dir, @arg1?key))
		  ]).

make_file_popup(P) :-
	new(Finder, @event?window),
	new(P, popup(file)),
	send(P, update_message, message(@event?receiver, selection, @arg1)),
	send_list(P, append,
		  [ menu_item(rename, message(Finder, rename_file, @arg1?key)),
		    menu_item(delete, message(Finder, delete_file, @arg1?key))
		  ]).


initialise(D, Mode:mode=[{open,save}], Label:[char_array]) :->
	default(Mode, open, TheMode),
	send_super(D, initialise, find_file?label_name),
	send(D, slot, message, @default),
	send(D, append, new(LookIn, finder_look_in_item(look_in))),
	send(LookIn, message, message(D, directory, @arg1)),
	send(D, append, new(Up, button(up)), right),
	send(D, append, new(New, button(newdir)), right),
	get(LookIn?value_font, height, FH),
	send_list([Up,New], reference, point(0, FH)),
	send(D, append, new(Dirs, list_browser(width := 30))),
	send(D, append, new(Files, list_browser(width := 30)), right),
	send_list([Dirs,Files],
		  [ attribute(hor_stretch, 100),
		    attribute(ver_stretch, 100)
		  ]),
	send(Dirs, name, subdirs),
	send(Files, name, files),
	send(Dirs, select_message, message(D, subdir, @arg1?key)),
	send(Dirs, popup, @finder_directory_popup),
	send(Files, select_message, message(D, selected_file, @arg1?key)),
	send(Files, open_message, message(D, open_file, @arg1?key)),
	send(Files, popup, @finder_file_popup),
	send(D, append, new(File, file_item(file))),
	send(D, append, button(ok), right),
	send(D, append, finder_filter_item(filter)),
	send(D, append, button(cancel), right),
	send(Up, label, image(resource(up))),
	send(New, label, image(resource(newdir))),
	send(D, default_button, ok),
	send(D, resize_message, message(D, layout, @arg2)),
	send(D, mode, TheMode, Label),
	send(D, keyboard_focus, File).


layout(D, Size:[size]) :->
	"Refine layout"::
	send_super(D, layout, Size),
	get(D, member, subdirs, SubDirs),
	get(D, member, files, Files),
	get(D, width, DW),
	get(D?border, width, BW),
	get(D?gap, width, GW), 
	BrW is (DW-2*BW-GW)//2,
	send(SubDirs, do_set, BW, @default, BrW),
	send(Files, do_set, BW+BrW+GW, @default, BrW),
	Right is DW-BW,
	right_to_left([ok, file], D, GW, Right),
	right_to_left([cancel, filter], D, GW, Right).

right_to_left([],  _, _, _).
right_to_left([T], D, _, Right) :-
	get(D, member, T, Item),
	send(Item, right_side, Right).
right_to_left([H|T], D, GW, Right) :-
	get(D, member, H, Item),
	get(Item, width, W),
	IX is Right - W,
	send(Item, x, IX),
	RX is IX - GW,
	right_to_left(T, D, GW, RX).
       

make_transient(D) :->
	"Make transient to current object"::
	get(D, frame, F),
	(   get(@event, '_value', @nil)		% no current event
	->  send(F, application, @nil)
	;   get(@event?window, frame, MainFrame),
	    (	get(MainFrame, application, App),
		App \== @nil
	    ->  send(F, application, App),
	        send(F, modal, application)
	    ;	send(F, modal, transient)
	    ),
	    send(F, transient_for, MainFrame),
	    send(F, create),
	    send(F, center, MainFrame?area?center)
	).


mode(D, Mode:{open,save}, Label:[char_array]) :->
	"Mode of operation"::
	send(D, slot, mode, Mode),
	get(D, member, ok, Ok),
	send(Ok, label, Mode?label_name),
	(   Label == @default
	->  send(D, label, Mode?label_name)
	;   send(D, label, Label)
	).


directory(D, Dir:directory) :->
	"Change directory"::
	(   get(D, directory, Old),
	    Old \== @nil,
	    send(Old, same, Dir)
	->  send(D, slot, directory, Dir),
	    send(D, fill_look_in)	% may be different
	;   send(D, slot, directory, Dir),
	    send(D, fill_look_in),
	    send(D, fill_browsers),
	    get(D, member, file, FileItem),
	    send(FileItem, directory, Dir),
	    (	get(D, default_file, Def),
		Def \== @nil
	    ->	send(FileItem, selection, Def)
	    ;	send(FileItem, clear)
	    )
	).


up(D) :->
	"Go one directory up"::
	get(D, directory, Dir),
	(   get(Dir, parent, Parent)
	->  send(D, directory, Parent)
	;   send(D, report, warning, 'No parent')
	).


subdir(D, Name:name) :->
	"Goto selected subdiectory"::
	get(D, directory, Dir),
	get(Dir, directory, Name, Sub),
	send(D, directory, Sub).


selected_file(D, Name:name) :->
	"User selected a file"::
	get(D, member, file, FileItem),
	send(FileItem, selection, Name),
	get(D, member, files, FileBrowser),
	ignore(catch(send(FileBrowser, selection, Name), _, true)).


open_file(D, Name:name) :->
	"Select file an execute ok"::
	send(D, selected_file, Name),
	get(D, member, ok, OK),
	send(OK, execute).


fill_look_in(D) :->
	get(D, directory, Dir),
	get(D, member, look_in, Item),
	send(Item, selection, Dir?path).


favourites(D, Favourites:chain) :<-
	"List of favourite places"::
	get(D, directory, Dir),
	(   get(Dir, parent, Parent)
	->  true
	;   Parent = Dir		% the root
	),
	new(Favourites, chain),
	fill_parents(Parent, Favourites),
	(   catch(expand_file_name(~, [Home]), _, fail),
	    \+ send(Favourites, member, Home)
	->  send(Favourites, append, Home)
	;   true
	).


fill_parents(Dir, Chain) :-
	get(Dir, path, Path),
	send(Chain, append, Path),
	(   get(Dir, parent, Parent)
	->  fill_parents(Parent, Chain)
	;   true
	).


filter(D, Filter:[regex]) :<-
	"Obtain filter as a regular expression"::
	(   get(D, member, filter, Item),
	    get(Item, selection, Filter)
	->  true
	;   Filter = @default
	).


filter(D, Ext:'[name|tuple|chain]') :->
	get(D, member, filter, FilterItem),
	send(FilterItem, filter, Ext).


fill_browsers(D) :->
	get(D, member, subdirs, SubDirs),
	get(D, member, files, Files),
	send(SubDirs, clear),
	send(Files, clear),
	get(D, directory, Dir),
	get(D, filter, Filter),
	new(FoundDirs, chain),
	new(FoundFiles, chain),
	send(Dir, scan, FoundFiles, FoundDirs, Filter),
	send(FoundFiles, sort),
	send(FoundDirs, sort),
	send(SubDirs, members, FoundDirs),
	send(Files, members, FoundFiles).


selection(D, FileName:name) :<-
	"Return selected filename"::
	get(D, member, file, FileItem),
	get(FileItem, selection, RawName),
	get(D, ensure_extension, RawName, FileName).


ensure_extension(D, Raw:name, WithExt:name) :<-
	"Ensure to apply the extension"::
	get(D, member, filter, Item),
	get(Item, selection, Regex),
	(   send(Regex, match, Raw)
	->  WithExt = Raw
	;   get(Item, default_extension, Ext)
	->  file_name_extension(Raw, Ext, WithExt)
	;   WithExt = Raw
	).

:- pce_group(validate).

check_file(D, FileName:name) :->
	"Perform sanity-check of the resulted file"::
	(   get(D, mode, open)
	->  send(D, check_open_file, FileName)
	;   send(D, check_save_file, FileName)
	).

check_open_file(D, FileName:name) :->
	"Check for in Open mode"::
	new(File, file(FileName)),
	(   send(File, access, read)
	->  true
	;   (   send(File, exists)
	    ->	send(D, report, error,
		     'You have no read-access on %s', FileName)
	    ;	send(File, exists, @off)
	    ->	send(D, report, error,
		     '%s is not a regular file', FileName)
	    ;	send(D, report, error,
		     '%s does not exist', FileName)
	    ),
	    fail
	).

check_save_file(D, FileName:name) :->
	"Check for in Save mode"::
	new(File, file(FileName)),
	(   access_file(FileName, write)
	->  (   get(D, confirm_overwrite, @on),
	        send(File, exists)
	    ->	send(D, ask_overwrite, FileName)
	    ;	true
	    )
	;   (   send(File, exists)
	    ->	send(D, report, error,
		     'You have no write-access on %s', FileName)
	    ;	send(File, exists, @off)
	    ->	send(D, report, error,
		     '%s is not a regular file', FileName)
	    ;	send(D, report, error,
		     'You cannot create a file here')
	    ),
	    fail
	).

ask_overwrite(D, FileName:name) :->
	"Ask user to overwrite file"::
	send(D?display, confirm, 'Overwrite file %s?', FileName).

:- pce_group(button).

ok(D) :->
	"User confirmed"::
	(   get(D, message, Msg),
	    send(Msg, instance_of, code)
	->  get(D, selection, FileName),
	    send(D, check_file, FileName),
	    send(Msg, forward_receiver, D, FileName)
	;   true
	).


cancel(D) :->
	"User pressed cancel"::
	send(D, destroy).

:- pce_group(edit).

newdir(D) :->
	"Create a new directory"::
	get(D, prompt_name, create_directory, DirName),
	get(D, directory, Dir),
	get(Dir, directory, DirName, SubDir),
	send(SubDir, make),
	send(D, directory, SubDir).

prompt_name(D, For:name, Name:name) :<-
	"Prompt for a (new) name"::
	new(D2, dialog(For?label_name)),
	send(D2, transient_for, D),
	send(D2, append, new(TI, text_item(name))),
	send(D2, append, button(For, message(D2, return, TI?selection?strip))),
	send(D2, append, button(cancel, message(D2, destroy))),
	send(D2, default_button, For),
	get(D2, confirm_centered, D?frame?area?center, Name),
	send(D2, destroy),
	Name \== ''.
	
delete_dir(D, Name:name) :->
	"Delete named directory"::
	send(D?display, confirm, 'Delete directory "%s"?', Name),
	get(D, directory, Dir),
	get(Dir, directory, Name, SubDir),
	send(SubDir, remove),
	send(D, fill_browsers).

rename_dir(D, Name:name) :->
	"Rename directory"::
	get(D, directory, Dir),
	get(Dir, file, Name, SubDir),
	get(D, prompt_name, rename, NewName),
	(   send(file(NewName), exists)
	->  send(D, ask_overwrite, NewName)
	;   true
	),
	send(SubDir, name, NewName),
	send(D, fill_browsers),
	get(D, member, subdirs, Browser),
	ignore(catch(send(Browser, selection, NewName),_,true)).

delete_file(D, Name:name) :->
	"Delete named file"::
	send(D?display, confirm, 'Delete file "%s"?', Name),
	get(D, directory, Dir),
	get(Dir, file, Name, File),
	send(File, remove),
	send(D, fill_browsers).

rename_file(D, Name:name) :->
	"Rename file"::
	get(D, directory, Dir),
	get(Dir, file, Name, File),
	get(D, prompt_name, rename, NewName),
	(   send(file(NewName), exists)
	->  send(D, ask_overwrite, NewName)
	;   true
	),
	send(File, name, NewName),
	send(D, fill_browsers),
	send(D, selected_file, NewName).

:- pce_end_class(find_file_dialog).


:- pce_begin_class(finder_look_in_item, directory_item,
		   "Directory item with favourites").

show_combo_box(DI, Show:bool) :->
	"User clicked right-box"::
	(   Show == @on
	->  get(DI, device, Finder),
	    get(Finder, favourites, Favourites),
	    send(DI, select_completion, Favourites, '', '', 0)
	;   send_super(DI, show_combo_box, Show)
	).

selected_completion(DI, Selected:char_array, _Apply:[bool]) :->
	"User selected from the browser"::
	send_super(DI, selected_completion, Selected),
	send(DI, apply, @on).

enter(DI) :->
	"Update current directory"::
	send(DI, apply, @on).

:- pce_end_class(finder_look_in_item).

:- pce_begin_class(finder_filter_item, text_item,
		   "Show file patterns").

initialise(FI, Name:name) :->
	send_super(FI, initialise, Name),
	send(FI, type, regex),
	send(FI, filter, @default).

selection(FI, Filter:regex) :<-
	(   get(FI, modified, @on)
	->  get(FI, value, Typed),
	    new(Filter, regex),
	    send(Filter, file_pattern, Typed),
	    send(FI, slot, selection, Filter)
	;   get(FI, slot, selection, Filter)
	).

default_extension(FI, Ext:name) :<-
	get(FI, selection, Regex),
	get(Regex, attribute, default_extension, Ext).


selected_completion(FI, Sel:name) :->
	"User selected an alternative"::
	get(FI?value_set, find,
	    @arg1?print_name == Sel,
	    Re),
	send(FI, selection, Re),
	get(FI, device, Finder),
	send(Finder, fill_browsers).

enter(FI) :->
	"Update with new filter"::
	get(FI, device, Finder),
	send(Finder, fill_browsers).


%	->filter(Filter)
%	
%	Defines file-selection filter.  The argument is either single
%	or a chain to specify alternatives.  Singles are either a plain
%	atom denoting the extension, or a tuple(Label, Ext), where Ext
%	is a single atom or a chain of atoms.
%	
%	For example:
%	
%	    ->filter: pl		% *.pl files
%	    ->filter: chain(pl,qlf)	% *.pl or *.qlf files
%	    ->filter: tuple(prolog, chain(pl,qlf))
%	    ->filter: chain(tuple(prolog, chain(pl, qlf)),
%	    		    tuple(c, chain(c,h)))

filter(FI, Ext:'[name|tuple|chain]') :->
	(   Ext == @default
	->  send(FI, filter, tuple(all_files, *))
	;   new(ValueSet, chain),
	    (   send(Ext, instance_of, chain)
	    ->  send(Ext, for_all,
		     message(ValueSet, append,
			     ?(@prolog, to_regex, @arg1)))
	    ;   to_regex(Ext, Regex),
		send(ValueSet, append, Regex)
	    ),
	    send(FI, value_set, ValueSet),
	    send(FI, selection, ValueSet?head)
	).


to_regex(Tuple, Regex) :-
	send(Tuple, instance_of, tuple), !,
	get(Tuple, first, LabelName),
	get(Tuple, second, Exts),
	new(Regex, regex),
	send(Regex, attribute, print_name, LabelName?label_name),
	alt_regex(Exts, Regex).
to_regex(Atom, Regex) :-
	new(Regex, regex),
	send(Regex, attribute, print_name, string('*.%s', Atom)),
	alt_regex(Atom, Regex).

alt_regex(*, Regex) :- !,
	send(Regex, pattern, '.*').
alt_regex(Atom, Regex) :-
	atom(Atom), !,
	ext_pattern(Atom, Plain, Pattern),
	send(Regex, pattern, Pattern),
	(   pce_finder:file_type(Plain, Name)
	->  send(Regex, attribute, print_name, Name)
	;   send(Regex, attribute, print_name,
		 string('%s files', Atom?label_name)?value)
	),
	send(Regex, attribute, default_extension, Atom).
alt_regex(Chain, Regex) :-
	chain_list(Chain, List),
	maplist(ext_pattern, List, Patterns),
	concat_atom(Patterns, '\\|', AltPattern),
	send(Regex, pattern, AltPattern),
	(   List = [Def|_]
	->  send(Regex, attribute, default_extension, Def)
	;   true
	).

ext_pattern(Ext, Pattern) :-
	ext_pattern(Ext, _, Pattern).

ext_pattern(Ext, Plain, Pattern) :-
	sub_atom(Ext, 0, _, _, '.'), !,
	sub_atom(Ext, 1, _, 0, Plain),
	concat_atom(['^.*\\', Ext, '$'], Pattern).
ext_pattern(Ext, Ext, Pattern) :-
	concat_atom(['^.*\\.', Ext, '$'], Pattern).

:- pce_end_class(finder_filter_item).

