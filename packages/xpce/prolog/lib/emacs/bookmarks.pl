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

:- module(emacs_bookmarks, []).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(pce_toc)).
:- use_module(library(pce_report)).
:- use_module(library(persistent_frame)).
:- require([ ignore/1
	   , file_directory_name/2
	   , term_to_atom/2
	   , catch/3
	   , default/3
	   , send_list/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides the first  definition   of  an advanced bookmarking
system   for   PceEmacs.   Bookmarks    are     kept    in    the   file
~/.xpce/emacs_bookmarks as Prolog data. Bookmarks   can be annotated and
are time-stampted.

The bookmark mechanism is available through the Browse menu of PceEmacs.

Some issues to consider:

	* Save `collapsed' status of nodes?
	* Selective expansion
	* Search
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@emacs_mark_list, new(emacs_bookmark_editor)).

resource(save, image, image('16x16/save.xpm')).
resource(cut,  image, image('16x16/cut.xpm')).
resource(open, image, image('16x16/book2.xpm')).

:- pce_begin_class(emacs_bookmark_editor, persistent_frame,
		   "PceEmacs bookmark administration and viewing").

variable(file,	       file, get, "File for holding the bookmarks").
variable(exit_message, code, get, "Registered exit message").

initialise(BM) :->
	send_super(BM, initialise, 'PceEmacs bookmarks'),
	send(BM, done_message, message(BM, status, hidden)),
	send(BM, append, new(D, dialog)),
	send(BM, fill_dialog),
	send(new(emacs_bookmark_window), below, D),
	send(new(V, view(size := size(40,8))), below, D),
	send(V, font, normal),
	send(V, ver_stretch, 0),
	send(@pce, exit_message, new(Msg, message(BM, save))),
	send(BM, slot, exit_message, Msg),
	ignore(send(BM, load)).

fill_dialog(BM) :->
	get(BM, member, dialog, D),
	send(D, pen, 0),
	send(D, gap, size(0, 5)),
	send(D, append, new(TB, tool_bar(BM))),
	send_list(TB, append,
		  [ tool_button(save, resource(save), 'Save bookmarks'),
		    gap,
		    tool_button(goto, resource(open), 'Open editor'),
		    tool_button(cut,  resource(cut),  'Delete selection')
		  ]),
	send(D, append, graphical(0,0,10,1), right), % make a gap
	send(D, append, new(reporter), right),
	send(D, resize_message, message(D, layout, @arg2)).

unlink(BM) :->
	get(BM, member, emacs_bookmark_window, W),
	send(W, select_node, @nil),	% save possible modified note
	send(BM, save),
	get(BM, exit_message, Msg),
	get(@pce, exit_messages, Chain),
	send(Chain, delete, Msg),
	send_super(BM, unlink).


tree(BM, Tree:toc_tree) :<-
	get(BM, member, emacs_bookmark_window, W),
	get(W, tree, Tree).

view(BM, V:view) :<-
	"View for annotations"::
	get(BM, member, view, V).

selection(BM, Sel:'name|emacs_bookmark') :<-
	get(BM, member, emacs_bookmark_window, W),
	get(W, selection, Sel0),
	get(Sel0, map, @arg1?identifier, Sel1),
	get(Sel1, head, Sel).

:- pce_group(edit).

goto(BM) :->
	"Edit current selection"::
	get(BM, member, emacs_bookmark_window, W),
	(   get(BM, selection, Sel),
	    send(Sel, instance_of, emacs_bookmark)
	->  send(W, open_node, Sel)
	;   send(BM, report, warning, 'No selection')
	).

cut(BM) :->
	"Delete selected nodes"::
	get(BM, member, emacs_bookmark_window, W),
	(   get(W, selection, Nodes),
	    \+ send(Nodes, empty)
	->  send(Nodes, for_all, message(@arg1, delete_tree))
	;   send(BM, report, warning, 'No selection')
	).

:- pce_group(interface).

bookmark(F, BM:emacs_bookmark, Sort:[bool]) :->
	"Append a bookmark"::
	get(F, tree, Tree),
	send(Tree?root, append, BM, Sort).

append_hit(F, Buffer:emacs_buffer, SOL:int) :->
	"Add bookmark for indicated line"::
	get(Buffer, scan, SOL, line, 0, end, EOL),
	get(Buffer, contents, SOL, EOL-SOL, Title),
	get(Buffer, line_number, SOL, Line),
	(   get(Buffer, file, File),
	    File \== @nil,
	    get(File, absolute_path, FileName)
	->  true
	;   send(Buffer, report, warning, 'No associated file'),
	    fail
	),
	send(F, bookmark, new(BM, emacs_bookmark(FileName, Line, Title))),
	send(BM, link, Buffer),
	send(F, open).

loaded_buffer(F, TB:emacs_buffer) :->
	"PceEmacs has loaded a file"::
	get(F, tree, Tree),
	send(Tree?root, loaded_buffer, TB).

:- pce_group(file).

save(BM) :->
	"Save bookmarks to file"::
	get(BM, bookmarks_file, write, File),
	send(file(File), backup),
	open(File, write, Fd),
	format(Fd, '/* PceEmacs Bookmarks */~n~n', []),
	get(BM, geometry, Geometry),
	format(Fd, 'geometry(~q).~n~n', [Geometry]),
	get(BM, tree, Tree),
	send(Tree?root, save, Fd),
	close(Fd),
	send(BM, report, status, 'Saved bookmarks to %s', File).

load(BM) :->
	"Load bookmarks from file"::
	get(BM, bookmarks_file, File),
	catch(open(File, read, Fd), _, fail),
	read(Fd, Term0),
	load_bookmarks(Term0, Fd, BM),
	close(Fd).


load_bookmarks(end_of_file, _, _) :- !.
load_bookmarks(Term, Fd, BM) :- !,
	load_bookmark(Term, BM),
	read(Fd, Term2),
	load_bookmarks(Term2, Fd, BM).

load_bookmark(bookmark(File, Line, Title, Stamp, Note), BM) :- !,
	new(Created, date),
	send(Created, posix_value, Stamp),
	send(BM, bookmark,
	     new(M, emacs_bookmark(File, Line, Title, Created, Note)),
	     @off),			% do not sort
	(   get(@emacs, file_buffer, File, Buffer)
	->  send(M, link, Buffer)
	;   true
	).
load_bookmark(geometry(GM), BM) :- !,
	send(BM, geometry, GM).
load_bookmark(Term, BM) :-
	term_to_atom(Term, Atom),
	send(BM, report, warning, 'Unknown term in bookmarks file: %s', Atom).

bookmarks_file(BM, Access:[{read,write}], File:name) :<-
	(   get(BM, file, F),
	    F \== @nil,
	    send(F, access, Access)
	->  get(F, absolute_path, File)
	;   new(F, file('~/.xpce/emacs_bookmarks')),
	    (   Access == write
	    ->  get(F, directory_name, DirName),
		new(D, directory(DirName)),
		(   send(D, exists)
		->  true
		;   send(D, make)
		)
	    ;   true
	    ),
	    get(F, absolute_path, File),
	    send(BM, slot, file, File)		% use the absolute path
	).
	    
:- pce_end_class.

:- pce_begin_class(emacs_bookmark_window, toc_window).

initialise(BW) :->
	send_super(BW, initialise),
	send(BW, root, emacs_toc_bookmark_folder(/)).

open_node(BW, Id:any) :->
	"Open bookmark on double-click"::
	(   send(Id, instance_of, emacs_bookmark)
	->  (   send(Id, exists)
	    ->	ignore(send(Id, update)),
	        send(@emacs, goto_source_location, Id)
	    ;	(   get(Id, file_name, File),
		    send(@display, confirm,
			 'Marked file "%s" does not exist.\nDelete bookmark?',
			 File)
		->  get(BW, node, Id, Node),
		    send(Node, delete_tree)
		;   true
		)
	    )
	).

select_node(BW, Id:any) :->
	get(BW?frame, view, View),
	(   get(View, modified, @on),
	    get(View, attribute, bookmark, BM0),
	    BM0 \== @nil
	->  send(BM0, note, View?contents)
	;   true
	),
	send(View, attribute, bookmark, @nil),
	send(View, clear),
	(   send(Id, instance_of, emacs_bookmark)
	->  (   get(Id, note, String),
	        String \== @nil
	    ->  send(View, contents, String)
	    ;	true
	    ),
	    send(View, attribute, bookmark, Id)
	;   true
	).

:- pce_end_class.

:- pce_begin_class(emacs_toc_bookmark_folder, toc_folder,
		   "Represent directory in bookmarks").

initialise(F, Path:name) :->
	(   Path == /
	->  (	get(@pce, operating_system, win32)
	    ->	RootName = 'My Computer'
	    ;	RootName = '/'
	    ),
	    send_super(F, initialise, RootName, @nil)
	;   get(file(Path), base_name, BaseName),
	    send_super(F, initialise, BaseName, Path)
	).

collapsed(F, Val:bool*) :->
	"Disable toc_window expansion mechanism"::
	send_class(F, node, collapsed(Val)).

append(F, BM:emacs_bookmark, Sort:[bool]) :->
	get(BM, file_name, FileName),
	get(F, identifier, Path),
	(   Path == @nil		% this is the root
	->  true
	;   send(FileName, prefix, Path)
	),
	get(F, sons, Sons),
	(   get(Sons, find, message(@arg1, append, BM), _)
	->  true
	;   sub_directory(Path, FileName, SubPath),
	    SubPath \== FileName,
	    send(F, collapsed, @off),
	    send(F, son, new(S, emacs_toc_bookmark_folder(SubPath))),
	    send(S, append, BM),
	    (	Sort \== @off
	    ->	send(F, sort)
	    ;	true
	    )
	;   send_class(F, node, collapsed(@off)),
	    send(F, son, new(emacs_toc_bookmark(BM))),
	    (	Sort \== @off
	    ->	send(F, sort)
	    ;	true
	    )
	).
	
sort(F) :->
	"Sort the nodes"::
	send(F, sort_sons, ?(@arg1, compare, @arg2)).

compare(F, N:toc_node, Diff:{smaller,equal,larger}) :<-
	"Put folders before files, otherwise alphabetical"::
	(   send(N, instance_of, toc_folder)
	->  get(F, member, text, T0),
	    get(N, member, text, T1),
	    get(T0?string, compare, T1?string, Diff)
	;   Diff = smaller
	).
		
sub_directory(@nil, File, SubPath) :- !,
	(   get(@pce, operating_system, win32)
	->  new(Re, regex('[a-zA-Z]:'))
	;   new(Re, regex('/[^/]*'))
	),
	send(Re, match, File),
	get(Re, register_value, File, 0, name, SubPath).
sub_directory(Path, File, SubPath) :-
	send(File, prefix, Path),
	file_directory_name(File, FileDir),
	(   FileDir == Path
	->  SubPath = File
	;   sub_directory(Path, FileDir, SubPath)
	).


save(F, Fd:prolog) :->
	"Save bookmarks to file"::
	get_chain(F, sons, Sons),
	save_sons(Sons, Fd).

save_sons([], _).
save_sons([H|T], Fd) :-
	send(H, save, Fd),
	save_sons(T, Fd).

loaded_buffer(F, TB:emacs_buffer) :->
	"PceEmacs has loaded this buffer"::
	(   get(TB, file, File),
	    File \== @nil,
	    get(File, absolute_path, Path),
	    get(F, identifier, Id),
	    (	Id == @nil
	    ->	true
	    ;   send(Path, prefix, Id)
	    )
	->  send(F?sons, for_all,
		 message(@arg1, loaded_buffer, TB))
	;   true
	).

:- pce_end_class.

:- pce_begin_class(emacs_toc_bookmark, toc_file,
		   "Represent a bookmark").

initialise(F, BM:emacs_bookmark) :->
	get(BM, title, Title),
	get(BM, file_name, Path),
	get(file(Path), base_name, File),
	get(BM, line_no, Line),
	send_super(F, initialise,
		   string('%s:%d %s', File, Line, Title), BM).

append(_F, _BM:emacs_bookmark) :->
	"Can't append to a file"::
	fail.

save(F, Fd:prolog) :->
	"Save bookmarks to file"::
	get(F, identifier, BM),
	get(BM, term, bookmark(File, Line, Title, Stamp, NoteText)),
	format(Fd, 'bookmark(~q, ~q, ~q, ~0f, ~q).~n',
	       [File, Line, Title, Stamp, NoteText]).

loaded_buffer(F, TB:emacs_buffer) :->
	"PceEmacs has loaded this buffer"::
	get(F, identifier, BM),
	get(BM, file_name, FileName),
	(   get(TB, file, File),
	    File \== @nil,
	    get(File, absolute_path, Path),
	    Path == FileName
	->  send(BM, link, TB)
	;   true
	).

compare(F, N2:toc_node, Diff:{smaller,equal,larger}) :<-
	(   send(N2, instance_of, emacs_toc_bookmark_folder)
	->  Diff = larger
	;   get(F, identifier, BM0),
	    get(N2, identifier, BM1),
	    (	get(BM0, file_name, F1),
		get(BM1, file_name, F2),
		get(file(F1), base_name, B1),
		get(file(F2), base_name, B2),
		get(B1, compare, B2, Diff),
		Diff \== equal
	    ->  true
	    ;   get(BM0, line_no, L0),
		get(BM1, line_no, L1),
		get(number(L0), compare, L1, Diff)
	    )
	).

:- pce_end_class.

:- pce_begin_class(emacs_bookmark, source_location,
		   "Bookmark in PceEmacs").

variable(title,		string,		get,  "Represented title").
variable(created,	date,		get,  "Date of creation").
variable(note,		string*,	both, "Annotation").

initialise(BM, File:name, Line:int, Title:string,
	   Created:[date], Note:[string]*) :->
	send_super(BM, initialise, File, Line),
	send(BM, slot, title, Title),
	(   Created == @default
	->  send(BM, slot, created, new(date))
	;   send(BM, slot, created, Created)
	),
	default(Note, @nil, TheNote),
	send(BM, slot, note, TheNote).

term(BM, Term:prolog) :<-
	"Describe bookmark as a Prolog term"::
	ignore(send(BM, update)),
	get(BM, file_name, File),
	get(BM, line_no, Line),
	get(BM?title, value, Title),
	get(BM?created, posix_value, Stamp),
	(   get(BM, note, Note),
	    Note \== @nil
	->  get(Note, value, NoteText)
	;   NoteText = ''
	),
	Term = bookmark(File, Line, Title, Stamp, NoteText).

exists(BM) :->
	"Test whether associated file exists"::
	get(BM, file_name, File),
	send(file(File), exists).

:- pce_group(buffer).

link(BM, To:text_buffer) :->
	"Link the bookmark using a fragment"::
	get(BM, line_no, Line),
	get(To,	scan, 0,   line, Line-1, start, SOL),
	get(To,	scan, SOL, line, 0,      end,   EOL),
	new(_, emacs_bookmark_hyper(BM, fragment(To, SOL, EOL-SOL, bookmark))).
%	format('Linked ~p to ~p~n', [BM, To]).

update(BM) :->
	"If bookmark is linked, update <-line_no"::
	get(BM, hypered, fragment, Fragment),
	(   get(Fragment, text_buffer, TB),
	    TB \== @nil
	->  get(Fragment, start, Start),
	    get(TB, line_number, Start, Line),
	    send(BM, line_no, Line)
	;   true			% destroy?
	).

:- pce_end_class.


:- pce_begin_class(emacs_bookmark_hyper, hyper,
		   "Hyper from bookmark to fragment").

initialise(H, BM:emacs_bookmark, To:fragment) :->
	send_super(H, initialise, BM, To, fragment, bookmark).

unlink_from(H) :->
	"Bookmark is deleted"::
	get(H, to, Fragment),
	free(Fragment),
	free(H).

unlink_to(H) :->
	"Fragment is deleted, update line"::
	get(H, from, BM),
	send(BM, update),
	free(H).

:- pce_end_class.
