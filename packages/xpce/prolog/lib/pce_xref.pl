:- module(pce_xref_gui,
	  [ gxref/0,
	    xref_file_imports/2,	% +File, -Imports
	    xref_file_exports/2		% +File, -Exports
	  ]).
:- use_module(pce).
:- use_module(persistent_frame).
:- use_module(tabbed_window).
:- use_module(toolbar).
:- use_module(pce_report).
:- use_module(pce_util).
:- use_module(pce_toc).
:- use_module(pce_arm).
:- use_module(pce_tagged_connection).
:- use_module(dragdrop).
:- use_module(pce_prolog_xref).
:- use_module(print_graphics).
:- use_module(tabular).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(autowin)).
:- use_module(library(broadcast)).

version('0.1.0').

:- dynamic
	setting/2.

setting_menu([ warn_autoload,
	       warn_not_called
	     ]).

setting(warn_autoload,      false).
setting(warn_not_called,    true).
setting(hide_system_files,  true).
setting(hide_profile_files, true).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE based font-end of the Prolog cross-referencer.  Tasks:

	* Cross-reference currently loaded program		OK
	* Generate module-dependency graph			OK
	* Information on
		- Syntax and other encountered errors
		- Export/Import relation between modules	OK
		- Undefined predicates				OK
		- Unused predicates				OK
	* Summary information
		- Syntax and other encountered errors
		- Exports never used (not for libs!)
		- Undefined predicates
		- Unused predicates
	* Export module import and export header
		- Using require/1
		- Using use_module/1
		- Using use_module/2				OK
		- Export header for non-module files		OK

----------------
NOTE: This is work in progress.  

Its in CVS as this makes it easier to maintain and as-is, the
tools is now in a useable state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

gxref :-
	send(new(XREF, xref_frame), open),
	send(XREF, wait),
	send(XREF, update).


:- pce_begin_class(xref_frame, persistent_frame,
		   "GUI for the Prolog cross-referencer").

initialise(F) :->
	send_super(F, initialise, 'Prolog XREF'),
	new(BrowserTabs, tabbed_window),
	send(BrowserTabs, left, new(WSTabs, tabbed_window)),
	send(BrowserTabs, name, browsers),
	send(BrowserTabs, hor_shrink, 10),
	send(BrowserTabs, hor_stretch, 10),
	send(WSTabs, name, workspaces),
	send_list([BrowserTabs, WSTabs], label_popup, F?tab_popup),
	send(new(TD, tool_dialog(F)), above, BrowserTabs),
	send(new(report_dialog), below, BrowserTabs),
	send(F, append, BrowserTabs),
	send_list(BrowserTabs,
		  [ append(new(xref_file_tree), files),
		    append(new(xref_predicate_browser), predicates)
		  ]),
	send_list(WSTabs,
		  [ append(new(xref_depgraph), dependencies)
		  ]),
	send(F, fill_toolbar, TD).
	
tab_popup(_F, P:popup) :<-
	"Popup for tab labels"::
	new(P, popup),
	send_list(P, append,
		  [ menu_item(close, message(@arg1, destroy)),
		    menu_item(detach, message(@arg1, untab))
		  ]).

fill_toolbar(F, TD:tool_dialog) :->
	send(TD, append, new(File, popup(file))),
	send(TD, append,
	     new(Settings, popup(settings,
				 message(F, setting, @arg1, @arg2)))),
	send(TD, append, new(View, popup(view))),
	send(TD, append, new(Help, popup(help))),
	send_list(File, append,
		  [ menu_item(exit, message(F, destroy))
		  ]),
	send_list(View, append,
		  [ menu_item(refresh, message(F, update))
		  ]),
	send_list(Help, append,
		  [ menu_item(about, message(F, about))
		  ]),
	send(Settings, show_current, @on),
	send(Settings, multiple_selection, @on),
	send(F, update_setting_menu).
	     
about(_F) :->
	version(Version),
	send(@display, inform,
	     string('SWI-Prolog cross-referencer version %s\n\
		    By Jan Wielemaker', Version)).

:- pce_group(parts).

workspace(F, Which:name, Create:[bool], Expose:bool, WS:window) :<-
	"Find named workspace"::
	get(F, member, workspaces, Tabs),
	(   get(Tabs, member, Which, WS)
	->  true
	;   Create == @on
	->  workspace_term(Which, New),
	    new(WS, New),
	    send(WS, name, Which),
	    send(Tabs, append, WS)
	),
	(   Expose == @on
	->  send(Tabs, on_top, WS?name)
	;   true
	).

workspace_term(file_info, prolog_file_info).
workspace_term(header,    xref_view).
	    
browser(F, Which:name, Browser:browser) :<-
	"Find named browser"::
	get(F, member, browsers, Tabs),
	get(Tabs, member, Which, Browser).

update(F) :->
	"Update all windows"::
	send(F, xref_all),
	get(F, member, browsers, Tabs),
	send(Tabs?members, for_some,
	     message(@arg1, update)),
	get(F, member, workspaces, WSs),
	send(WSs?members, for_some,
	     message(@arg1, update)).

xref_all(F) :->
	"Run X-referencer on all files"::
	forall(source_file(File),
	       send(F, xref_file, File)).

xref_file(F, File:name) :->
	"XREF a single file if not already done"::
	(   xref_done(File, Time),
	    catch(time_file(File, Modified), _, fail),
	    Modified == Time
	->  true
	;   send(F, report, progress, 'XREF %s', File),
	    xref_source(File),
	    send(F, report, done)
	).

:- pce_group(actions).


file_info(F, File:name) :->
	"Show summary info on File"::
	get(F, workspace, file_info, @on, @on, Window),
	send(Window, file, File),
	broadcast(xref_refresh_file(File)).

file_header(F, File:name) :->
	"Create import/export header"::
	get(F, workspace, header, @on, @on, View),
	send(View, file_header, File).
		     
:- pce_group(settings).

update_setting_menu(F) :->
	"Update the menu for the settings with the current values"::
	get(F, member, tool_dialog, TD),
	get(TD, member, menu_bar, MB),
	get(MB, member, settings, Popup),
	send(Popup, clear),
	setting_menu(Entries),
	(   member(Name, Entries),
	    setting(Name, Value),
	    send(Popup, append, new(MI, menu_item(Name))),
	    (	Value == true
	    ->	send(MI, selected, @on)
	    ;	true
	    ),
	    fail ; true
	).

setting(F, S:name, PceVal:bool) :->
	"Update setting and redo analysis"::
	pce_to_prolog_bool(PceVal, Val),
	retractall(setting(S, _)),
	assert(setting(S, Val)),
	send(F, update).

pce_to_prolog_bool(@on, true).
pce_to_prolog_bool(@off, false).

:- pce_end_class(xref_frame).


		 /*******************************
		 *	      WORKSPACE		*
		 *******************************/

:- pce_begin_class(xref_depgraph, picture,
		   "Workspace showing dependecies").
:- use_class_template(arm).
:- use_class_template(print_graphics).

initialise(W) :->
	send_super(W, initialise),
	send(W, popup, new(P, popup)),
	send_list(P, append,
		  [ menu_item(layout, message(W, layout)),
		    gap,
		    menu_item(view_whole_project, message(W, show_project)),
		    gap,
		    menu_item(clear, message(W, clear, destroy)),
		    gap,
		    menu_item(print, message(W, print))
		  ]).

update(P) :->
	"Initial screen"::
	send(P, display,
	     new(T, text('Drag files or directories to dependency view\n\
			  or use background menu to show the whole project')),
	     point(10,10)),
	send(T, name, intro_text),
	send(T, colour, grey50).

remove_intro_text(P) :->
	"Remove the introductionary text"::
	(   get(P, member, intro_text, Text)
	->  send(Text, destroy)
	;   true
	).

show_project(P) :->
	get(P, sources, Sources),
	send(P, clear, destroy),
	forall(member(Src, Sources),
	       send(P, append, Src)),
	send(P, update_links),
	send(P, layout).

sources(_, Sources:prolog) :<-
	findall(S, dep_source(S), Sources).

%	dep_source(?Src)
%	
%	Generate all sources for the dependecy graph one-by-one.

dep_source(Src) :-
	source_file(Src),
	(   setting(hide_system_files, true)
	->  \+ library_file(Src)
	;   true
	),
	(   setting(hide_profile_files, true)
	->  \+ profile_file(Src)
	;   true
	).

append(P, File:name, Create:[bool|{always}]) :->
	"Append File.  If Create == always also if a system file"::
	default(Create, @on, C),
	get(P, node, File, C, _).

node(G, File:name, Create:[bool|{always}], Pos:[point],
     Gr:xref_file_graph_node) :<-
	"Get the node representing File"::
	(   get(G, member, File, Gr)
	->  true
	;   (   Create == @on
	    ->  dep_source(File)
	    ;   Create == always
	    ),
	    (	Pos == @default
	    ->  get(G?visible, center, At)
	    ;   At = Pos
	    ),
	    send(G, display, new(Gr, xref_file_graph_node(File)), At),
	    send(G, remove_intro_text)
	).
	
update_links(G) :->
	"Add all export links"::
	send(G?graphicals, for_all,
	     if(message(@arg1, instance_of, xref_file_graph_node),
		message(@arg1, create_export_links))).

layout(G, MoveOnly:[chain]) :->
	"Do graph layout"::
	get(G?graphicals, find_all,
	    message(@arg1, instance_of, xref_file_graph_node), Nodes),
	get(Nodes, find_all, not(@arg1?connections), UnConnected),
	send(Nodes, subtract, UnConnected),
	new(Pos, point(10,10)),
	send(UnConnected, for_all,
	     and(message(@arg1, position, Pos),
		 message(Pos, offset, 0, 25))),
	get(Nodes, head, First),
	send(First, layout,
	     nominal := 100,
	     iterations := 1000,
	     network := Nodes,
	     move_only := MoveOnly).


:- pce_group(dragdrop).

drop(G, Obj:object, Pos:point) :->
	"Drop a file on the graph"::
	(   send(Obj, instance_of, xref_file_text)
	->  get(Obj, path, File),
	    (   get(G, node, File, Node)
	    ->  send(Node, flash)
	    ;   get(G, node, File, always, Pos, Node),
		send(G, update_links)
	    )
	;   send(Obj, instance_of, xref_directory_text)
	->  get(Obj, files, Files),
	    layout_new(G,
		       (   send(Files, for_all,
				message(G, append, @arg1, always)),
			   send(G, update_links)
		       ))
	).

preview_drop(G, Obj:object*, Pos:point) :->
	"Show preview of drop"::
	(   Obj == @nil
	->  send(G, report, status, '')
	;   send(Obj, instance_of, xref_file_text)
	->  (   get(Obj, device, G)
	    ->  send(Obj, move, Pos)
	    ;   get(Obj, path, File),
		get(Obj, string, Label),
		(   get(G, node, File, _Node)
		->  send(G, report, status, '%s: already in graph', Label)
		;   send(G, report, status, 'Add %s to graph', Label)
		)
	    )
	;   send(Obj, instance_of, xref_directory_text)
	->  get(Obj, path, Path),
	    send(G, report, status, 'Add files from directory %s', Path)
	).

:- pce_end_class(xref_depgraph).

:- pce_begin_class(xref_file_graph_node, xref_file_text).

:- send(@class, handle, handle(w/2, 0, link, north)).
:- send(@class, handle, handle(w, h/2, link, west)).
:- send(@class, handle, handle(w/2, h, link, south)).
:- send(@class, handle, handle(0, h/2, link, east)).

initialise(N, File:name) :->
	send_super(N, initialise, File),
	send(N, font, bold),
	send(N, background, grey80).

create_export_links(N, Add:[bool]) :->
	"Create the export links to other files"::
	get(N, path, Exporter),
	forall(export_link(Exporter, Importer, Callables),
	       create_export_link(N, Add, Importer, Callables)).

create_export_link(From, Add, Importer, Callables) :-
	(   get(From?device, node, Importer, Add, INode)
	->  send(From, link, INode, Callables)
	;   true
	).

create_import_links(N, Add:[bool]) :->
	"Create the import links from other files"::
	get(N, path, Importer),
	forall(export_link(Exporter, Importer, Callables),
	       create_import_link(N, Add, Exporter, Callables)).

create_import_link(From, Add, Importer, Callables) :-
	(   get(From?device, node, Importer, Add, INode)
	->  send(INode, link, From, Callables)
	;   true
	).

link(N, INode:xref_file_graph_node, Callables:prolog) :->
	"Create export link to INode"::
	(   get(N, connections, INode, CList),
	    get(CList, find, @arg1?from == N, C)
	->  send(C, callables, Callables)
	;   new(L, xref_export_connection(N, INode, Callables)),
	    send(L, hide)
	).

:- pce_global(@xref_file_graph_node_recogniser,
	      make_xref_file_graph_node_recogniser).

make_xref_file_graph_node_recogniser(G) :-
	new(G, move_gesture(left, '')).

event(N, Ev:event) :->
	"Add moving (overrule supreclass"::
	(   send(@xref_file_graph_node_recogniser, event, Ev)
	->  true
	;   send_super(N, event, Ev)
	).

popup(N, Popup:popup) :<-
	get_super(N, popup, Popup),
	send_list(Popup, append,
		  [ gap,
		    menu_item(show_exports,
			      message(@arg1, show_import_exports, export)),
		    menu_item(show_imports,
			      message(@arg1, show_import_exports, import)),
		    gap,
		    menu_item(hide,
			      message(@arg1, destroy))
		  ]).

show_import_exports(N, Which:{import,export}) :->
	"Show who I'm exporting to"::
	get(N, device, G),
	layout_new(G, 
		   (   (   Which == export
		       ->  send(N, create_export_links, @on)
		       ;   send(N, create_import_links, @on)
		       ),
		       send(G, update_links)
		   )).

layout_new(G, Goal) :-
	get(G?graphicals, find_all,
	    message(@arg1, instance_of, xref_file_graph_node), Nodes0),
	Goal,
	get(G?graphicals, find_all,
	    message(@arg1, instance_of, xref_file_graph_node), Nodes),
	send(Nodes, subtract, Nodes0),
	(   send(Nodes, empty)
	->  send(G, report, status, 'No nodes added')
	;   send(G, layout, Nodes),
	    get(Nodes, size, Size),
	    send(G, report, status, '%d nodes added', Size)
	).

:- pce_end_class(xref_file_graph_node).

:- pce_begin_class(xref_export_connection, tagged_connection).

variable(callables, prolog, get, "Callables in Import/export link").

initialise(C, From:xref_file_graph_node, To:xref_file_graph_node,
	   Callables:prolog) :->
	send_super(C, initialise, From, To),
	send(C, arrows, second),
	send(C, slot, callables, Callables),
	length(Callables, N),
	send(C, tag, xref_export_connection_tag(C, N)).

callables(C, Callables:prolog) :->
	send(C, slot, callables, Callables). % TBD: update tag?

called_by_popup(Conn, P:popup) :<-
	"Create popup to show relating predicates"::
	new(P, popup(called_by, message(Conn, edit_callable, @arg1))),
	get(Conn, callables, Callables),
	get(Conn?from, path, ExportFile),
	get(Conn?to, path, ImportFile),
	sort_callables(Callables, Sorted),
	forall(member(C, Sorted),
	       append_io_callable(P, ImportFile, ExportFile, C)).

%	append_io_callable(+Popup, -ImportFile, +Callable)

append_io_callable(P, ImportFile, ExportFile, Callable) :-
	callable_to_label(Callable, Label),
	send(P, append, new(MI, menu_item(@nil, @default, Label))),
	send(MI, popup, new(P2, popup)),
	send(P2, append,
	     menu_item(prolog('<definition>'(Callable)),
		       @default, definition?label_name)),
	send(P2, append, gap),
	qualify_from_file(Callable, ExportFile, QCall),
	findall(By, used_in(ImportFile, QCall, By), ByList0),
	sort_callables(ByList0, ByList),
	forall(member(C, ByList),
	       ( callable_to_label(C, CLabel),
		 send(P2, append, menu_item(prolog(C), @default, CLabel)))).

edit_callable(C, Callable:prolog) :->
	"Edit definition or callers"::
	(   Callable = '<definition>'(Def)
	->  get(C?from, path, ExportFile),
	    edit_callable(Def, ExportFile)
	;   get(C?to, path, ImportFile),
	    edit_callable(Callable, ImportFile)
	).
	
:- pce_end_class(xref_export_connection).


:- pce_begin_class(xref_export_connection_tag, text,
		   "Text showing import/export count").

variable(connection, xref_export_connection, get, "Related connection").

initialise(Tag, C:xref_export_connection, N:int) :->
	send(Tag, slot, connection, C),
	send_super(Tag, initialise, string('(%d)', N)),
	send(Tag, colour, blue),
	send(Tag, underline, @on).

:- pce_global(@xref_export_connection_tag_recogniser,
	      new(popup_gesture(@receiver?connection?called_by_popup, left))).

event(Tag, Ev:event) :->
	(   send_super(Tag, event, Ev)
	->  true
	;   send(@xref_export_connection_tag_recogniser, event, Ev)
	).

:- pce_end_class(xref_export_connection_tag).



%	export_link(+ExportingFile, -ImportingFile, -Callables)
%	export_link(-ExportingFile, +ImportingFile, -Callables)
%	
%	Callables are exported from ExportingFile to ImportingFile.

export_link(ExportFile, ImportingFile, Callables) :-
	setof(Callable,
	      export_link_1(ExportFile, ImportingFile, Callable),
	      Callables0),
	sort_callables(Callables0, Callables).


export_link_1(ExportFile, ImportFile, Callable) :-       % module export
	nonvar(ExportFile),
	xref_module(ExportFile, Module), !,
	(   xref_exported(ExportFile, Callable),
	    xref_defined(ImportFile, Callable, imported(ExportFile)),
	    xref_called(ImportFile, Callable)
	;   defined(ExportFile, Callable),
	    single_qualify(Module:Callable, QCall),
	    xref_called(ImportFile, QCall)
	),
	ImportFile \== ExportFile,
	atom(ImportFile).
export_link_1(ExportFile, ImportFile, Callable) :-	% Non-module export
	nonvar(ExportFile), !,
	defined(ExportFile, Callable),
	xref_called(ImportFile, Callable),
	atom(ImportFile),
	ExportFile \== ImportFile.
export_link_1(ExportFile, ImportFile, Callable) :-	% module import
	nonvar(ImportFile),
	xref_module(ImportFile, Module), !,
	xref_called(ImportFile, Callable),
	(   xref_defined(ImportFile, Callable, imported(ExportFile))
	;   single_qualify(Module:Callable, QCall),
	    QCall = M:G,
	    (	defined(ExportFile, G),
		xref_module(ExportFile, M)
	    ;	defined(ExportFile, QCall)
	    )
	),
	ImportFile \== ExportFile,
	atom(ExportFile).
export_link_1(ExportFile, ImportFile, Callable) :-	% Non-module import
	xref_called(ImportFile, Callable),
	\+ (  xref_defined(ImportFile, Callable, How),
	      How \= imported(_)
	   ),
					% see also undefined/2
	(   xref_defined(ImportFile, Callable, imported(ExportFile))
	;   defined(ExportFile, Callable),
	    \+ xref_module(ExportFile, _)
	;   Callable = _:_,
	    defined(ExportFile, Callable)
	;   Callable = M:G,
	    defined(ExportFile, G),
	    xref_module(ExportFile, M)
	).


		 /*******************************
		 *	     FILE TREE		*
		 *******************************/

:- pce_begin_class(xref_file_tree, toc_window,
		   "Show loaded files as a tree").
:- use_class_template(arm).

initialise(Tree) :->
	send_super(Tree, initialise),
	send(Tree, clear),
	listen(Tree, xref_refresh_file(File),
	       send(Tree, refresh_file, File)).

unlink(Tree) :->
	unlisten(Tree),
	send_super(Tree, unlink).

refresh_file(Tree, File:name) :->
	"Update given file"::
	(   get(Tree, node, File, Node)
	->  send(Node, set_flags)
	;   true
	).

collapse_node(_, _:any) :->
	true.

expand_node(_, _:any) :->
	true.

update(FL) :->
	get(FL, expanded_ids, Chain),
	send(FL, clear),
	send(FL, report, progress, 'Building source tree ...'),
	send(FL, append_all_sourcefiles),
	send(FL, expand_ids, Chain),
	send(@display, synchronise),
	send(FL, report, progress, 'Flagging files ...'),
	send(FL, set_flags),
	send(FL, report, done).

append_all_sourcefiles(FL) :->
	"Append all files loaded into Prolog"::
	forall(source_file(File),
	       send(FL, append, File)),
	send(FL, sort).

clear(Tree) :->
	"Remove all nodes, recreate the toplevel"::
	send_super(Tree, clear),
	send(Tree, root, new(Root, toc_folder(project, project))),
	forall(top_node(Name, Class),
	       (   New =.. [Class, Name, Name],
		   send(Tree, son, project, New))),
	send(Root, for_all, message(@arg1, collapsed, @off)).

append(Tree, File:name) :->
	"Add Prolog source file"::
	send(Tree, append_node, new(prolog_file_node(File))).
	
append_node(Tree, Node:toc_node) :->
	"Append a given node to the tree"::
	get(Node, parent_id, ParentId),
	(   get(Tree, node, ParentId, Parent)
	->  true
	;   send(Tree, append_node,
		 new(Parent, prolog_directory_node(ParentId)))
	),
	send(Parent, son, Node).
	
sort(Tree) :->
	forall(top_node(Name, _),
	       (   get(Tree, node, Name, Node),
		   send(Node, sort_sons, ?(@arg1, compare, @arg2)),
		   send(Node?sons, for_all, message(@arg1, sort))
	       )).

select_node(Tree, File:name) :->
	"User selected a node"::
	(   exists_file(File)
	->  send(Tree?frame, file_info, File)
	;   true
	).

set_flags(Tree) :->
	"Set alert-flags on all nodes"::
	forall(top_node(Name, _),
	       (   get(Tree, node, Name, Node),
		   (   send(Node, instance_of, prolog_directory_node)
		   ->  send(Node, set_flags)
		   ;   send(Node?sons, for_all, message(@arg1, set_flags))
		   )
	       )).

top_node('.',		prolog_directory_node).
top_node('alias',	toc_folder).
top_node('/',		prolog_directory_node).

:- pce_end_class(xref_file_tree).


:- pce_begin_class(prolog_directory_node, toc_folder,
		   "Represent a directory").

variable(flags, name*, get, "Warning status").

initialise(DN, Dir:name, Label:[name]) :->
	"Create a directory node"::
	(   Label \== @default
	->  Name = Label
	;   alias_path(Name, Dir)
	->  true
	;   file_base_name(Dir, Name)
	),
	send_super(DN, initialise, xref_directory_text(Dir, Name), Dir).

parent_id(FN, ParentId:name) :<-
	"Get id for the parent"::
	get(FN, identifier, Path),
	(   alias_path(_, Path)
	->  ParentId = alias
	;   file_directory_name(Path, ParentId)
	).

sort(DN) :->
	"Sort my sons"::
	send(DN, sort_sons, ?(@arg1, compare, @arg2)),
	send(DN?sons, for_all, message(@arg1, sort)).

compare(DN, Node:toc_node, Diff:{smaller,equal,larger}) :<-
	"Compare for sorting children"::
	(   send(Node, instance_of, prolog_file_node)
	->  Diff = smaller
	;   get(DN, label, L1),
	    get(Node, label, L2),
	    get(L1, compare, L2, Diff)
	).

set_flags(DN) :->
	"Set alert images"::
	send(DN?sons, for_all, message(@arg1, set_flags)),
	(   get(DN?sons, find, @arg1?flags \== ok, _Node)
	->  send(DN, collapsed_image, @xref_alert_closedir),
	    send(DN, expanded_image, @xref_alert_opendir),
	    send(DN, slot, flags, alert)
	;   send(DN, collapsed_image, @xref_ok_closedir),
	    send(DN, expanded_image, @xref_ok_opendir),
	    send(DN, slot, flags, ok)
	),
	send(@display, synchronise).

:- pce_end_class(prolog_directory_node).


:- pce_begin_class(prolog_file_node, toc_file,
		   "Represent a file").

variable(flags, name*, get, "Warning status").

initialise(FN, File:name) :->
	"Create from a file"::
	absolute_file_name(File, Path),
	send_super(FN, initialise, new(T, xref_file_text(Path)), Path),
	send(T, default_action, info).

basename(FN, BaseName:name) :<-
	"Get basename of the file for sorting"::
	get(FN, identifier, File),
	file_base_name(File, BaseName).

parent_id(FN, ParentId:name) :<-
	"Get id for the parent"::
	get(FN, identifier, Path),
	file_directory_name(Path, Dir),
	(   alias_path('.', Dir)
	->  ParentId = '.'
	;   ParentId = Dir
	).

sort(_) :->
	true.

compare(FN, Node:toc_node, Diff:{smaller,equal,larger}) :<-
	"Compare for sorting children"::
	(   send(Node, instance_of, prolog_directory_node)
	->  Diff = larger
	;   get(FN, basename, L1),
	    get(Node, basename, L2),
	    get(L1, compare, L2, Diff)
	).

set_flags(FN) :->
	"Set alert images"::
	get(FN, identifier, File),
	(   file_warnings(File, _)
	->  send(FN, image, @xref_alert_file),
	    send(FN, slot, flags, alert)
	;   send(FN, image, @xref_ok_file),
	    send(FN, slot, flags, ok)
	),
	send(@display, synchronise).

:- pce_global(@xref_ok_file,
	      make_xref_image([ image('16x16/doc.xpm'),
				image('16x16/ok.xpm')
			      ])).
:- pce_global(@xref_alert_file,
	      make_xref_image([ image('16x16/doc.xpm'),
				image('16x16/alert.xpm')
			      ])).

:- pce_global(@xref_ok_opendir,
	      make_xref_image([ image('16x16/opendir.xpm'),
				image('16x16/ok.xpm')
			      ])).
:- pce_global(@xref_alert_opendir,
	      make_xref_image([ image('16x16/opendir.xpm'),
				image('16x16/alert.xpm')
			      ])).

:- pce_global(@xref_ok_closedir,
	      make_xref_image([ image('16x16/closedir.xpm'),
				image('16x16/ok.xpm')
			      ])).
:- pce_global(@xref_alert_closedir,
	      make_xref_image([ image('16x16/closedir.xpm'),
				image('16x16/alert.xpm')
			      ])).

make_xref_image([First|More], Image) :-
	new(Image, image(@nil, 0, 0, pixmap)),
	send(Image, copy, First),
	forall(member(I2, More),
	       send(Image, draw_in, bitmap(I2))).

:- pce_end_class(prolog_file_node).




		 /*******************************
		 *	     FILE INFO		*
		 *******************************/


:- pce_begin_class(prolog_file_info, window,
		   "Show information on File").
:- use_class_template(arm).

variable(tabular,     tabular, get, "Displayed table").
variable(prolog_file, name*,   get, "Displayed Prolog file").

initialise(W, File:[name]*) :->
	send_super(W, initialise),
	send(W, pen, 0),
	send(W, scrollbars, vertical),
	send(W, display, new(T, tabular)),
	send(T, rules, all),
	send(T, cell_spacing, -1),
	send(W, slot, tabular, T),
	(   atom(File)
	->  send(W, prolog_file, File)
	;   true
	).

resize(W) :->
	send_super(W, resize),
	get(W?visible, width, Width),
	send(W?tabular, table_width, Width-3).


file(V, File0:name*) :->
	"Set vizualized file"::
	(   File0 == @nil
	->  File = File0
	;   absolute_file_name(File0, File)
	),
	(   get(V, prolog_file, File)
	->  true
	;   send(V, slot, prolog_file, File),
	    send(V, update)
	).


clear(W) :->
	send(W?tabular, clear).


update(V) :->
	"Show information on the current file"::
	send(V, clear),
	send(V, scroll_to, point(0,0)),
	(   get(V, prolog_file, File),
	    File \== @nil
	->  send(V?frame, xref_file, File), % Make sure data is up-to-date
	    send(V, show_info)
	;   true
	).


module(W, Module:name) :<-
	"Module associated with this file"::
	get(W, prolog_file, File),
	(   xref_module(File, Module)
	->  true
	;   Module = user		% TBD: does not need to be true!
	).

:- pce_group(info).

show_info(W) :->
	get(W, tabular, T),
	BG = (background := khaki1),
	get(W, prolog_file, File),
	new(FG, xref_file_text(File)),
	send(FG, font, huge),
	send(T, append, FG, halign := center, colspan := 2, BG),
	send(T, next_row),
	send(W, show_module),
	send(W, show_modified),
	send(W, show_undefined),
	send(W, show_not_called),
	send(W, show_exports),
	send(W, show_imports),
	true.

show_module(W) :->
	"Show basic module info"::
	get(W, prolog_file, File),
	get(W, tabular, T),
	(   xref_module(File, Module)
	->  send(T, append, 'Module:', bold, right),
	    send(T, append, Module),
	    send(T, next_row)
	;   true
	).

show_modified(W) :->
	get(W, prolog_file, File),
	get(W, tabular, T),
	time_file(File, Stamp),
	convert_time(Stamp, Modified),
	send(T, append, 'Modified:', bold, right),
	send(T, append, Modified),
	send(T, next_row).

show_exports(W) :->
	get(W, prolog_file, File),
	(   xref_module(File, Module),
	    findall(E, xref_exported(File, E), Exports),
	    Exports \== []
	->  send(W, show_export_header, export, imported_by),
	    sort_callables(Exports, Sorted),
	    forall(member(Callable, Sorted),
		   send(W, show_module_export, File, Module, Callable))
	;   true
	),
	(   findall(C-Fs,
		    ( setof(F, export_link_1(File, F, C), Fs),
		      \+ xref_exported(File, C)),
		    Pairs0),
	    Pairs0 \== []
	->  send(W, show_export_header, defined, used_by),
	    keysort(Pairs0, Pairs),	% TBD
	    forall(member(Callable-ImportFiles, Pairs),
		   send(W, show_file_export, Callable, ImportFiles))
	;   true
	).

show_export_header(W, Left:name, Right:name) :->
	get(W, tabular, T),
	BG = (background := khaki1),
	send(T, append, Left?label_name, bold, center, BG),
	send(T, append, Right?label_name, bold, center, BG),
	send(T, next_row).

show_module_export(W, File:name, Module:name, Callable:prolog) :->
	get(W, prolog_file, File),
	get(W, tabular, T),
	send(T, append, xref_predicate_text(Module:Callable, @default, File)),
	findall(In, exported_to(File, Callable, In), InL),
	send(T, append, new(XL, xref_graphical_list)),
	(   InL == []
	->  true
	;   sort_files(InL, Sorted),
	    forall(member(F, Sorted),
		   send(XL, append, xref_imported_by(F, Callable)))
	),
	send(T, next_row).
	
show_file_export(W, Callable:prolog, ImportFiles:prolog) :->
	get(W, prolog_file, File),
	get(W, tabular, T),
	send(T, append, xref_predicate_text(Callable, @default, File)),
	send(T, append, new(XL, xref_graphical_list)),
	sort_files(ImportFiles, Sorted),
	qualify_from_file(Callable, File, QCall),
	forall(member(F, Sorted),
	       send(XL, append, xref_imported_by(F, QCall))),
	send(T, next_row).

qualify_from_file(Callable, _, Callable) :-
	Callable = _:_, !.
qualify_from_file(Callable, File, M:Callable) :-
	xref_module(File, M), !.
qualify_from_file(Callable, _, Callable).


%	exported_to(+ExportFile, +Callable, -ImportFile)
%	
%	ImportFile imports Callable from ExportFile.  The second clause
%	deals with auto-import.
%	
%	TBD: Make sure the autoload library is loaded before we begin.

exported_to(ExportFile, Callable, ImportFile) :-
	xref_defined(ImportFile, Callable, imported(ExportFile)),
	atom(ImportFile).		% avoid XPCE buffers.
exported_to(ExportFile, Callable, ImportFile) :-
	'$autoload':library_index(Callable, _, ExportFileNoExt),
	file_name_extension(ExportFileNoExt, _, ExportFile),
	xref_called(ImportFile, Callable),
	atom(ImportFile),
	\+ xref_defined(ImportFile, Callable, _).

show_imports(W) :->
	"Show predicates we import"::
	get(W, prolog_file, File),
	findall(E-Cs,
		setof(C, export_link_1(E, File, C), Cs),
		Pairs),
	(   Pairs \== []
	->  sort(Pairs, Sorted),	% TBD: use sort_files/2
	    (   xref_module(File, _)
	    ->  send(W, show_export_header, from, imports)
	    ;	send(W, show_export_header, from, uses)
	    ),
	    forall(member(E-Cs, Sorted),
		   send(W, show_import, E, Cs))
	;   true
	).

show_import(W, File:name, Callables:prolog) :->
	"Show imports from file"::
	get(W, tabular, T),
	send(T, append, xref_file_text(File)),
	send(T, append, new(XL, xref_graphical_list)),
	sort_callables(Callables, Sorted),
	forall(member(C, Sorted),
	       send(XL, append, xref_predicate_text(C, @default, File))),
	send(T, next_row).


show_undefined(W) :->
	"Add underfined predicates to table"::
	get(W, prolog_file, File),
	findall(Undef, undefined(File, Undef), UndefList),
	(   UndefList == []
	->  true
	;   BG = (background := khaki1),
	    get(W, tabular, T),
	    (	setting(warn_autoload, true)
	    ->  Label = 'Undefined/autoload'
	    ;   Label = 'Undefined'
	    ),
	    send(T, append, Label, bold, center, BG),
	    send(T, append, 'Called by', bold, center, BG),
	    send(T, next_row),
	    sort_callables(UndefList, Sorted),
	    forall(member(Callable, Sorted),
		   send(W, show_undef, Callable))
	).

show_undef(W, Callable:prolog) :->
	"Show undefined predicate"::
	get(W, prolog_file, File),
	get(W, module, Module),
	get(W, tabular, T),
	send(T, append,
	     xref_predicate_text(Module:Callable, undefined, File)),
	send(T, append, new(L, xref_graphical_list)),
	findall(By, xref_called(File, Callable, By), By),
	sort_callables(By, Sorted),
	forall(member(P, Sorted),
	       send(L, append, xref_predicate_text(Module:P, called_by, File))),
	send(T, next_row).
	

show_not_called(W) :->
	"Show predicates that are not called"::
	get(W, prolog_file, File),
	findall(NotCalled, not_called(File, NotCalled), NotCalledList),
	(   NotCalledList == []
	->  true
	;   BG = (background := khaki1),
	    get(W, tabular, T),
	    send(T, append, 'Not called', bold, center, colspan := 2, BG),
	     send(T, next_row),
	    sort_callables(NotCalledList, Sorted),
	    forall(member(Callable, Sorted),
		   send(W, show_not_called_pred, Callable))
	).

show_not_called_pred(W, Callable:prolog) :->
	"Show a not-called predicate"::
	get(W, prolog_file, File),
	get(W, module, Module),
	get(W, tabular, T),
	send(T, append,
	     xref_predicate_text(Module:Callable, not_called, File),
	     colspan := 2),
	send(T, next_row).

:- pce_end_class(prolog_file_info).


:- pce_begin_class(xref_predicate_text, text,
		   "Text representing a predicate").

class_variable(colour, colour, dark_green).

variable(callable,	 prolog, get, "Predicate indicator").
variable(classification, [name], get, "Classification of the predicate").
variable(file,		 name*,  get, "File of predicate").

initialise(T, Callable0:prolog,
	   Class:[{undefined,called_by,not_called}],
	   File:[name]) :->
	"Create from callable or predicate indicator"::
	single_qualify(Callable0, Callable),
	send(T, slot, callable, Callable),
	callable_to_label(Callable, File, Label),
	send_super(T, initialise, Label),
	(   File \== @default
	->  send(T, slot, file, File)
	;   true
	),
	send(T, classification, Class).
       
%	single_qualify(+Term, -Qualified)
%	
%	Strip redundant M: from the term, leaving at most one qualifier.

single_qualify(_:Q0, Q) :-
	is_qualified(Q0), !,
	single_qualify(Q0, Q).
single_qualify(Q, Q).

is_qualified(M:_) :-
	atom(M).

pi(IT, PI:prolog) :<-
	"Get predicate as predicate indicator (Name/Arity)"::
	get(IT, callable, Callable),
	to_predicate_indicator(Callable, PI).

classification(T, Class:[name]) :->
	send(T, slot, classification, Class),
	(   Class == undefined
	->  get(T, callable, Callable),
	    strip_module(Callable, _, Plain),
	    (	autoload_predicate(Plain)
	    ->  send(T, colour, navy_blue),
		send(T, slot, classification, autoload)
	    ;   global_predicate(Plain)
	    ->  send(T, colour, navy_blue),
		send(T, slot, classification, global)
	    ;   send(T, colour, red)
	    )
	;   Class == not_called
	->  send(T, colour, red)
	;   true
	).

:- pce_global(@xref_predicate_text_recogniser,
	      new(handler_group(@arm_recogniser,
				click_gesture(left, '', single, 
					      message(@receiver, edit))))).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@xref_predicate_text_recogniser, event, Ev)
	).


arm(TF, Val:bool) :->
	"Preview activiity"::
	(   Val == @on
	->  send(TF, underline, @on),
	    (	get(TF, classification, Class),
		Class \== @default
	    ->  send(TF, report, status,
		     '%s predicate %s', Class?capitalise, TF?string)
	    ;   send(TF, report, status,
		     'Predicate %s', TF?string)
	    )
	;   send(TF, underline, @off),
	    send(TF, report, status, '')
	).

edit(T) :->
	get(T, file, File),
	get(T, callable, Callable),
	edit_callable(Callable, File).

:- pce_end_class(xref_predicate_text).


:- pce_begin_class(xref_file_text, text,
		   "Represent a file-name").

variable(path,		 name,	       get, "Filename represented").
variable(default_action, name := edit, both, "Default on click").

initialise(TF, File:name) :->
	absolute_file_name(File, Path),
	short_file_name(Path, ShortId),
	short_file_name_to_atom(ShortId, Label),
	send_super(TF, initialise, Label),
	send(TF, name, Path),
	send(TF, slot, path, Path).

:- pce_global(@xref_file_text_recogniser,
	      make_xref_file_text_recogniser).

make_xref_file_text_recogniser(G) :-
	new(C, click_gesture(left, '', single, 
			     message(@receiver, run_default_action))),
	new(P, popup_gesture(@arg1?popup)),
	new(D, drag_and_drop_gesture(left)),
	send(D, cursor, @default),
	new(G, handler_group(C, D, P, @arm_recogniser)).

popup(_, Popup:popup) :<-
	new(Popup, popup),
	send_list(Popup, append,
		  [ menu_item(edit, message(@arg1, edit)),
		    menu_item(info, message(@arg1, info)),
		    menu_item(header, message(@arg1, header))
		  ]).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@xref_file_text_recogniser, event, Ev)
	).

arm(TF, Val:bool) :->
	"Preview activiity"::
	(   Val == @on
	->  send(TF, underline, @on),
	    send(TF, report, status, 'File %s', TF?path)
	;   send(TF, underline, @off),
	    send(TF, report, status, '')
	).

run_default_action(T) :->
	get(T, default_action, Def),
	send(T, Def).

edit(T) :->
	get(T, path, Path),
	edit(file(Path)).

info(T) :->
	get(T, path, Path),
	send(T?frame, file_info, Path).

header(T) :->
	get(T, path, Path),
	send(T?frame, file_header, Path).

prolog_source(T, Src:string) :<-
	"Import declarations"::
	get(T, path, File),
	new(V, xref_view),
	send(V, file_header, File),
	get(V?text_buffer, contents, Src),
	send(V, destroy).

:- pce_end_class(xref_file_text).


:- pce_begin_class(xref_directory_text, text,
		   "Represent a directory-name").

variable(path,		 name,	       get, "Filename represented").

initialise(TF, Dir:name, Label:[name]) :->
	absolute_file_name(Dir, Path),
	(   Label == @default
	->  file_base_name(Path, TheLabel)
	;   TheLabel = Label
	),
	send_super(TF, initialise, Label),
	send(TF, slot, path, Path).

files(DT, Files:chain) :<-
	"List of files that belong to this directory"::
	new(Files, chain),
	get(DT, path, Path),
	(   source_file(File),
	    sub_atom(File, 0, _, _, Path),
	    send(Files, append, File),
	    fail ; true
	).

:- pce_global(@xref_directory_text_recogniser,
	      make_xref_directory_text_recogniser).

make_xref_directory_text_recogniser(G) :-
	new(D, drag_and_drop_gesture(left)),
	send(D, cursor, @default),
	new(G, handler_group(D, @arm_recogniser)).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@xref_directory_text_recogniser, event, Ev)
	).

arm(TF, Val:bool) :->
	"Preview activiity"::
	(   Val == @on
	->  send(TF, underline, @on),
	    send(TF, report, status, 'Directory %s', TF?path)
	;   send(TF, underline, @off),
	    send(TF, report, status, '')
	).

:- pce_end_class(xref_directory_text).


:- pce_begin_class(xref_imported_by, figure,
		   "Indicate import of callable into file").

variable(callable, prolog, get, "Callable term of imported predicate").

:- pce_global(@xref_horizontal_format,
	      make_xref_horizontal_format).

make_xref_horizontal_format(F) :-
	new(F, format(vertical, 1, @on)),
	send(F, row_sep, 3),
	send(F, column_sep, 0).

initialise(IT, File:name, Imported:prolog) :->
	send_super(IT, initialise),
	send(IT, format, @xref_horizontal_format),
	send(IT, display, new(F, xref_file_text(File))),
	send(F, name, file_text),
	send(IT, slot, callable, Imported),
	send(IT, show_called_by).
	
path(IT, Path:name) :<-
	"Represented file"::
	get(IT, member, file_text, Text),
	get(Text, path, Path).

show_called_by(IT) :->
	"Add number indicating calls"::
	get(IT, called_by, List),
	length(List, N),
	send(IT, display, new(T, text(string('(%d)', N)))),
	send(T, name, called_count),
	(   N > 0
	->  send(T, underline, @on),
	    send(T, colour, blue),
	    send(T, recogniser, @xref_called_by_recogniser)
	;   send(T, colour, grey60)
	).

called_by(IT, ByList:prolog) :<-
	"Return list of callables satisfied by the import"::
	get(IT, path, Source),
	get(IT, callable, Callable),
	findall(By, used_in(Source, Callable, By), ByList).

%	used_in(+Source, +QCallable, -CalledBy)
%	
%	Determine which the callers for   QCallable in Source. QCallable
%	is qualified with the module of the exporting file (if any).

used_in(Source, M:Callable, By) :-		% we are the same module
	xref_module(Source, M), !,
	xref_called(Source, Callable, By).
used_in(Source, _:Callable, By) :-		% we imported
	xref_defined(Source, Callable, imported(_)), !,
	xref_called(Source, Callable, By).
used_in(Source, Callable, By) :-
	xref_called(Source, Callable, By).
used_in(Source, Callable, '<export>') :-
	xref_exported(Source, Callable).

:- pce_group(event).

:- pce_global(@xref_called_by_recogniser,
	      new(popup_gesture(@receiver?device?called_by_popup, left))).

called_by_popup(IT, P:popup) :<-
	"Show called where import is called"::
	new(P, popup(called_by, message(IT, edit_called_by, @arg1))),
	get(IT, called_by, ByList),
	sort_callables(ByList, Sorted),
	forall(member(C, Sorted),
	       ( callable_to_label(C, Label),
		 send(P, append, menu_item(prolog(C), @default, Label)))).

edit_called_by(IT, Called:prolog) :->
	"Edit file on the predicate Called"::
	get(IT, path, Source),
	edit_callable(Called, Source).

:- pce_end_class(xref_imported_by).


:- pce_begin_class(xref_graphical_list, figure,
		   "Show list of exports to files").

variable(wrap, {extend,wrap,wrap_fixed_width,clip} := extend, get,
	 "Wrapping mode").

initialise(XL) :->
	send_super(XL, initialise),
	send(XL, margin, 500, wrap).

append(XL, I:graphical) :->
	(   send(XL?graphicals, empty)
	->  true
	;   send(XL, display, text(', '))
	),
	send(XL, display, I).

:- pce_group(layout).

:- pce_global(@xref_graphical_list_format,
	      make_xref_graphical_list_format).

make_xref_graphical_list_format(F) :-
	new(F, format(horizontal, 500, @off)),
	send(F, column_sep, 0),
	send(F, row_sep, 0).

margin(T, Width:int*, How:[{wrap,wrap_fixed_width,clip}]) :->
	"Wrap items to indicated width"::
	(   Width == @nil
	->  send(T, slot, wrap, extend),
	    send(T, format, @rdf_composite_format)
	;   send(T, slot, wrap, How),
	    How == wrap
	->  FmtWidth is max(10, Width),
	    new(F, format(horizontal, FmtWidth, @off)),
	    send(F, column_sep, 0),
	    send(F, row_sep, 0),
	    send(T, format, F)
	;   throw(tbd)
	).

:- pce_end_class(xref_graphical_list).



		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

:- pce_begin_class(xref_predicate_browser, browser,
		 "Show loaded files").

initialise(PL) :->
	send_super(PL, initialise),
	send(PL, popup, new(P, popup)),
	send_list(P, append,
		  [ menu_item(edit, message(@arg1, edit))
		  ]).

update(PL) :->
	send(PL, clear),
	forall((defined(File, Callable), atom(File), \+ library_file(File)),
	       send(PL, append, Callable, @default, File)),
	forall((xref_current_source(File), atom(File), \+library_file(File)),
	       forall(undefined(File, Callable),
		      send(PL, append, Callable, undefined, File))),
	send(PL, sort).

append(PL, Callable:prolog, Class:[name], File:[name]) :->
	send_super(PL, append, xref_predicate_dict_item(Callable, Class, File)).

:- pce_end_class(xref_predicate_browser).


:- pce_begin_class(xref_predicate_dict_item, dict_item,
		   "Represent a Prolog predicate").

variable(callable, prolog, get, "Callable term").
variable(file,     name*,  get, "Origin file").

initialise(PI, Callable0:prolog, _Class:[name], File:[name]) :->
	"Create from callable, class and file"::
	single_qualify(Callable0, Callable),
	send(PI, slot, callable, Callable),
	callable_to_label(Callable, Label),
	send_super(PI, initialise, Label),
	(   File \== @default
	->  send(PI, slot, file, File)
	;   true
	).

edit(PI) :->
	"Edit Associated prediate"::
	get(PI, file, File),
	get(PI, callable, Callable),
	edit_callable(Callable, File).

:- pce_end_class(xref_predicate_dict_item).


		 /*******************************
		 *	   UTIL CLASSES		*
		 *******************************/

:- pce_begin_class(xref_view, view,
		   "View with additional facilities for formatting").

initialise(V) :->
	send_super(V, initialise),
	send(V, font, fixed).

update(_) :->
	true.				% or ->clear?  ->destroy?

file_header(View, File:name) :->
	"Create import/export fileheader for File"::
	(   xref_module(File, _)
	->  Decls = Imports
	;   xref_file_exports(File, Export),
	    Decls = [Export|Imports]
	),
	xref_file_imports(File, Imports),
	send(View, clear),
	send(View, declarations, Decls),
	(   (   nonvar(Export)
	    ->  send(View, report, status,
		     'Created module header for non-module file %s', File)
	    ;   send(View, report, status,
		     'Created import header for module file %s', File)
	    )
	->  true
	;   true
	).

declarations(V, Decls:prolog) :->
	pce_open(V, append, Out),
	call_cleanup(print_decls(Decls, Out), close(Out)).

print_decls([], _) :- !.
print_decls([H|T], Out) :- !,
	print_decls(H, Out),
	print_decls(T, Out).
print_decls(Term, Out) :-
	portray_clause(Out, Term).

:- pce_end_class(xref_view).


		 /*******************************
		 *	  LOGIC (MAY MOVE)	*
		 *******************************/

%	short_file_name(+File, -ShortId)
%	
%	Create a short name for a file

short_file_name(Path, ShortId) :-
	(   alias_path(Alias, Dir),
	    atom_concat(Dir, Local, Path)
	->  (   Alias == '.'
	    ->  ShortId = Local
	    ;   file_name_extension(Base, pl, Local)
	    ->  ShortId =.. [Alias, Base]
	    ;   ShortId =.. [Alias, Local]
	    )
	;   ShortId = Path
	).
	
%	short_file_name_to_atom(+ShortId, -Atom)
%	
%	Convert a short filename into an atom

short_file_name_to_atom(Atom, Atom) :-
	atomic(Atom), !.
short_file_name_to_atom(Term, Atom) :-
	term_to_atom(Term, Atom).


%	alias_path(-Alias, ?Dir)
%	
%	Enumerate the defined aliases, sorting   them  starting with the
%	longest path-name, so the first hit is immediately the best one.

:- dynamic
	alias_cache/2.

alias_path(Alias, Dir) :-
	(   alias_cache(_, _)
	->  true
	;   build_alias_cache
	),
	(   nonvar(Dir)
	->  ensure_slash(Dir, DirSlash),
	    alias_cache(Alias, DirSlash)
	;   alias_cache(Alias, Dir)
	).

build_alias_cache :-
	findall(t(DirLen, AliasLen, Alias, Dir),
		search_path(Alias, Dir, AliasLen, DirLen), Ts),
	sort(Ts, List0),
	reverse(List0, List),
	forall(member(t(_, _, Alias, Dir), List),
	       assert(alias_cache(Alias, Dir))).

search_path('.', Here, 999, DirLen) :-
	working_directory(Here0, Here0),
	ensure_slash(Here0, Here),
	atom_length(Here, DirLen).
search_path(Alias, Dir, AliasLen, DirLen) :-
	file_search_path(Alias, _),
	Spec =.. [Alias,'.'],
	atom_length(Alias, AliasLen0),
	AliasLen is 1000 - AliasLen0,	% must do reverse sort
	absolute_file_name(Spec, Dir0,
			   [ file_type(directory),
			     access(read),
			     solutions(all),
			     file_errors(fail)
			   ]),
	ensure_slash(Dir0, Dir),
	atom_length(Dir, DirLen).

ensure_slash(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).

%	library_file(+Path)
%	
%	True if Path comes from the Prolog tree and must be considered a
%	library.

library_file(Path) :-
	current_prolog_flag(home, Home),
	sub_atom(Path, 0, _, _, Home).

%	profile_file(+Path)
%	
%	True if path is a personalisation file.  This is a bit hairy.

profile_file(Path) :-
	short_file_name(Path, user_profile(File)),
	known_profile_file(File).

known_profile_file('.plrc').
known_profile_file('pl.ini').
known_profile_file('.pceemacsrc').
known_profile_file(File) :-
	sub_atom(File, 0, _, _, 'lib/xpce/emacs').

%	sort_files(+Files, -Sorted)
%	
%	Sort files, keeping groups comming from the same alias together.

sort_files(Files0, Sorted) :-
	sort(Files0, Files),		% remove duplicates
	maplist(key_file, Files, Keyed),
	keysort(Keyed, KSorted),
	unkey(KSorted, Sorted).

key_file(File, Key-File) :-
	short_file_name(File, Key).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

%	available(+File, +Callable, -HowDefined)
%	
%	True if Callable is available in File.

available(File, Called, How) :-
	xref_defined(File, Called, How0), !,
	How = How0.
available(_, Called, How) :-
	built_in_predicate(Called), !,
	How = builtin.
available(_, Called, How) :-
	setting(warn_autoload, false),
	autoload_predicate(Called), !,
	How = autoload.
available(_, Called, How) :-
	setting(warn_autoload, false),
	global_predicate(Called), !,
	How = global.
available(_, Called, How) :-
	Called = _:_,
	defined(_, Called), !,
	How = module_qualified.
available(_, M:G, How) :-
	defined(ExportFile, G),
	xref_module(ExportFile, M), !,
	How = module_overruled.
available(_, Called, How) :-
	defined(ExportFile, Called),
	\+ xref_module(ExportFile, _), !,
	How == plain_file.


%	built_in_predicate(+Callable)
%	
%	True if Callable is a built-in

built_in_predicate(Goal) :-
	strip_module(Goal, _, Plain),
	xref_built_in(Plain).

%	autoload_predicate(+Callable)
%	autoload_predicate(+Callable, -File)
%	
%	True if Callable can be autoloaded.  TBD: make sure the autoload
%	index is up-to-date.

autoload_predicate(Goal) :-
	'$autoload':library_index(Goal, _, _).


autoload_predicate(Goal, File) :-
	'$autoload':library_index(Goal, _, FileNoExt),
	file_name_extension(FileNoExt, pl, File).


%	global_predicate(+Callable)
%	
%	True if Callable can  be  auto-imported   from  the  global user
%	module.

global_predicate(Goal) :-
	predicate_property(user:Goal, _), !.

%	to_predicate_indicator(+Term, -PI)
%	
%	Convert to a predicate indicator.

to_predicate_indicator(PI, PI) :-
	is_predicate_indicator(PI), !.
to_predicate_indicator(Callable, PI) :-
	callable(Callable),
	predicate_indicator(Callable, PI).

%	is_predicate_indicator(=PI)
%	
%	True if PI is a predicate indicator.

is_predicate_indicator(Name/Arity) :-
	atom(Name),
	integer(Arity).
is_predicate_indicator(Module:Name/Arity) :-
	atom(Module),
	atom(Name),
	integer(Arity).

%	predicate_indicator(+Callable, -Name)
%	
%	Generate a human-readable predicate indicator

predicate_indicator(Module:Goal, PI) :-
	atom(Module), !,
	predicate_indicator(Goal, PI0),
	(   hidden_module(Module)
	->  PI = PI0
	;   PI = Module:PI0
	).
predicate_indicator(Goal, Name/Arity) :-
	callable(Goal), !,
	functor(Goal, Name, Arity).
predicate_indicator(Goal, Goal).

hidden_module(user) :- !.
hidden_module(system) :- !.
hidden_module(M) :-
	sub_atom(M, 0, _, _, $).

%	sort_callables(+List, -Sorted)
%	
%	Sort list of callable terms.

sort_callables(Callables, Sorted) :-
	key_callables(Callables, Tagged),
	keysort(Tagged, KeySorted),
	unkey(KeySorted, SortedList),
	ord_list_to_set(SortedList, Sorted).

key_callables([], []).
key_callables([H0|T0], [Key-H0|T]) :-
	key_callable(H0, Key),
	key_callables(T0, T).
	
key_callable(Callable, k(Name, Arity, Module)) :-
	predicate_indicator(Callable, PI),
	(   PI = Name/Arity
	->  Module = user
	;   PI = Module:Name/Arity
	).

unkey([], []).
unkey([_-H|T0], [H|T]) :-
	unkey(T0, T).

%	ord_list_to_set(+OrdList, -OrdSet)
%	
%	Removed duplicates (after unification) from an ordered list,
%	creating a set.

ord_list_to_set([], []).
ord_list_to_set([H|T0], [H|T]) :-
	ord_remove_same(H, T0, T1),
	ord_list_to_set(T1, T).

ord_remove_same(H, [H|T0], T) :- !,
	ord_remove_same(H, T0, T).
ord_remove_same(_, L, L).


%	callable_to_label(+Callable, +File, -Label)
%	callable_to_label(+Callable, -Label)

callable_to_label(Callable, Label) :-
	callable_to_label(Callable, @nil, Label).

callable_to_label(pce_principal:send_implementation(Id,_,_), _, Id) :-
	atom(Id), !.
callable_to_label(pce_principal:get_implementation(Id,_,_,_), _, Id) :-
	atom(Id), !.
callable_to_label('<export>', _, '<export>') :- !.
callable_to_label('<directive>'(Line), _, Label) :- !,
	atom_concat('<directive>@', Line, Label).
callable_to_label(_:'<directive>'(Line), _, Label) :- !,
	atom_concat('<directive>@', Line, Label).
callable_to_label(Callable, File, Label) :-
	to_predicate_indicator(Callable, PI0),
	(   PI0 = M:PI1
	->  (	atom(File),
		xref_module(File, M)
	    ->  PI = PI1
	    ;   PI = PI0
	    )
	;   PI = PI0
	),
	term_to_atom(PI, Label).

%	edit_callable(+Callable, +File)

edit_callable('<export>', File) :- !,
	edit(file(File)).
edit_callable(Callable, File) :-
	local_callable(Callable, File, Local),
	(   xref_defined(File, Local, How),
	    xref_definition_line(How, Line)
	->  edit(file(File, line(Line)))
	;   autoload_predicate(Local)
	->  functor(Local, Name, Arity),
	    edit(Name/Arity)
	).
edit_callable(pce_principal:send_implementation(Id,_,_), _) :-
	atom(Id),
	concat_atom([Class,Method], ->, Id), !,
	edit(send(Class, Method)).
edit_callable(pce_principal:get_implementation(Id,_,_,_), _) :-
	atom(Id),
	concat_atom([Class,Method], <-, Id), !,
	edit(get(Class, Method)).
edit_callable('<directive>'(Line), File) :-
	File \== @nil, !,
	edit(file(File, line(Line))).
edit_callable(_:'<directive>'(Line), File) :-
	File \== @nil, !,
	edit(file(File, line(Line))).
edit_callable(Callable, _) :-
	to_predicate_indicator(Callable, PI),
	edit(PI).

local_callable(M:Callable, File, Callable) :-
	xref_module(File, M), !.
local_callable(Callable, _, Callable).


		 /*******************************
		 *	      WARNINGS		*
		 *******************************/

%	file_warnings(+File:atom, -Warnings:list(atom))
%	
%	Unify Warnings with a list  of   dubious  things  found in File.
%	Intended to create icons.  Fails if the file is totally ok.

file_warnings(File, Warnings) :-
	setof(W, file_warning(File, W), Warnings).

file_warning(File, undefined) :-
	undefined(File, _) -> true.
file_warning(File, not_called) :-
	setting(warn_not_called, true),
	not_called(File, _) -> true.


%	not_called(+File, -Callable)
%	
%	Callable is a term defined in File, and for which no callers can
%	be found.

not_called(File, NotCalled) :-		% module version
	xref_module(File, Module), !,
	defined(File, NotCalled),
	\+ (   xref_called(File, NotCalled)
	   ;   xref_exported(File, NotCalled)
	   ;   xref_hook(NotCalled)
	   ;   xref_hook(Module:NotCalled)
	   ;   NotCalled = _:Goal,
	       xref_hook(Goal)
	   ;   xref_called(_, Module:NotCalled)
	   ;   NotCalled = _:_,
	       xref_called(_, NotCalled)
	   ;   NotCalled = M:G,
	       xref_called(ModFile, G),
	       xref_module(ModFile, M)
	   ).
not_called(File, NotCalled) :-		% non-module version
	defined(File, NotCalled),
	\+ (   xref_called(ImportFile, NotCalled),
	       \+ xref_module(ImportFile, _)
	   ;   NotCalled = _:_,
	       xref_called(_, NotCalled)
	   ;   NotCalled = M:G,
	       xref_called(ModFile, G),
	       xref_module(ModFile, M)
	   ;   xref_called(AutoImportFile, NotCalled),
	       \+ defined(AutoImportFile, NotCalled),
	       global_predicate(NotCalled)
	   ).
	   
%	xref_called(?Source, ?Callable) 
%	
%	True if Callable is called in   Source, after removing recursive
%	calls.

xref_called(Source, Callable) :-
	xref_called(Source, Callable, By),
	By \= Callable.			% recursive calls

%	defined(?File, ?Callable)
%	
%	True if Callable is defined in File and not imported.

defined(File, Callable) :-
	xref_defined(File, Callable, How),
	atom(File),
	How \= imported(_),
	How \= (multifile).

%	undefined(+File, -Callable)
%	
%	Callable is called in File, but no   definition can be found. If
%	File is not a module file we   consider other files that are not
%	module files.

undefined(File, Undef) :-
	xref_module(File, _), !,
	xref_called(File, Undef),
	\+ (  available(File, Undef, How),
	      How \== plain_file
	   ).
undefined(File, Undef) :-
	xref_called(File, Undef),
	\+ available(File, Undef, _).


		 /*******************************
		 *    IMPORT/EXPORT HEADERS	*
		 *******************************/

%	file_imports(+File, -Imports)
%	
%	Determine which modules must  be  imported   into  this  one. It
%	considers all called predicates that are   not covered by system
%	predicates. Next, we have three sources to resolve the remaining
%	predicates, which are tried in the   order below. The latter two
%	is dubious.
%	
%		* Already existing imports
%		* Imports from other files in the project
%		* Imports from the (autoload) library
%	
%	We first resolve all imports to   absolute  files. Localizing is
%	done afterwards.  Imports is a list of
%	
%		use_module(FileSpec, Callables)

xref_file_imports(FileSpec, Imports) :-
	canonical_filename(FileSpec, File),
	findall(Called, called_no_builtin(File, Called), Resolve0),
	resolve_old_imports(Resolve0, File, Resolve1, Imports0),
	find_new_imports(Resolve1, File, Imports1),
	disambiguate_imports(Imports1, File, Imports2),
	flatten([Imports0, Imports2], ImportList),
	keysort(ImportList, SortedByFile),
	merge_by_key(SortedByFile, ImportsByFile),
	maplist(make_import(File), ImportsByFile, Imports).
	
canonical_filename(FileSpec, File) :-
	absolute_file_name(FileSpec,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   File).

called_no_builtin(File, Callable) :-
	xref_called(File, Callable),
	\+ defined(File, Callable),
	\+ built_in_predicate(Callable).

resolve_old_imports([], _, [], []).
resolve_old_imports([H|T0], File, UnRes, [From-H|T]) :-
	xref_defined(File, H, imported(From)), !,
	resolve_old_imports(T0, File, UnRes, T).
resolve_old_imports([H|T0], File, [H|UnRes], Imports) :-
	resolve_old_imports(T0, File, UnRes, Imports).

find_new_imports([], _, []).
find_new_imports([H|T0], File, [FL-H|T]) :-
	findall(F, resolve(H, F), FL0),
	sort(FL0, FL),
	find_new_imports(T0, File, T).

disambiguate_imports(Imports0, File, Imports) :-
	ambiguous_imports(Imports0, Ambig, UnAmbig, _Undef),
	(   Ambig == []
	->  Imports = UnAmbig
	;   new(D, xref_disambiguate_import_dialog(File, Ambig)),
	    get(D, confirm_centered, Result),
	    (	Result == ok
	    ->  get(D, result, List),
		send(D, destroy),
		append(UnAmbig, List, Imports)
	    )
	).

ambiguous_imports([], [], [], []).
ambiguous_imports([[]-C|T0], Ambig, UnAmbig, [C|T]) :- !,
	ambiguous_imports(T0, Ambig, UnAmbig, T).
ambiguous_imports([[F]-C|T0], Ambig, [F-C|T], Undef) :- !,
	ambiguous_imports(T0, Ambig, T, Undef).
ambiguous_imports([A-C|T0], [A-C|T], UnAmbig, Undef) :-
	is_list(A), !,
	ambiguous_imports(T0, T, UnAmbig, Undef).


%	resolve(+Callable, -File)
%	
%	Try to find files from which to resolve Callable.

resolve(Callable, File) :-		% Export from module files
	xref_exported(File, Callable),
	atom(File).
resolve(Callable, File) :-		% Non-module files
	defined(File, Callable),
	atom(File),
	\+ xref_module(File, _).
resolve(Callable, File) :-		% The Prolog autoload library
	autoload_predicate(Callable, File).


%	merge_by_key(+KeyedList, -ListOfKey-Values)
%	
%	Example: [a-x, a-y, b-z] --> [a-[x,y], b-[z]]

merge_by_key([], []).
merge_by_key([K-V|T0], [K-[V|Vs]|T]) :-
	same_key(K, T0, Vs, T1),
	merge_by_key(T1, T).

same_key(K, [K-V|T0], [V|VT], T) :- !,
	same_key(K, T0, VT, T).
same_key(_, L, [], L).


%	make_import(+RefFile, +ImportList, -UseModules)
%	
%	Glues it all together to make a list of directives.

make_import(RefFile, File-Imports, (:-use_module(ShortPath, PIs))) :-
	local_filename(File, RefFile, ShortPath),
	sort_callables(Imports, SortedImports),
	maplist(predicate_indicator, SortedImports, PIs).

local_filename(File, RefFile, ShortPath) :-
	atom(RefFile), 
	file_directory_name(File, Dir),
	file_directory_name(RefFile, Dir), !,	% i.e. same dir
	file_base_name(File, Base),
	remove_extension(Base, ShortPath).
local_filename(File, _RefFile, ShortPath) :-
	short_file_name(File, ShortPath0),
	remove_extension(ShortPath0, ShortPath).


remove_extension(Term0, Term) :-
	Term0 =.. [Alias,ShortPath0],
	file_name_extension(ShortPath, pl, ShortPath0), !,
	Term  =.. [Alias,ShortPath].
remove_extension(ShortPath0, ShortPath) :-
	atom(ShortPath0),
	file_name_extension(ShortPath, pl, ShortPath0), !.
remove_extension(Path, Path).

:- pce_begin_class(xref_disambiguate_import_dialog, auto_sized_dialog,
		   "Prompt for alternative sources").

initialise(D, File:name, Ambig:prolog) :->
	send_super(D, initialise, string('Disambiguate calls for %s', File)),
	forall(member(Files-Callable, Ambig),
	       send(D, append_row, File, Callable, Files)),
	send(D, append, button(ok)),
	send(D, append, button(cancel)).

append_row(D, File:name, Callable:prolog, Files:prolog) :->
	send(D, append, xref_predicate_text(Callable, @default, File)),
	send(D, append, new(FM, menu(file, cycle)), right),
	send(FM, append, menu_item(@nil, @default, '-- Select --')),
	forall(member(Path, Files),
	       (   short_file_name(Path, ShortId),
		   short_file_name_to_atom(ShortId, Label),
		   send(FM, append, menu_item(Path, @default, Label))
	       )).

result(D, Disam:prolog) :<-
	"Get disambiguated files"::
	get_chain(D, graphicals, Grs),
	selected_files(Grs, Disam).

selected_files([], []).
selected_files([PreText,Menu|T0], [File-Callable|T]) :-
	send(PreText, instance_of, xref_predicate_text),
	send(Menu, instance_of, menu),
	get(Menu, selection, File),
	atom(File), !,
	get(PreText, callable, Callable),
	selected_files(T0, T).
selected_files([_|T0], T) :-
	selected_files(T0, T).


ok(D) :->
	send(D, return, ok).

cancel(D) :->
	send(D, destroy).

:- pce_end_class(xref_disambiguate_import_dialog).

%	xref_file_exports(+File, -Exports)
%	
%	Produce the export-header for non-module files.  Fails if the
%	file is already a module file.

xref_file_exports(FileSpec, (:- module(Module, Exports))) :-
	canonical_filename(FileSpec, File),
	\+ xref_module(File, _),
	findall(C, export_link_1(File, _, C), Cs),
	sort_callables(Cs, Sorted),
	file_base_name(File, Base),
	file_name_extension(Module, _, Base),
	maplist(predicate_indicator, Sorted, Exports).
