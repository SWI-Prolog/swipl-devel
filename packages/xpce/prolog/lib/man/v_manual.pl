/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(man_manual, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ checkpce/0
	   , pcedemo/0
	   , concat/3
	   , ignore/1
	   , pce_help_file/2
	   , send_list/3
	   ]).

:- use_module(library(pce_emacs)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			    OVERALL ARCHITECTURE

The following diagram  provides an overall  view of the  design of the
manual tools.

			 ManualTool
			     |
			     | (select)
			     |
			     V         	 | ClassBrowser
			   Tools         | ClassHierarchy
                             |  	 | TopicBrowser
			     |  	 | KeywordBrowser
			     |
			     | (find/browse)
			     V
		   [Type] Name [Summary]
		            /|\
       Examples----/-------- | -----------\
	          /          |             \
	         /           |	            \
             Sources      Textual          Relations
	        	Attributes    [Type] Name [Summary]


The communication between  the tools is arranged  via messages send to
and possible broadcasted by ManualTool.  These messages are:

    ->request_selection: man_frame, object*, [bool]
    	Set the <-selection and <-selection_holder attribute of the
	ManualTool and broadcasts the following messages:

		* SelectionHolder ->release_selection
		* AllTools        ->selected: object*
	
	If bool == @on, the card viewer is started automatically

    ->tool_focus: object*
        Set the focus of all tools.  Broadcasted to all tools.

    ->relate: object
        Request manual to relate selection to object.

    ->edit_mode: bool
        Switch edit_mode on/off.  Broadcasted to all tools.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(man_manual, frame,
		   "PCE manual main object").

resource(geometry,	geometry,		'+0+0').

variable(selection,		object*,	get,
	 "Currently selected object").
variable(selection_holder,	man_frame*,	get,
	 "Tool holding selection").
variable(tool_focus,		object*,	get,
	 "Arg of last ->tool_focus").
variable(tools,			sheet,		get,
	 "Tool-name --> tool mapping").
variable(edit_mode,		bool,		get,
	 "Can database be edited?").
variable(space,			man_space,	get,
	 "Manual module collection").
variable(focus_history,		chain,		get,
	 "Chain of focused cards").
variable(selection_history,	chain,		get,
	 "Chain of selected cards").
variable(maintainer,		bool,		get,
	 "Indicates the user is a maintainer").
variable(exit_message,		code*,		get,
	 "Message called on exit").


		/********************************
		*            CREATE		*
		********************************/

initialise(M, Dir:[directory]) :->
	"Create the manual main object"::
	send(M, send_super, initialise, 'PCE Manual'),
	default(Dir, directory('$PCEHOME/man/reference'), Directory),
	send(M, check_directory, Directory),
	send(M, slot, space, new(Space, man_space(reference, Directory))),
	send(M, slot, tools, new(sheet)),
	send(M, slot, edit_mode, @off),
	send(M, slot, focus_history, new(chain)),
	send(M, slot, selection_history, new(chain)),

	send(Space, attribute, attribute(report_to, M)),
	send(M, append, new(D, dialog)),
	fill_dialog(D),

	ifmaintainer((
	  send(@pce, exit_message,
	       new(Msg,
		   if(M?space?modified == @on,
		      and(message(@pce, confirm,
				  'Manual database has changed.  Save?'),
			  message(M?space, save_some))))),
          send(M, slot, exit_message, Msg))),

	send(M, report, status, 'For help, see `File'' menu').

unlink(M) :->
	"Manual is destroyed"::
	get(M, exit_message, Msg),
	ignore(send(@pce?exit_messages, delete, Msg)),
	send(M, send_super, unlink).


check_directory(M, Dir:directory) :->
	"Check the manual directory"::
	(   send(Dir, exists)
	->  true
	;   send(@display, inform,
		 'Cannot find manual directory %s\n%s',
		 Dir?path,
		 'Please touch `.../xpce/build'' and reinstall')
	),
	(   send(Dir, access, write)
	->  send(M, slot, maintainer, @on)
	;   send(M, slot, maintainer, @off)
	).


fill_dialog(D) :-
	new(M, D?frame),
	send(D, append, new(MB, menu_bar(options))),
	send(MB, append, new(F, popup(file))),
	send(MB, append, new(V, popup(browsers,
				      message(M, start_tool, @arg1)))),
	send(MB, append, new(T, popup(tools,
				      message(M, start_tool, @arg1)))),
	send(MB, append, new(H, popup(history))),

	/* FILE menu */

	send_list(F, append,
	     [ menu_item(about,
			 message(M, about))
	     , menu_item(help,
			 message(M, help))
	     , menu_item(mailing_list,
			 message(M, mailing_list))
	     , menu_item(printed_copy,
			 message(M, printed_copy),
			 end_group := @on)
	     , menu_item(demo_programs,
			 message(M, start_demo),
			 @default, @on)
	     , menu_item('ChangeLog',
			 message(M, changelog))
	     , menu_item('FAQ',
			 message(M, faq),
			 @default, @on)
	     ]),

	(    get(M, maintainer, @on)
	->   send_list(F, append,
		       [ menu_item(edit_mode,
				   message(M, toggle_edit_mode))
		       , menu_item(list_modules,
				   message(M, list_modules))
		       , menu_item(list_all_modules,
				   message(M, list_all_modules))
		       , menu_item(save_manual,
				   message(M?space, save_some),
				   @default, @on,
				   M?space?modified == @on)
		       ])
	;    true
	),
	send_list(F, append,
	     [ menu_item(quit,
			 message(M, quit))
	     , menu_item(quit_pce,
			 message(M, quit_pce))
	     ]),


	/* BROWSERS menu */

	send_list(V, append,
	     [ menu_item(manual_tools,      @default, @default, @on)
	     , menu_item(class_hierarchy,   @default, @default, @off)
	     , menu_item(class_browser,     @default, @default, @off)
	     , menu_item(global_objects,    @default, @default, @off)
	     , menu_item(errors,    	    @default, @default, @off)
	     , menu_item(prolog_predicates, @default, @default, @on)
	     , menu_item(keywords,	    @default, @default, @off)
	     , menu_item(group_overview,    @default, @default, @off)
	     , menu_item(examples,	    @default, @default, @on)
/*
	     , menu_item(topics,            @default, @default, @on)
	     , menu_item(changes,	    @default, @default, @off)
	     , menu_item(bug_fixes,   	    @default, @default, @on)
*/
	     ]),
	(    get(M, maintainer, @on)
	->   send_list(V, append,
	     [ menu_item(class_finder,	    @default, @default, @off)
	     ])
	;    true
	),

	/* TOOLS menu */

	send_list(T, append,
	     [ statistics
	     , inspector
	     , menu_item(visual_hierarchy,
			 end_group := @on)
	     , menu_item(dialog_editor,
			 message(M, dialog_editor))
	     , menu_item(emacs,
			 message(M, start_emacs),
			 end_group := @on)
	     , menu_item(check_object_base,
			 message(M, check_object_base))
	     ]),

	/* HISTORY menu */

	new(SI, menu_item(selection, @nil, @default, @off,
			  not(message(M?selection_history, empty)))),
	new(FI, menu_item(focus, @nil, @default, @off,
			  not(message(M?focus_history, empty)))),
	send(SI, popup,
	     new(SH, popup(selection, message(M, select_history_menu,
					      selection_history, @arg1)))),
	send(FI, popup,
	     new(FH, popup(focus, message(M, select_history_menu,
					  focus_history, @arg1)))),

	send(SH, update_message, message(M, update_history_menu,
					 selection_history, @receiver)),
	send(FH, update_message, message(M, update_history_menu,
					 focus_history, @receiver)),
	send(H, append, SI),
	send(H, append, FI),

	send(D, append, new(label)).


		/********************************
		*         STARTING TOOLS	*
		********************************/

start_tool(M, ToolName:name) :->
	"Start named tool"::
	(   get(M?tools, value, ToolName, Tool)
	->  send(Tool, expose)
	;   create_tool(M, ToolName, Tool),
	    send(Tool, open)
	->  send(M, register_tool, ToolName, Tool)
	;   send(@display, inform, 'Failed to start %s', ToolName)
	).
	

register_tool(M, Name:name, Tool:man_frame) :->
	"Register frame as a menual tool"::
	send(Tool, slot, tool_name, Name),
	send(M?tools, append, attribute(Name, Tool)).


expose_tool(M, ToolName:name) :->
	"Expose named tool"::
	get(M?tools, value, ToolName, Tool),
	send(Tool, expose).


create_tool(M, Name, Tool) :-
	tool_class(Name, M, Term),
	new(Tool, Term).

tool_class(class_browser,	M, man_class_browser(M)).
tool_class(class_finder,	M, man_class_browser(M)).
tool_class(class_hierarchy,	M, man_class_hierarchy(M)).
tool_class(keywords,		M, man_keyword_browser(M)).
tool_class(topics,		M, man_topic_browser(M)).
tool_class(card_viewer,		M, man_card_editor(M)).
tool_class(statistics,		M, man_statistics(M)).
tool_class(inspector,		M, isp_frame(M)).
tool_class(visual_hierarchy,	M, vis_frame(M)).
tool_class(global_objects,	M, man_object_browser(M)).
tool_class(errors,		M, man_error_browser(M)).
tool_class(manual_tools,	M,
	   man_module_browser(M, tools, man_browser_card, 'Manual Tools')).
tool_class(prolog_predicates,   M,
	   man_module_browser(M, predicates,
			      man_predicate_card, 'Prolog Predicates')).
tool_class(examples,		M,
	   man_module_browser(M, examples, man_example_card, 'PCE Examples')).
tool_class(changes,		M,
	   man_module_browser(M, changes, man_change_card, 'PCE Changes')).
tool_class(bug_fixes,		M,
	   man_module_browser(M, bug_fixes, man_bug_card, 'PCE Bug Fixes')).
tool_class(group_overview,	M,
	   man_group_browser(M, groups, 'Group Browser')).


		/********************************
		*          DESTROYING		*
		********************************/

destroy_tool(M, Tool:man_frame) :->
	"Destroy a tool"::
	(   get(M, selection_holder, Tool)
	->  ignore(send(Tool, release_selection)),	% TBD: forward
	    send(M, slot, selection_holder, @nil)
	;   true
	),
	send(M?tools, for_all,
	     if(@arg1?value == Tool,
		message(M?tools, delete, @arg1?name))),
	send(Tool, destroy).


quit(M) :->
	"Quit Manual Tool"::
	save_if_modified(M),
	send(@display, confirm, 'Quit all manual tools?'),
	send(M?tools, for_all, message(@arg1?value, quit)),
	send(M, destroy).


quit_pce(M) :->
	"Exit from PCE process"::
	save_if_modified(M),
	send(@display, confirm, 'Really exit PCE?'),
	send(@pce, die).


save_if_modified(M) :-
	(   get(M?space, modified, @on)
	->  send(@display, confirm, 'Manual Database is modified. Save?'),
	    send(M?space, save_some)
	;   true
	).

		/********************************
		*           MANUAL DATA		*
		********************************/

module(M, Name:name, Create:[bool], Module) :<-
	"Find/create manual module"::
	get(M, space, Space),
	(   send(Space, ensure_loaded, Name)
	->  get(Space, module, Name, Module)
	;   Create == @on
	->  new(Module, man_module(Space, Name))
	;   fail
	).


list_modules(M) :->
	"List associated modules"::
	new(V, view('Loaded Modules')),
	new(D, dialog),
	send(D, append, button(quit, message(D?frame, free))),
	send(D, below, V),
	send(V, tab_stops, vector(200)),
	send(V, font, font(helvetica, roman, 12)),
	send(V, format, '%s\t%s\n\n', 'Module Name', 'Number of Cards'),
	new(NM, number(0)),
	new(NC, number(0)),
	send(M?space?modules, for_all,
	     block(message(NM, plus, 1),
		   message(NC, plus, @arg2?id_table?size),
		   message(V, format, '%s\t%s\n',
			   @arg2?name, @arg2?id_table?size))),
	send(V, caret, 0),
	send(V, format, '%d cards in %d modules\n\n', NC, NM),
	send(V, caret, 0),
	send(V, open).

list_all_modules(M) :->
	"Load and list all modules from the directory"::
	send(M?space, load_all_modules),
	send(M, list_modules).


		 /*******************************
		 *	    VIEW FILES		*
		 *******************************/

changelog(_M) :->
	"View ChangeLog"::
	get(@pce, home, Home),
	get(string('%s/ChangeLog', Home), value, Path),
	send(@emacs, goto_source_location, Path).

faq(_M) :->
	"Start @helper on faq-database"::
	get(@pce, home, Home),
	concat(Home, '/man/faq/pce.hlp', HelpFile),
	pce_help_file(pce_faq, HelpFile),
	send(@helper, give_help, pce_faq, main).


		/********************************
		*              HELP		*
		********************************/

about(_M) :->
	send(@display, inform,
	     'PCE version %s\n\nCopyright 1992-1994, University of Amsterdam\n\ninfo: jan@swi.psy.uva.nl\n\n',
	     @pce?version).


help(M) :->
	"Give help on the overall manual"::
	give_help(M, @nil, manual).

printed_copy(_M) :->
	"Where to obtain printed copies"::
	send(@display, inform,
	     '%s\n%s\n%s\n',
	     'TeX generated PostScript of the printed Reference Manual',
	     'Is available using anonymous ftp to swi.psy.uva.nl',
	     '(145.18.114.17), directory pub/xpce/doc/refman').

mailing_list(M) :->
	"(Un)Subscribe on the XPCE mailing lists"::
	new(D, dialog('Mailing list')),
	send(D, append,
	     label(reporter,
		   '(Un)Subscribe to mailing list xpce@swi.psy.uva.nl')),
	send(D, append,
	     new(A, text_item(your_mail_address, string('%s@', @pce?user)))),
	send(D, append,
	     button(subscribe,
		    message(D, return,
			    create(tuple, subscribe, A?selection)))),
	send(D, append,
	     button(unsubscribe,
		    message(D, return,
			    create(tuple, unsubscribe, A?selection)))),
	send(D, append,
	     button(cancel, message(D, return, @nil))),
	get(D, confirm_centered, Answer),
	send(D, destroy),
	(   Answer \== @nil
	->  object(Answer, tuple(Op, Address)),
	    new(P, process(mail, '-s', Op,
			   'xpce-request@swi.psy.uva.nl')),
	    send(P, use_tty, @off),
	    send(P, open),
	    send(P, format, 'Please %s %s\n', Op, Address),
	    send(P, close),
	    repeat,
	    (	get(P, read_line, Out),
		send(M, report, progress, Out)
	    ->  fail
	    ;	!,
	        get(P, code, Code),
		(   Code == 0
		->  send(M, report, status,
			 'Request sent to xpce-request@swi.psy.uva.nl')
		;   send(M, report, error,
			 '%N failed with status %s', P, Code)
		)
	    )
	;   true
	).
	

		/********************************
		*              DEMO		*
		********************************/

start_demo(M) :->
	send(M, report, progress, 'Starting demo tool ...'),
	ensure_loaded(library(pce_demo)),
	pcedemo,
	send(M, report, done).


		/********************************
		*            CHECKING		*
		********************************/

check_object_base(_M) :->
	(   checkpce
	->  send(@display, inform, 'Object base is consistent')
	;   send(@display, inform, '%s\n%s',
		 'Object base is corrupted',
		 'See Prolog window for details')
	).


		 /*******************************
		 *     START EXTERNAL TOOLS	*
		 *******************************/

dialog_editor(_M) :->
	"Start the dialog editor"::
	use_module(library(dialog)),
	send(@prolog, dialog).		% avoid pce_require to find this


start_emacs(_M) :->
	"Start PceEmacs (*scratch* buffer)"::
	emacs.


		/********************************
		*            INSPECTOR		*
		********************************/

inspect(M, V:object) :->
	"Start inspector on object"::
	send(M, start_tool, inspector),
	send(M?tools?inspector, inspect, V).


		 /*******************************
		 *	 EXTERNAL INVOKES	*
		 *******************************/

manual(M, Object:'class|variable|method|resource') :->
	"Open manual on object"::
	send(M, open),
	(   send(Object, instance_of, class)
	->  send(M, start_tool, class_browser),
	    send(M, request_tool_focus, Object)
	;   send(M, request_selection, @nil, Object, @on)
	).


		/********************************
		*         COMMUNICATION		*
		********************************/

request_selection(M, Frame:man_frame*, Obj:any*, Open:[bool]) :->
	"Request to become selection holder"::
	get(M, selection_holder, OldHolder),
	(   OldHolder \== @nil
	->  (   send(OldHolder, release_selection)
	    ->  true
	    ;   send(@display, inform,
		     '%s does not release selection', OldHolder)
	    )
	;   true
	),
	send(M, slot, selection_holder, Frame),
	send(M, slot, selection, Obj),
	send(M, update_history, selection_history, Obj),
	send(M?tools, for_some, message(@arg1?value, selected, Obj)),
	(   send(Obj, instance_of, man_keyword_card)
	->  true
	;   (   \+ get(M?tools, value, card_viewer, _)
	    ->  (   Open == @on
		->  send(M, report, status, 'Starting Card Viewer ...'),
		    send(M, start_tool, card_viewer),
		    send(M, report, status, done)
		;   true
		)
	    ;   send(M, expose_tool, card_viewer)  % exposes it?
	    )
	).
	

request_tool_focus(M, Obj:object*) :->
	"Change the tool focus"::
	send(M, slot, tool_focus, Obj),
	send(M, update_history, focus_history, Obj),
	send(M?tools, for_some, message(@arg1?value, tool_focus, Obj)),
	(   send(Obj, instance_of, class),
	    \+ get(M?tools, value, class_browser, _),
	    send(@display, confirm, 'Start Class Browser?')
	->  send(M, start_tool, class_browser)
	;   send(M, expose_tool, class_browser)	  % exposes it!
	).


maintainer(M, Val:bool) :->
	"Switch maintainer-mode on/off"::
	send(M, slot, maintainer, Val),
	send(M?tools, for_some, message(@arg1?value, maintainer, Val)).


		/********************************
		*             HISTORY		*
		********************************/

update_history(M, History:name, Obj:object*) :->
	"Add object to the requested history"::
	get(M, History, Chain),
	(   get(Chain, head, Obj)
	->  true
	;   ignore(send(Chain, delete, Obj)),
	    send(Chain, prepend, Obj),
	    (   get(Chain, size, S),
	        S > 10
	    ->  send(Chain, delete_tail)
	    ;   true
	    )
	).


update_history_menu(M, History, Menu) :->
	"Update the contents of the history popup"::
	get(M, History, Chain),
	send(Menu, clear),
	send(Chain, for_some,
	     message(Menu, append,
		     create(menu_item,
			    @arg1, @default,
			    when(message(@arg1, instance_of, chain),
				 ?(@pce, instance, string, 'G %s:%s',
				   when(message(@arg1?head, instance_of,
						class),
					@arg1?head?name,
					@arg1?head?context?name),
				   @arg1?head?group),
				 progn(assign(new(X, var),
					      create(string, '%s',
						     @arg1?man_name)),
				       message(X, translate, '\t', ' '),
				       X))))).


select_history_menu(M, History:name, Obj) :->
	"Trap selected history item"::
	(   History == selection_history
	->  send(M, request_selection, @nil, Obj, @on)
	;   send(M, request_tool_focus, Obj)
	).


		/********************************
		*           (UN)RELATE		*
		********************************/

request_relate(M, Obj:object) :->
	"Relate selection to object"::
	request_relate(M, relate, Obj).

request_unrelate(M, Obj:object) :->
	"Destroy relation to selection"::
	request_relate(M, unrelate, Obj).
	
request_relate(M, CD, Obj) :-
	(   get(M, edit_mode, @on)
	->  (   get(M, selection, Selection),
		Selection \== @nil
	    ->  get(Selection, class_name, SClass),
	        get(Obj, class_name, OClass),
		relate(M, SClass-OClass, CD, Selection, Obj)
	    ;   send(@display, inform, 'First make a selection')
	    )
	;   send(@display, inform, 'Manual is in read-only mode')
	).

relate(_, _-_, create, Obj, Obj) :- !,
	send(@display, inform, 'Can''t relate %s to itself', Obj?man_name).
relate(M, man_keyword_card-man_keyword_card, CD, Kwd1, Kwd2) :- !,
	send(@display, confirm,
	     '%s %s <-> %s', CD, Kwd1?name, Kwd2?name),
	send(M, create_relation, CD, Kwd1, see_also, Kwd2),
	send(M, create_relation, CD, Kwd2, see_also, Kwd1).
relate(M, _-man_keyword_card, CD, Selection, Kwd) :- !,
	send(@display, confirm,
	     '%s %s -> %s', CD, Kwd?name, Selection?man_name),
	send(M, create_relation, CD, Kwd, see_also, Selection).
relate(M, man_keyword_card-_, CD, Kwd, Object) :- !,
	send(@display, confirm,
	     '%s %s -> %s', CD, Kwd?name, Object?man_name),
	send(M, create_relation, CD, Kwd, see_also, Object).
relate(M, _-_, CD, Selection, Obj) :-
	send(@display, confirm,
	     '%s %s <-> %s', CD, Selection?man_name, Obj?man_name),
	send(M, create_relation, CD, Selection, see_also, Obj),
	send(M, create_relation, CD, Obj, see_also, Selection).


create_relation(M, CD, From, Rel, To) :->
	(   CD == relate
	->  send(From, man_relate, Rel, To),
	    send(M?tools, for_some,
		 message(@arg1?value, related, From, Rel, To))
	;   CD == unrelate
	->  send(From, man_unrelate, Rel, To),
	    send(M?tools, for_some,
		 message(@arg1?value, unrelated, From, Rel, To))
	).


		 /*******************************
		 *	    (UN)INHERIT		*
		 *******************************/

request_inherit(M, Obj:object) :->
	"Relate selection to object"::
	request_inherit(M, relate, Obj).

request_uninherit(M, Obj:object) :->
	"Destroy relation to selection"::
	request_inherit(M, unrelate, Obj).
	
request_inherit(M, CD, Obj) :-
	(   get(M, edit_mode, @on)
	->  (   get(M, selection, Selection),
		Selection \== @nil
	    ->  inherit(M, CD, Selection, Obj)
	    ;   send(@display, inform, 'First make a selection')
	    )
	;   send(@display, inform, 'Manual is in read-only mode')
	).

inherit(_, create, Obj, Obj) :- !,
	send(@display, inform, 'Can''t inherit %s from myself', Obj?man_name).
inherit(M, CD, Selection, Obj) :-
	send(@display, confirm,
	     '%s description of %s from %s',
	     when(CD == relate, 'Inherit', 'UnInherit'),
	     Obj?man_name, Selection?man_name),
	send(M, create_relation, CD, Obj, inherit, Selection),
	send(@man_description_cache, clear),
	send(@man_source_cache, clear).


		/********************************
		*            SOURCES		*
		********************************/

request_source(_M, Obj:object) :->
	"Display source of object"::
	(   get(Obj, source, Location)
	->  send(@emacs, goto_source_location, Location)
	;   send(@display, inform, 'Can''t find source')
	).


		/********************************
		*          EDIT MODE		*
		********************************/

edit_mode(M, Val) :->
	"Set overall edit_mode"::
	send(M, slot, edit_mode, Val),
	send(M?tools, for_some, message(@arg1?value, edit_mode, Val)).


toggle_edit_mode(M) :->
	"Toggle current setting of edit_mode"::
	(   get(M, edit_mode, @off)
	->  send(M, edit_mode, @on)
	;   send(M, edit_mode, @off)
	),
	get(M, edit_mode, @Val),
	send(M, report, status, 'Edit mode is now %s', Val).

:- pce_end_class.


		/********************************
		*          TOOL FRAMES		*
		********************************/

:- pce_begin_class(man_frame(label), frame).

variable(manual,	man_manual,	get,
	 "Manual we are related to").
variable(tool_name,	name,		get,
	 "Name of the tool in this frame").


initialise(F, Manual:man_manual, Label:[name]) :->
	"Create from label"::
	send(F, send_super, initialise, Label),
	send(F, slot, manual, Manual),
	send(F, done_message, message(F, quit)).


tool_focus(_F, _Focus:object*) :->
	"Generic operation: fail"::
	fail.


selected(_F, _Obj:object*) :->
	"Generic operation: fail"::
	fail.


release_selection(_F) :->
	"Generic operation: true"::
	true.


edit_mode(_F, _Val:bool) :->
	"Generic operation: fail"::
	fail.


related(_F, _From:object, _Rel:name, _To:object) :->
	"Generic operation: fail"::
	fail.


unrelated(_F, _From:object, _Rel:name, _To:object) :->
	"Generic operation: fail"::
	fail.


quit(F) :->
	"Destroy a tool"::
	send(F?manual, destroy_tool, F).


		/********************************
		*      GENERIC USER ACTIONS	*
		********************************/

request_selection(F, Obj:any*, Open:[bool]) :->
	send(F?manual, request_selection, F, Obj, Open).

request_tool_focus(F, Obj:object) :->
	send(F?manual, request_tool_focus, Obj).

request_source(F, Obj:object) :->
	send(F?manual, request_source, Obj).

request_relate(F, Obj:object) :->
	send(F?manual, request_relate, Obj).

request_unrelate(F, Obj:object) :->
	send(F?manual, request_unrelate, Obj).

request_inherit(F, Obj:object) :->
	send(F?manual, request_inherit, Obj).

request_uninherit(F, Obj:object) :->
	send(F?manual, request_uninherit, Obj).

help(F) :->
	"Give help on a manual tool"::
	get(F, manual, Manual),
	get(F, tool_name, ToolName),
	give_help(Manual, F, ToolName).

:- pce_end_class.

		/********************************
		*              HELP		*
		********************************/

give_help(Manual, Frame, ToolName) :-
	get(Manual, module, tools, @on, Tools),
	(   get(Tools?id_table, find_value, @arg2?tool_name == ToolName, Card)
	->  send(Manual, request_selection, Frame, Card, @on)
	;   get(Manual, edit_mode, @on),
	    get(Manual, selection, ToolCard),
	    ToolCard \== @nil,
	    send(ToolCard, instance_of, man_browser_card),
	    send(@display, confirm, 'Assign %s to browser %s',
		 ToolCard?man_name, ToolName)
	->  send(ToolCard, store, tool_name, ToolName)
	;   send(@display, inform, 'Sorry, Can''t find help card ...')
	).

