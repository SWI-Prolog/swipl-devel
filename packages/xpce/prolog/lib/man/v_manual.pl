/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(man_manual, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ absolute_file_name/3
	   , auto_call/1
	   , default/3
	   , forall/2
	   , ignore/1
	   , pce_help_file/2
	   , send_list/3
	   ]).


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

class_variable(geometry,	geometry,		'+0+0').
class_variable(user_scope,	chain,			chain(basic, user),
	 "Default scoping of manual material").
class_variable(edit,		bool,			@off).

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
variable(user_scope,		chain,		get,
	 "Types in user's scope").
variable(search_patterns,	chain*,		both,
	 "Search patterns to be highlighted").


		/********************************
		*            CREATE		*
		********************************/

initialise(M, Dir:[directory]) :->
	"Create the manual main object"::
	send(M, send_super, initialise, 'PCE Manual'),
	send(M, can_resize, @off),
	send(M, done_message, message(M, quit)),
	get(M, class_variable_value, user_scope, Scope),
	get(M, class_variable_value, edit, Edit),
	send(M, slot, maintainer, Edit),
	default(Dir, directory('$PCEHOME/man/reference'), Directory),
	send(M, check_directory, Directory),
	send(M, slot, space, new(Space, man_space(reference, Directory))),
	send(M, slot, tools, new(sheet)),
	send(M, slot, edit_mode, @off),
	send(M, slot, focus_history, new(chain)),
	send(M, slot, selection_history, new(chain)),
	send(M, slot, user_scope, Scope),

	send(Space, attribute, attribute(report_to, M)),
	send(M, append, new(D, dialog)),
	send(M, fill_dialog, D),

	ifmaintainer((
	  send(@pce, exit_message, new(Msg, message(M, save_if_modified))),
          send(M, slot, exit_message, Msg))),

	send(M, check_runtime),
	send(M, report, status, 'For help, see `File'' menu').


open(M, Pos:[point]) :->
	send(M, send_super, open, Pos),
	send(M, check_licence).


unlink(M) :->
	"Manual is destroyed"::
	get(M, space, Space),
	send(Space, delete_attribute, report_to),
	get(M, exit_message, Msg),
	ignore(send(@pce?exit_messages, delete, Msg)),
	send(M, send_super, unlink).


check_directory(M, Dir:directory) :->
	"Check the manual directory"::
	(   send(Dir, exists)
	->  true
	;   send(M, report, error, 'Cannot find manual directory %s', Dir?path)
	).


check_runtime(_M) :->
	"Check for runtime system"::
	(   get(@pce, is_runtime_system, @on)
	->  send(@display, inform,
		 '%s.  %s\n%s %s',
		 'This is a runtime version of XPCE',
		 'Most of the manual will not work.',
		 'Contact xpce-request@swi.psy.uva.nl',
		 'for a information on the development version')
	;   true
	).


check_licence(M) :->
	"Open about box if not licenced"::
	(   get(@pce, attribute, unlicenced_copy, @on)
	->  send(M, about)
	;   true
	).


fill_dialog(M, D) :->
	send(D, gap, size(5, 8)),
	send(D, append, new(MB, menu_bar)),
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
				   message(M, save_if_modified, @off),
				   @default, @on,
				   M?modified == @on)
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
	     [ menu_item(manual_tools,      end_group := @on)
	     , menu_item(class_hierarchy)
	     , menu_item(class_browser)
	     , menu_item(global_objects)
	     , menu_item(errors,    	    end_group := @on)
	     , menu_item(prolog_predicates)
	     , menu_item(library_overview,
			 message(M, library_overview),
			 end_group := @on)
	     , menu_item(search)
	     , menu_item(group_overview)
	     , menu_item(examples,	    end_group := @on)
	     ]),
	(    get(M, maintainer, @on)
	->   send_list(V, append,
	     [ menu_item(class_finder,	    end_group := @off)
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
tool_class(search,		M, man_search_tool(M)).
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
	send(M, save_if_modified),
%	send(@display, confirm, 'Quit all manual tools?'),
	send(M?tools, for_all, message(@arg1?value, quit)),
	send(M, destroy).


quit_pce(M) :->
	"Exit from PCE process"::
	send(M, save_if_modified),
	send(@display, confirm, 'Really exit PCE?'),
	send(@pce, die).


		 /*******************************
		 *	   SAVE/MODIFIED	*
		 *******************************/

modified(M, Modified:bool) :<-
	"See if manual database has been modified"::
	(   (   get(M?space, modified, @on)
	    ;   object(@man_classification),
		get(@man_classification, modified, @on)
	    )
	->  Modified = @on
	;   Modified = @off
	).
	

save_if_modified(M, Ask:[bool]) :->
	"Save if some part has been modified"::
	(   get(M, modified, @on)
	->  (   Ask \== @on
	    ;   send(@display, confirm, 'Manual Database is modified. Save?')
	    ), !,
	    send(M?space, save_some),
	    ClassifyTab = @man_classification,
	    (	object(ClassifyTab),
		get(ClassifyTab, modified, @on)
	    ->	send(M, report, progress,
		     'Saving %s ...', ClassifyTab?file?base_name),
		send(ClassifyTab?file, backup),
	        send(ClassifyTab, save_in_file, ClassifyTab?file),
		send(ClassifyTab, modified, @off),
		send(M, report, done)
	    ;	true
	    )
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
	auto_call(start_emacs),
	send(@emacs, goto_source_location, Path).

:- pce_help_file(pce_faq,     pce_help('pcefaq.hlp')).
:- pce_help_file(pce_library, pce_help('lib/overview.hlp')).

faq(_M) :->
	"Start @helper on faq-database"::
	send(@helper, give_help, pce_faq, main).

library_overview(_M) :->
	"Hypertext tool for library overview"::
	send(@helper, give_help, pce_library, main).


		/********************************
		*	   ABOUT/LICENCE	*
		********************************/

pce_ifhostproperty(prolog(quintus),
(   about(Fmt, Font) :- pce_host:about(Fmt, Font)),
[ about('XPCE version %s'+[@pce?version], boldhuge),
  about('Copyright 1992-1998, University of Amsterdam', normal),
  about('xpce-request@swi.psy.uva.nl', bold),
  about('Jan Wielemaker\nAnjo Anjewierden', italic),
  about('SWI\nUniversity of Amsterdam\nRoetersstraat 15\n1018 WB  Amsterdam\nThe Netherlands', normal),
  (about('Licenced to "%s"'+[Holder], bold) :-
	get(@pce, attribute, licence_holder, Holder)),
  (about('Licence will expire in %d days'+[Days], normal) :-
	get(@pce, attribute, days_to_expiration, Days)),
  (about('Unregistered copy\nXPCE will CRASH after about 20 minutes', bold) :-
	get(@pce, attribute, unlicenced_copy, @on))
]).

about(M) :->
	"Print about and licence info"::
	new(D, dialog('About XPCE')),
	send(D, transient_for, M),
	forall(about(Txt, Font),
	       (   (   Txt = Fmt+Args
		   ->  Term =.. [string, Fmt | Args]
		   ;   Term = string(Txt)
		   ),
		   send(D, append, new(T, text(Term, center, Font))),
		   send(T, alignment, center)
	       )),
	send(D, append, button(ok, message(D, destroy))),
	send(D, open_centered).

		 /*******************************
		 *	       HELP		*
		 *******************************/

help(M) :->
	"Give help on the overall manual"::
	give_help(M, @nil, manual).


		/********************************
		*              DEMO		*
		********************************/

start_demo(M) :->
	send(M, report, progress, 'Starting demo tool ...'),
	ensure_loaded(demo(pce_demo)),
	Goal = pcedemo, Goal,		% fool require generation
	send(M, report, done).


		/********************************
		*            CHECKING		*
		********************************/

check_object_base(_M) :->
	(   auto_call(checkpce)
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
	auto_call(dialog).


start_emacs(_M) :->
	"Start PceEmacs (*scratch* buffer)"::
	auto_call(emacs).


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

manual(M, Object:'class|behaviour|object') :->
	"Open manual on object"::
	send(M, open),
	(   send(Object, instance_of, class)
	->  send(M, start_tool, class_browser),
	    send(M, request_tool_focus, Object)
	;   (   send(Object, instance_of, behaviour)
	    ;	send(Object, instance_of, man_global)
	    )
	->  send(M, request_selection, @nil, Object, @on)
	;   Object = @Ref,
	    atom(Ref)
	->  send(M, request_selection, @nil, man_global(Ref), @on)
	;   send(M, report, error, 'Cannot start manual from %O', Object),
	    fail
	).


		 /*******************************
		 *	    USER-SCOPING	*
		 *******************************/

:- pce_global(@man_classification, load_man_classification).

load_man_classification(C) :-
	absolute_file_name(library('man/classification.obj'),
			   [access(read)], FileName),
	new(F, file(FileName)),
	get(F, object, C),
	send(C, attribute, file, file(F?absolute_path)),
	send(C, attribute, modified, @off).


in_scope(M, Obj:object) :->
	"Test if object is in current scope"::
	get(M, user_scope, Scope),
	get(Obj, man_id, Id),
	(   (   get(@man_classification, member, Id, Type)
	    ->	send(Scope, member, Type)
	    ;	send(Scope, member, obscure)
	    )
	;   get(Obj, man_creator, Creator),
	    Creator \== built_in,
	    send(Scope, member, user)
	).


user_scope(M, Scope:chain) :->
	"Modify scope and inform tools"::
	(   send(M?user_scope, equal, Scope)
	->  true
	;   send(M, slot, user_scope, Scope),
	    send(M?tools, for_some,
		 message(@arg1?value, user_scope, Scope))
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
	(   \+ get(M?tools, value, card_viewer, _)
	->  (   Open == @on
	    ->  send(M, report, progress, 'Starting Card Viewer ...'),
		send(M, start_tool, card_viewer),
		send(M, report, done)
	    ;   true
	    )
	;   send(M, expose_tool, card_viewer)  % exposes it?
	).
	

request_tool_focus(M, Obj:object*) :->
	"Change the tool focus"::
	send(M, slot, tool_focus, Obj),
	send(M, update_history, focus_history, Obj),
	send(M?tools, for_some, message(@arg1?value, tool_focus, Obj)),
	(   send(Obj, instance_of, class),
	    \+ get(M?tools, value, class_browser, _)
	->  send(M, report, progress, 'Starting Class Browser'),
	    send(M, start_tool, class_browser),
	    send(M, report, done)
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
	->  auto_call(start_emacs),
	    send(@emacs, goto_source_location, Location)
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


user_scope(_F, _Scope:chain) :->
	"Generic operation: fail"::
	fail.


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

