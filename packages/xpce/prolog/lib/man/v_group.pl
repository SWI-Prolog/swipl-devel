/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(man_group_browser, []).

:- use_module(library(pce)).
:- use_module(util).

:- pce_begin_class(man_group_browser(module_name), man_frame).

variable(module,	man_module*,	get,	"Group module").

initialise(MB, Manual:man_manual, ModuleName:[name], Label:[name]) :->
	"Create from Manual"::
	default(ModuleName, groups, MN),
	default(Label, 'Group Browser', Lbl),

	send(MB, send_super, initialise, Manual, Lbl),

	send(MB, append, new(V, view('', size(50, 20)))),
	get(V, image, Image),
	send(V, font, font(helvetica, roman, 12)),
	send(Image, tab_stops, vector(100)),
	send(Image, wrap, none),
	send(V, key_binding,
	     '\C-x\C-s',	message(MB, save_if_modified)),
	send(V, key_binding,
	     '\C-cRET',		message(MB, load_manual_groups)),
	send(V, exact_case, @off),

	send(V?editor, send_method,
	     send_method(insert_self, vector('[int]', '[char]'),
			 if(@receiver?editable == @on,
			    message(@receiver, send_class, insert_self,
				    @arg1, @arg2),
			    and(message(@receiver, isearch_forward),
				message(@receiver, '_start_isearch', @arg2))))),
	send(V?editor, send_method,
	     send_method('_isearch', vector(event_id),
			 if(@arg1 == 13,
			    and(message(@receiver, caret,
					@receiver?selection_end),
				message(@receiver, select_line, newline := @on),
				message(@receiver, flush),
				if(message(@receiver?frame,
					   show_group_members)),
				message(@receiver, focus_function, @nil)),
			    message(@receiver, send_class, '_isearch',
				    @arg1)))),

	send(Image, recogniser, @man_group_recogniser),

	send(new(Dialog, dialog), below, V),
	send(V, name, view),
	fill_dialog(Dialog),
	send(new(B, man_summary_browser(man_summary, size(50, 20))),
	     right, V),
	send(B, name, browser),

	send(MB, view, MN),
	send(MB, edit_mode, Manual?edit_mode).
	

view(MB, View) :<-
	"Get the view"::
	get(MB, member, view, View).

browser(MB, Browser) :<-
	"Get the browser"::
	get(MB, member, browser, Browser).


:- pce_global(@man_group_recogniser, make_man_group_recogniser).

make_man_group_recogniser(R) :-
	IM = @event?receiver,
	E = IM?device,
	Tool = IM?frame,
	Manual = Tool?manual,

	SelectLine = and(message(E, caret, ?(IM, index, @event)),
			 message(E, select_line, newline := @on)),
	EditMode = ((Manual?edit_mode) == @on),

	new(CG1, click_gesture(left, '', single, SelectLine)),
	send(CG1, condition, not(EditMode)),
	new(CG2, click_gesture(left, '', double,
			       and(if(EditMode, SelectLine),
				   message(Tool, show_group_members)))),

	new(R, handler_group(CG1, CG2)).



		/********************************
		*            DIALOG		*
		********************************/

fill_dialog(D) :-
	get(D, frame, MB),
	send(D, append, button(help,   message(MB, help))),
	send(D, append, button(quit,   message(MB, quit))),
	send(D, append, new(label), right). 	% reporter


		 /*******************************
		 *	     MEMBERS		*
		 *******************************/

show_group_members(T, Group:[name]) :->
	"Show members of selected group"::
	(   Group == @default
	->  get(T, view, View),
	    get(View, selected, Line),
	    send(@man_group_line_regex, match, Line),
	    get(@man_group_line_regex, register_value, Line, 1, name, Grp)
	;   Grp = Group
	),

	new(Members, chain),
	AddIfInGroup = if(@arg1?group == Grp,
			  message(Members, add, @arg1)),

	send(@classes, for_all,
	     and(assign(new(Class, var), @arg2),
		 message(Class?instance_variables, for_all, AddIfInGroup),
		 message(Class?send_methods, for_all, AddIfInGroup),
		 message(Class?get_methods, for_all, AddIfInGroup))),
	send(T?browser, members, Members).


		/********************************
		*          COMMUNICATION	*
		********************************/

edit_mode(MB, Val:bool) :->
	"Switch edit mode on/off"::
	get(MB, view, View),
	send(View, editable, Val).


selected(MB, Obj:object*) :->
	"Set the selection"::
	(   send(Obj, instance_of, man_group_card)
	->  get(Obj, index, LineNo),
	    send(MB?view, select_line, LineNo)
	;   true
	).


release_selection(MB) :->
	send(MB?view, selection, 0, 0).


		/********************************
		*            FILLING		*
		********************************/

view(MB, ModuleName:name) :->
	"View group module"::
	get(MB?manual, module, ModuleName, @on, Module),
	send(MB, slot, module, Module),
	get(MB, view, View),
	new(Chain, chain),
	send(Module?id_table, for_all,
	     message(Chain, append, @arg2)),
	send(Chain, sort, ?(@arg1?index, compare, @arg2?index)),
	send(Chain, for_all,
	     message(View, format,
		     '%s\t%s\n', @arg1?name, @arg1?summary)),
	send(View, caret, 0).




		 /*******************************
		 *	      SAVING		*
		 *******************************/

save_if_modified(MB) :->
	"Save if contents of the view is modified"::
	get(MB, view, View),
	(   get(View, modified, @on)
	->  send(MB, save)
	;   send(MB, report, status, 'No changes need saving')
	).

:- pce_global(@man_group_line_regex,
	      new(regex(string('\\([^\t]*\\)\t\\(.*\\)\n')))).

save(MB) :->
	"Save all summary cards"::
	get(MB, view, View),
	get(View, text_buffer, TB),
	get(TB, size, Size),		% ensure a newline (HACK)
	(   get(TB, character, Size-1, 10)
	->  true
	;   send(TB, append, string('\n'))
	),
	get(MB, module, Module),
	new(NotDone, chain),
	send(Module?id_table, for_all,
	     message(NotDone, append, @arg2)),
	new(Index, number(0)),
	send(@man_group_line_regex, for_all, TB,
	     message(@prolog, save_line,
		     Module,
		     ?(@arg1, register_value, TB, 1, name),
		     ?(@arg1, register_value, TB, 2),
		     Index,
		     NotDone)),
	send(NotDone, for_all, message(@arg1, free)),
	send(MB, report, status, 'Saved %d groups', Index),
	send(View, modified, @off).


save_line(Module, Group, Summary, Index, NotDone) :-
	get(Module?id_table, member, Group, Card), !,
	send(Summary, strip),
	(   send(NotDone, delete, Card)
	->  true
	;   send(Module, report, error, 'Duplicate group: %s', Group)
	),
	send(Card, store, summary, Summary),
	send(Index, plus, 1),
	get(Index, value, Idx),
	send(Card, store, index, Idx).
save_line(Module, Group, Summary, Index, _NotDone) :-
	send(Index, plus, 1),
	new(_, man_group_card(Module, Group, Index, Summary)).


		 /*******************************
		 *	MANUAL INTERACTION	*
		 *******************************/

load_manual_groups(MB) :->
	"Load groups from the online manual database"::
	get(MB, view, View),
	new(F, file('$PCEHOME/man/info/group.def')),
	send(F, open, read),
	repeat,
	    (	get(F, read_line, Line)
	    ->  substitute(Line, ['\%.*',
				  	'',
				  '#group\\s *\\(\\w+\\)\\s *"\\(.*\\)"',
				  	'\\1\t\\2'
				  ]),
		send(Line, strip),
		\+ send(Line, equal, ''),
		send(View, format, '%s\n', Line),
		fail
	    ;	!,
	        send(F, close)
	    ).
	    
		
%	Move to library!

to_regex(Pattern, Regex) :-
	get(@pce, convert, string(Pattern), regex, Regex).


substitute(_, []) :- !.
substitute(S, [Search, Replace | Rest]) :-
	to_regex(Search, Re),
	(   Replace = call(Head)
	->  Head =.. [Pred|Args],
	    append(Args, [@arg1, @arg2], AllArgs),
	    Msg =.. [message, @prolog, Pred | AllArgs],
	    send(Re, for_all, S, Msg)
	;   send(Re, for_all, S,
		 message(@arg1, replace, @arg2, string(Replace)))
	),
	substitute(S, Rest).

:- pce_end_class.
