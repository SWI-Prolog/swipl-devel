/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(pce_show_event,
	  [ event_monitor/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_toc)).
:- require([ forall/2
	   , pce_help_file/2
	   , send_list/2
	   , send_list/3
	   ]).

:- pce_help_file(event_monitor, pce_help('event_monitor.hlp')).

program_version('0.0').

event_monitor :-
	send(new(event_viewer), open).

:- pce_begin_class(event_viewer, frame,
		   "Show event-details").

initialise(V) :->
	send_super(V, initialise, 'XPCE Event-viewer'),
	send(V, append, new(P, picture)),
	send(new(D, dialog), below, P),
	send(new(event_hierarchy_window), left, D),
	send(new(event_list), right, D),
	send(new(D2, dialog), above, D),
	send(D2, name, menu_bar_dialog),
	send(V, fill_menu_bar),
	send(V, fill_dialog),
	send(V, fill_picture),
	send(V, keyboard_focus, P).

fill_menu_bar(V) :->
	get(V, member, menu_bar_dialog, D),
	send(D, pen, 0),
	send(D, gap, size(0, 5)),
	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(Help, popup(help))),
	send_list(Help, append,
		  [ menu_item(about,
			      message(V, about)),
		    menu_item(help,
			      message(V, help))
		  ]).

about(_) :->
	program_version(Version),
	send(@display, inform,
	     'XPCE event-monitor\nversion %s\n\nBy Jan Wielemaker',
	     Version).

help(_) :->
	send(@helper, give_help, event_monitor).

event_property(id, next_row, []).
event_property(x,  next_row, [length(4)]).
event_property(y,  right,    [length(4), alignment(left)]).

modifier(s).
modifier(c).
modifier(m).

fill_dialog(V) :->
	get(V, member, dialog, Dialog),
	send(Dialog, append, new(DI, text_item(description))),
	send(DI, length, 40),
	send(DI, editable, @off),
	(   event_property(Name, Where, Atts),
	    send(Dialog, append, new(I, text_item(Name)), Where),
	    send(I, length, 15),
	    send(I, editable, @off),
	    send_list(I, Atts),
	    fail
	;   true
	),
	send(Dialog, append, new(I, menu(modifiers, marked))),
	forall(modifier(M), send(I, append, M)).

update_dialog(V, Ev:event) :->
	get(V, member, dialog, Dialog),
	(   event_property(Name, _, _),
	    get(Dialog, member, Name, Item),
	    get(Ev, Name, Value),
	    send(Item, selection, Value),
	    fail
	;   true
	),
	get(Dialog, member, modifiers, Menu),
	forall(modifier(M),
	       (   send(Ev, has_modifier, M)
	       ->  send(Menu, selected, M, @on)
	       ;   send(Menu, selected, M, @off)
	       )),
	get(Dialog, member, description, DI),
	(   get(Ev, id, Id),
	    description(Id, Description)
	->  send(DI, selection, Description)
	;   send(DI, selection, '<no description>')
	).

show_event(V, Ev:event) :->
	"Show last event"::
	get(V, member, event_hierarchy_window, EH),
	send(V, update_dialog, Ev),
	send(V, flush),
	send(EH, show, Ev).

append_event(V, Ev:event) :->
	get(V, member, event_list, EL),
	send(EL, append, Ev),
	send(V, show_event, Ev).

event(V, Ev:event) :->
	(   send(Ev, is_a, keyboard)
	->  send(V, append_event, Ev)
	;   send_super(V, event, Ev)
	).

fill_picture(V) :->
	get(V, member, picture, P),
	send(P, display, new(B, box(100,100))),
	send(B, fill_pattern, colour(dark_green)),
	send(P, resize_message, message(V, resize_box, B)),
	send(B, recogniser,
	     handler(any, message(V, append_event, @arg1))).

resize_box(_V, B:box) :->
	get(B, device, P),
	get(P, visible, area(X, Y, W, H)),
	send(B, set, X+W/5, Y+H/5, 3*W/5, 3*H/5).

:- pce_end_class.

:- pce_begin_class(event_hierarchy_window, toc_window,
		   "Show event-hierarchy").

initialise(EH) :->
	send_super(EH, initialise),
	get(@event_tree, root, Root),
	send(EH, root, toc_folder(Root?value, Root)),
	send(EH, expand_root).

expand_node(W, Node:event_node) :->
	(   get(Node, sons, Sons),
	    Sons \== @nil
	->  send(Node?sons, for_all,
		 message(W, add_node, Node, @arg1))
	;   true
	).

add_node(W, Parent:event_node, Son:event_node) :->
	(   (   get(Son, sons, Sons),
		Sons \== @nil,
		\+ send(Sons, empty)
	    ;   get(Son, value, Id),
		special(Id)
	    )
	->  send(W, son, Parent, toc_folder(Son?value, Son))
	;   send(W, son, Parent, toc_file(Son?value, Son))
	).

special(control).
special(printable).
special(meta).	

show(W, Ev:event) :->
	"Show indicated event-id"::
	get(Ev, id, Id),
	(   special(Special),
	    send(Ev, is_a, Special)
	->  get(@event_tree, node, Special, SpecialEvNode),
	    find(W, SpecialEvNode, SpecialNode),
	    get(Ev, key, Key),
	    get(string('%s (%d)', Key, Id), value, KeyId),
	    (	get(W, node, KeyId, Node)
	    ->	true
	    ;	send(W, son, SpecialEvNode, new(Node, toc_file(KeyId, KeyId))),
		send_class(SpecialNode, node, collapsed(@off))
	    )
	;   get(@event_tree, node, Id, EvNode),
	    find(W, EvNode, Node)
	),
	get(Node?tree, window, Window),
	send(Window, normalise, Node),
	get(Node, member, bitmap, BM),
	send(BM, flush),
	send(BM, flash),
	send(Node?tree, selection, Node).

find(W, EvNode, Node) :-
	(   get(W, node, EvNode, Node)
	->  true
	;   get(EvNode, parent, ParentEvNode),
	    find(W, ParentEvNode, ParentNode),
	    send(W, add_node, ParentEvNode, EvNode),
	    send_class(ParentNode, node, collapsed(@off)),
	    get(W, node, EvNode, Node)
	).
	
:- pce_end_class.

:- pce_begin_class(event_list, browser).

initialise(EL) :->
	send_super(EL, initialise),
	send(EL, select_message,
	     message(EL?frame, show_event, @arg1?object)).

append(EL, Ev:event) :->
	get(Ev, clone, Clone),
	send_super(EL, append, new(DI, dict_item(Clone?key, @default, Clone))),
	send(EL, normalise, DI),
	send(EL, selection, DI).

:- pce_end_class.

description(any,		       'Root of event-hierarchy').
description(mouse,		       'Mouse (-button) related event').
description(keyboard,		       'Keyboard related event').
description(focus,		       'Keyboard/pointer-focus related event').
description(area,		       'Area-crossing event').
description(area_enter,		       'Pointer enters area (of graphical)').
description(area_exit,		       'Pointer leaves area (of graphical)').
description(area_cancel,	       'Pointer leaves area with mouse-button down').
description(area_resume,	       'Pointer re-enters area with mouse-button down').
description(position,		       'Pointer movement, no buttons depressed').
description(loc_move,		       'Pointer moves, no buttons depressed').
description(loc_still,		       'Pointer did not move for 3/4 second').
description(release_focus,	       'Graphical lost (pointer-) focus').
description(obtain_focus,	       'Graphical got (pointer-) focus').
description(obtain_keyboard_focus,     'Graphical gets keyboard focus of window').
description(activate_keyboard_focus,   'Graphical gets keyboard focus').
description(deactivate_keyboard_focus, 'Window of graphical looses keyboard focus').
description(release_keyboard_focus,    'Graphical looses keyboard focus of window').
description(ascii,		       'Normal key-press event').
description(meta,		       'Meta (ALT) key-press event').
description(control,		       'Non-printable key-press event').
description(printable,		       'Printable key-press event').
description(function,		       'Function-key key-press event').
description(key_left,		       'Left function-keypad key-press event').
description(key_right,		       'Right function-keypad key-press event').
description(key_top,		       'Top function-keypad key-press event').
description(cursor,		       'Cursor-keypad key-press event').
description(named_function,	       'Named function key-press event').
description(cursor_up,		       'Cursor-up key-press event').
description(cursor_down,	       'Cursor-up key-press event').
description(cursor_left,	       'Cursor-left key-press event').
description(cursor_right,	       'Cursor-right key-press event').
description(page_up,		       'Page-up key-press event').
description(page_down,		       'Page-down key-press event').
description(cursor_home,	       '"Home" key-press event').
description(end,		       '"End" key-press event').
description(127,		       '"Delete" key-press event').
description(NamedFunction, Description) :-
	get(@event_tree, node, NamedFunction, Node),
	get(Node, parent, Parent),
	get(Parent, value, named_function), !,
	get(string('"%s" key-press event', NamedFunction?capitalise),
	    value,
	    Description).
description(NamedFunction, Description) :-
	get(@event_tree, node, NamedFunction, Node),
	get(Node, parent, Parent),
	get(Parent, value, named_function), !,
	get(string('"%s" key-press event', NamedFunction?capitalise),
	    value,
	    Description).
description(ButtonDrag, Description) :-
	new(Re, regex('ms_\\([a-z]+\\)_drag')),
	send(Re, match, ButtonDrag), !,
	get(Re, register_value, ButtonDrag, 1, name, LR),
	get(string('Move with %s mouse-button depressed', LR),
	    value,
	    Description).
description(ButtonDown, Description) :-
	new(Re, regex('ms_\\([a-z]+\\)_\\([a-z]+\\)')),
	send(Re, match, ButtonDown), !,
	get(Re, register_value, ButtonDown, 1, name, LR),
	get(Re, register_value, ButtonDown, 2, name, UD),
	get(string('%s mouse-button %s', LR?capitalise, UD),
	    value,
	    Description).
description(FunctionKey, Description) :-
	new(Re, regex('key_\\([a-z]+\\)_\\([0-9]+\\)')),
	send(Re, match, FunctionKey), !,
	get(Re, register_value, FunctionKey, 1, name, Set),
	get(Re, register_value, FunctionKey, 2, int, N),
	get(string('%s function-keypad "F%d" key-press event', Set?capitalise, N),
	    value,
	    Description).



