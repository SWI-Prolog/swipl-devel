/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(dia_menu_item, []).
:- use_module(library(pce)).

:- pce_autoload(sub_dialog, library('dialog/sub_dialog')).
:- pce_autoload(drag_and_drop_dict_item_gesture, library(dragdict)).


:- pce_begin_class(dia_menu_item_editor, sub_dialog).

variable(default,	'chain|function',	get,	"Default value").
variable(message,	'code*',		get,	"Change message").

initialise(ME, Name:[name], Def:[chain|function], Msg:[code]*,
	   Attributes:name ...) :->
	default(Name, members, Nm),
	default(Def, new(chain), Default),
	default(Msg, @nil, Message),

	send(ME, slot, message, Message),

	send(ME, send_super, initialise),
	send(ME, name, Nm),
	send(ME, pen, 1),
	send(ME, border, 5),
	send(ME, radius, 7),

	send(ME, append_dialog_item,
	     new(B, list_browser(width := 30, height := 7))),
	send(B, style, end_group, style(underline := @on)),
	send(B, label, Nm?label_name),
	send(B, select_message,
	     and(message(ME, mode, edit),
		 message(ME, current, @arg1?object))),
	send(B, recogniser,
	     new(G, drag_and_drop_dict_item_gesture)),
	send(G, get_source, @arg1?object),
	send(ME, append_dialog_item, new(A, sub_dialog), right),
	fill_attribute_dialog(A, Attributes, create),
	send(ME, layout_dialog),
	send(ME, default, Default).


fill_attribute_dialog(D, Attributes, Mode) :-
	get(D, device, Dev),
	new(MI, Dev?current),

	send(D, append_dialog_item,
	     new(M, menu(mode, choice,
			 and(message(Dev, mode, @arg1),
			     message(Dev, current, @nil))))),
	send(M, append, create),
	send(M, append, edit),
	send(D, append_dialog_item,
	     new(Name, text_item(name,
				 when(MI, MI?value, ''),
				 message(MI, value, @arg1)))),
	send(D, append_dialog_item,
	     dia_label_item(label,
			    when(MI, MI?label, ''),
			    if(@arg1 \== '', message(MI, label, @arg1)))),
	send(Attributes, for_all,
	     message(@prolog, append_attribute, D, @arg1)),

	send(D, send_method,
	     send_method(modified_item, vector(graphical, bool),
			 if(@arg1 == Name,
			    message(Dev, changed_name_item),
			    new(or)))),

	send(Dev, mode, Mode).

append_attribute(D, end_group) :-
	get(D, device, Dev),
	new(MI, Dev?current),
	send(D, append_dialog_item, new(EG, menu(end_group, choice))),
	send(EG, append, @off),
	send(EG, append, @on),
	send(EG, default, when(MI, MI?end_group, @off)),
	send(EG, message, message(MI, end_group, @arg1)).
					% To be extended (generic)
					% and integrated with toplevel


delete_item(Device, Member) :-
	get(Device, member, Member, Gr), !,
	send(Gr, free).
delete_item(_, _).

mode(ME, Mode:{edit,create}) :->
	"Switch to indicated mode"::
	get(ME, member, sub_dialog, SD),
	delete_item(SD, create_item),
	delete_item(SD, modify_item),
	delete_item(SD, delete),

	get(SD, member, name, Name),
	get(SD, member, label, Label),
	get(SD, member, mode, ModeMenu),
	
	send(ModeMenu, selection, Mode),

	(   Mode == create
	->  send(SD, append_dialog_item,
		 button(create_item,
			and(message(ME, create, Name?selection),
			    message(Name, clear),
			    message(Label, clear),
			    message(@receiver, default_button, @off))))
	;   send(SD, append_dialog_item,
		 button(modify_item,
			and(message(ME, modify_current),
			    message(@receiver, default_button, @off)))),
	    send(SD, append_dialog_item,
		 button(delete, message(ME, delete_current)))
	),
	send(SD, layout_dialog).

mode(ME, Mode:{edit,create}) :<-
	get(ME, member, sub_dialog, SD),
	get(SD, member, mode, Menu),
	get(Menu, selection, Mode).

changed_name_item(ME) :->
	"Name changed: prepare create"::
	get(ME, member, sub_dialog, SD),
	(   get(ME, mode, create)
	->  get(SD, member, create_item, Button)
	;   get(SD, member, modify_item, Button),
	    (	get(SD, member, label, LabelItem),
		get(LabelItem, kind, text)
	    ->	send(LabelItem, selection, '')
	    ;	true
	    )
	),
	send(Button, default_button, @on).


		 /*******************************
		 *	  GET THE PARTS		*
		 *******************************/

browser(ME, B:list_browser) :<-
	"Find the list-browser part"::
	get(ME, member, list_browser, B).
name_item(ME, I:text_item) :<-
	"Find the text-item displaying the name"::
	get(ME, member, sub_dialog, SD),
	get(SD, member, name, I).
label_item(ME, I:dia_label_item) :<-
	"Find the dia_label_item displaying the label"::
	get(ME, member, sub_dialog, SD),
	get(SD, member, label, I).


		 /*******************************
		 *	    CURRENT		*
		 *******************************/

current(ME, MI:menu_item) :<-
	get(ME?browser, selection, DI), DI \== @nil,
	get(DI, object, MI).

current(ME, MI:menu_item*, Restore:[bool]) :->
	"Select the indicated menu-item"::
	get(ME, browser, Browser),
	(   get(Browser?dict?members, find, @arg1?object == MI, DI)
	->  send(Browser, selection, DI)
	;   send(Browser, selection, @nil)
	),
	(   Restore \== @off
	->  get(ME, member, sub_dialog, SD),
	    send(SD?graphicals, for_some, message(@arg1, restore))
	;   true
	).


		 /*******************************
		 *	      EDIT		*
		 *******************************/

apply_sub(ME) :->
	"Apply members of the sub-dialog"::
	get(ME, member, sub_dialog, SD),
	send(SD?graphicals, for_some,
	     if(@arg1?name \== mode, message(@arg1, apply))).

create(ME, Name:name) :->
	"Create new item from name"::
	(   Name == ''
	->  send(ME, report, warning, 'Please enter a name first'),
	    fail
	;   true
	),
	send(ME, append, new(MI, dia_proto_menu_item(Name))),
	send(ME, current, MI, @off),
	send(ME, apply_sub),
	send(ME, apply).


modify_current(ME) :->
	"Modify current menu-item"::
	send(ME, apply_sub),
	send(ME, update_dict_item),
	send(ME?label_item, selection, ME?current?label).


update_dict_item(ME, DI:[dict_item]) :->
	"Update dict_item"::
	(   DI == @default
	->  get(ME?browser, selection, I), I \== @nil
	;   I = DI
	),
	get(I, object, MI),
	send(I, key, MI?value),
	send(I, style, when(MI?end_group == @on, end_group, @default)).


delete_current(ME) :->
	"Delete selected entry"::
	get(ME?browser, selection, DI),
	send(ME?browser, delete, DI),
	send(ME, current, @nil),
	send(ME, apply).


append(ME, Item:menu_item) :->
	"Append menu-item to the browser"::
	get(ME, browser, B),
	send(B, append, new(DI, dict_item(Item?value, @default, Item))),
	(   get(Item, end_group, @on)
	->  send(DI, style, end_group)
	;   true
	).
	

		 /*******************************
		 *	  PLAY DIALOG ITEM	*
		 *******************************/

selection(ME, Items:chain) :<-
	"Get the selection as a chain of menu-items"::
	get(ME, browser, Browser),
	get(Browser, members, DictItems),
	get(DictItems, map, @arg1?object, Items).


apply(ME, _Modified:[bool]) :->
	get(ME, message, Msg),
	(   Msg \== @nil
	->  get(ME, selection, Selection),
	    send(Msg, forward, Selection)
	;   true
	).


default(ME, Def:'function|chain') :->
	send(ME, slot, default, Def),
	send(ME, restore).


restore(ME) :->
	"Reload values from the Client"::
	send(ME?browser, clear),
	get(@pce, convert, ME?default, chain, Members),
	send(Members, for_all, message(ME, append, @arg1)).


:- pce_end_class.
