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

:- module(dia_menu_item, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

:- pce_autoload(drag_and_drop_dict_item_gesture, library(dragdict)).


:- pce_begin_class(dia_menu_item_editor, dialog_group,
		   "Edit items of a menu").

variable(default,	'chain|function',	get,	"Default value").
variable(message,	'code*',		get,	"Change message").

initialise(ME, Name:[name], Def:[chain|function], Msg:[code]*,
	   Attributes:name ...) :->
	default(Name, members, Nm),
	default(Def, new(chain), Default),
	default(Msg, @nil, Message),

	send(ME, slot, message, Message),

	send_super(ME, initialise, Nm, box),
	send(ME, append,
	     new(B, list_browser(width := 30, height := 7))),
	send(B, style, end_group, style(underline := @on)),
	send(B, label, Nm?label_name),
	send(B, select_message,
	     and(message(ME, current, @arg1?object))),
	send(B, recogniser,
	     new(G, drag_and_drop_dict_item_gesture)),
	send(G, get_source, @arg1?object),
	Term =.. [dia_menu_item_properties, ME | Attributes],
	send(ME, append, new(Term), right),
	send(ME, layout_dialog),
	send(ME, default, Default).


		 /*******************************
		 *	  GET THE PARTS		*
		 *******************************/

browser(ME, B:list_browser) :<-
	"Find the list-browser part"::
	get(ME, member, list_browser, B).
name_item(ME, I:text_item) :<-
	"Find the text-item displaying the name"::
	get(ME, member, attributes, SD),
	get(SD, member, name, I).
label_item(ME, I:dia_label_item) :<-
	"Find the dia_label_item displaying the label"::
	get(ME, member, attributes, SD),
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
	->  get(ME, member, attributes, SD),
	    send(SD, restore)
	;   true
	).


		 /*******************************
		 *	      EDIT		*
		 *******************************/

apply_sub(ME) :->
	"Apply members of the sub-dialog"::
	get(ME, member, attributes, SD),
	send(SD, apply).


create_item(ME, Name:name) :->
	"Create new item from name"::
	(   Name == ''
	->  send(ME, report, warning, 'Please enter a name first'),
	    fail
	;   true
	),
	send(ME, append_item, new(dia_proto_menu_item(Name))),
	send(ME, current, @nil, @on),
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


append_item(ME, Item:menu_item) :->
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
	    send(Msg, forward_receiver, ME, Selection)
	;   true
	).


default(ME, Def:'function|chain') :->
	send(ME, slot, default, Def),
	send(ME, restore).


restore(ME) :->
	"Reload values from the Client"::
	send(ME?browser, clear),
	get(@pce, convert, ME?default, chain, Members),
	send(Members, for_all, message(ME, append_item, @arg1)).


:- pce_end_class.

:- pce_begin_class(dia_menu_item_properties, dialog_group).

initialise(MIP, Dev:dia_menu_item_editor, Attributes:name ...) :->
	send_super(MIP, initialise, attributes),
	new(MI, Dev?current),

	send(MIP, append,
	     new(Name, dia_name_item(name,
				     when(MI, MI?value, ''),
				     message(MI, value, @arg1)))),
	send(MIP, append,
	     new(LI, dia_label_item(label,
				    when(MI, MI?label, ''),
				    if(@arg1 \== '',
				       message(MI, label, @arg1))))),
	new(_, hyper(Name, LI, label_item, name_item)),

	forall(member(A, Attributes),
	       append_attribute(MIP, MI, A)),

	send(MIP, append,
	     new(Add, button('<< add',
			     message(Dev, create_item, Name?selection)))),
	send(MIP, append,
	     button(edit, message(Dev, modify_current))),
	send(MIP, append,
	     button(delete, message(Dev, delete_current))),

	send(Add, default_button, @on).

append_attribute(MIP, MI, end_group) :-
	send(MIP, append_dialog_item, new(EG, menu(end_group, choice))),
	send(EG, append, @off),
	send(EG, append, @on),
	send(EG, default, when(MI, MI?end_group, @off)),
	send(EG, message, message(MI, end_group, @arg1)).
					% To be extended (generic)
					% and integrated with toplevel

:- pce_end_class(dia_menu_item_properties).


:- pce_begin_class(dia_name_item, text_item,
		   "Edit a name, constraining the label").

event(NI, Ev:event) :->
	send_super(NI, event, Ev),
	(   get(NI, hypered, label_item, LabelItem)
	->  get(NI?value_text?string, label_name, DefLabel),
	    send(LabelItem, selection, DefLabel)
	;   true
	).

:- pce_end_class.





