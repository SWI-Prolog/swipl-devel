/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(dia_attribute, []).
:- use_module(library(pce)).
:- use_module(proto, [dependency/3]).
:- require([ forall/2
	   ]).


:- pce_autoload(dia_menu_item_editor, library('dialog/menuitem')).
:- pce_autoload(dia_label_item, library('dialog/label')).
:- pce_autoload(image_item, library('dialog/image_item')).
:- pce_autoload(font_item, library(pce_font_item)).

:- pce_begin_class(dia_attribute_editor, dialog).

resource(activate, '{displayed,active}', active,
	 "Handle inappropriate items").

client(DE, Client:object, Ats:'name|tuple ...') :->
	"Prepare for editing object"::
	send(DE, clear),
	new(Hyper, hyper(DE, Client, client, attribute_editor)),
	send(Hyper, send_method,
	     send_method(unlink_to, new(vector),
			 and(message(DE, clear),
			     message(Hyper, send_class, unlink_to)))),
	pce_class_name(Client, ClassName),
	send(DE, append,
	     label(reporter, string('Edit %s `%N''', ClassName, Client))),
	send(Ats, for_all, message(DE, append_attribute, @arg1)),
	send(DE, append, button(quit)),
	send(DE, evaluate_conditions),
	send(DE, advance).
	

pce_class_name(Client, ClassName) :-
	get(Client, class_name, N0),
	(   get(N0, delete_prefix, dia_proto_, ClassName)
	;   get(N0, delete_prefix, dia_target_, ClassName)
	;   ClassName = N0
	), !.


client(DE, Client:object) :<-
	"Find the current client"::
	get(DE, hypered, client, Client).


modified_item(DE, Item:graphical, _Modified:bool) :->
	"Clear label on change of name"::
	get(Item, name, name),
	get(DE, member, label, LabelItem),
	send(LabelItem, clear),
	fail.

evaluate_conditions(DE) :->
	"Switch instances off/on"::
	get(DE, resource_value, activate, How),
	send(DE?graphicals, for_all,
	     if(?(@arg1, attribute, condition),
		message(@arg1, How,
			when(message(?(@arg1, attribute, condition), forward,
				     @arg1?window?client),
			     @on,
			     @off)))),
	send(DE, layout).

update_dependancies(DE, Changed:name) :->
	"Update other possible changed values"::
	get(DE?client, proto, Proto),
	forall(dependency(Proto, Changed, Dependant),
	       (   get(DE, member, Dependant, Item)
	       ->  send(Item, restore)
	       ;   true
	       )).

layout(DE) :->
	"Recompute layout and update size"::
	send(DE, send_super, layout),
	get(DE, bounding_box, area(_, _, W, H)),
	get(DE, size, size(DW, DH)),
	get(DE, gap, size(GW, GH)),
	NW is max(DW, W+2*GW),
	NH is max(DH, H+2*GH),
	send(DE, size, size(NW, NH)).


append_attribute(DE, A:'name|tuple') :->
	"Append item for named attribute"::
	(   object(A, tuple(Attr, Cond))
	;   Attr = A
	), !,
	get(DE, client, Client),
	(   get(Client, send_method, Attr, tuple(_, Method)),
	    get(Method, argument_type, 1, Type)
	->  make_item(Attr, Type, Client, Item),
	    send(DE, append, Item),
	    (	nonvar(Cond)
	    ->	send(Item, attribute, attribute(condition, Cond))
	    ;	true
	    )
	;   send(DE, report, error, 'No method for %s', Attr)
	).


:- pce_global(@att_generic,
	      new(and(message(@receiver?window?client, @receiver?name, @arg1),
		      message(@receiver?window, evaluate_conditions),
		      message(@receiver?window, update_dependancies,
			      @receiver?name)))).

make_item(members, Type, Client, Item) :-		% CHAIN (menu-items)
	send(Type, includes, chain),
	send(Client, instance_of, menu), !,
	new(Item, dia_menu_item_editor(members, Client?members, @att_generic)).
make_item(popup_items, Type, Client, Item) :-		% CHAIN (menu-items)
	send(Type, includes, chain),
	new(Item, dia_menu_item_editor(popup_items,
				       Client?popup_items, @att_generic,
				       end_group)).
make_item(Attr, Type, Client, Item) :-		% TYPE
	send(Type, includes, type), !,
	new(Item, text_item(Attr, Client?Attr, @att_generic)),
	send(Item, type, Type),
	new(Ch, chain),
	send(@types, for_all, if(@arg2?kind \== alias,
				 message(Ch, append, @arg1))),
	send(Ch, sort),
	send(Item, value_set, Ch).
make_item(Attr, Type, Client, Item) :-		% CHAR_ARRAY|IMAGE (label)
	send(Type, includes, char_array),
	send(Type, includes, image), !,	
	new(Item, dia_label_item(Attr, Client?Attr, @att_generic)).
make_item(Attr, Type, Client, Item) :-		% IMAGE
	send(Type, includes, image), !,
	new(Item, image_item(Attr, Client?Attr, @att_generic)).
make_item(Attr, Type, Client, Item) :-		% we have a small value-set
	get(Type, value_set, ValueSet),
	get(ValueSet, size, Size),
	Size =< 4, !,
	new(Item, menu(Attr, choice, @att_generic)),
	send(ValueSet, for_all, message(Item, append, @arg1)),
	send(Item, default, Client?Attr).
make_item(Attr, Type, Client, Item) :-		% FONT
	send(Type, includes, font), !,
	new(Item, font_item(Attr, Client?Attr, @att_generic)).
make_item(Attr, Type, Client, Item) :-		% 0..100 (percentage)
	send(Type, includes, '0..100'), !,
	new(Item, slider(Attr, 0, 100, Client?Attr, @att_generic)),
	send(Item, drag, @on),
	send(Item, width, 100).
make_item(Attr, Type, Client, Item) :-		% INT
	unalias_type(Type, T2),
	(   send(T2, includes, int)
	;   get(T2, kind, int_range)
	), !,
	new(Item, text_item(Attr, Client?Attr, @att_generic)),
	send(Item, type, Type),
	send(Item, width, 5).
make_item(Attr, Type, Client, Item) :-		% All the rest
	new(Item, text_item(Attr, Client?Attr, @att_generic)),
	send(Item, type, Type).	

unalias_type(Type, Type).
unalias_type(Type, T2) :-
	get(Type, kind, alias),
	get(Type, context, Ctx),
	unalias_type(Ctx, T2).


quit(DE) :->
	send(DE, destroy).

:- pce_end_class.
