/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(dia_proto,
	  [ proto/4,
	    proto_term/3,
	    attribute/3,
	    dependency/3,
	    port/4,
	    icon/2,
	    summary/2
	  ]).
:- use_module(library(pce)).


proto(reporter,		label(reporter, 'I report warnings, etc.'),
      [],
      []).
proto(label,		label(name, 'I am a label'),
      [],
      []).
proto(image,		label(identifier, image('happy.bm')),
      [ fixed_reference := @off ],
      []).
proto(button,		button(button),
      [ can_resize := @on
      ],
      []).
proto(text_item,	text_item(text_item),
      [ can_resize := @on ],
      []).
proto(slider,		slider(slider, 0, 100, 25),
      [ can_resize := @on ],
      []).
proto(choice,		menu(choice, choice),
      [ members := chain(a, or, b) ],
      [ clear ]).
proto(toggle,		menu(toggle, toggle),
      [ members := chain(a, 'and/or', b) ],
      [ clear ]).
proto(cycle,		menu(cycle, cycle),
      [ members := chain(a) ],
      [ clear ]).
proto(list_browser,	list_browser(@default, 30, 5),
      [ name := list,
	show_label := @on,
	can_resize := @on ],
      []).
proto(editor,		editor(@default, 30, 5),
      [ name := editor,
	contents := 'This is an editor',
	can_resize := @on ],
      [ clear ]).


		 /*******************************
		 *	       ICONS		*
		 *******************************/

icon(reporter,		'reporter.bm').
icon(label,		'label.bm').
icon(image,		'image.bm').
icon(button,		'button.bm').
icon(text_item,		'text_item.bm').
icon(slider,		'slider.bm').
icon(choice,		'choice.bm').
icon(toggle,		'toggle.bm').
icon(cycle,		'cycle.bm').
icon(list_browser,	'list.bm').
icon(editor,		'editor.bm').


summary(reporter,	'Label to report messages').
summary(label,		'Information label (text)').
summary(image,		'Information image (image)').
summary(button,		'Push button').
summary(text_item,	'Text entry field').
summary(slider,		'Slider for numerical values').
summary(choice,		'Menu for a single value (radio-button)').
summary(toggle,		'Menu for multiple values').
summary(cycle,		'Menu for a single value (with popup)').
summary(list_browser,	'Dynamic scrollable list of items (list_browser)').
summary(editor,		'Text editor (scrollable)').


		 /*******************************
		 *	     NEW TERMS		*
		 *******************************/

proto_term(reporter,	label,		[name, selection]).
proto_term(label,	label,		[name, selection]).
proto_term(image,	label,		[name, selection]).
proto_term(button,	button,		[name]).
proto_term(text_item,	text_item,	[name]).
proto_term(slider,	slider,		[name, low, high, selection]).
proto_term(choice,	menu,		[name, kind := choice]).
proto_term(toggle,	menu,		[name, kind := toggle]).
proto_term(cycle,	menu,		[name, kind := cycle]).
proto_term(list_browser,list_browser,	[dict := @default,width,height]).
proto_term(editor,	editor,		[text_buffer:= @default,width,height]).
proto_term(dialog,	dialog,		[label]).
proto_term(menu_item,	menu_item,	[ value,
					  [message],
					  [label],
					  [end_group],
					  [condition],
					  [accelerator]
					]).
proto_term(popup,	popup,		[ name, [message]]).


		 /*******************************
		 *	EDITABLE ATTRIBUTES	*
		 *******************************/

:- pce_global(@dia_selection_is_text,
	      new(message(@arg1?selection, instance_of, char_array))).

	% CREATE

attribute(create, reporter, name).
attribute(create, reporter, selection).
attribute(create, reporter, font(@dia_selection_is_text)).
attribute(create, reporter, length).

attribute(create, button, name).
attribute(create, button, label).
attribute(create, button, default_button).
attribute(create, button, has_popup).
attribute(create, button, popup_items(@arg1?has_popup == @on)).

attribute(create, label, name).
attribute(create, label, selection).
attribute(create, label, font(@dia_selection_is_text)).
attribute(create, label, has_popup).
attribute(create, label, popup_items(@arg1?has_popup == @on)).

attribute(create, slider, name).
attribute(create, slider, label).
attribute(create, slider, show_label).
attribute(create, slider, show_value).
attribute(create, slider, drag).
attribute(create, slider, width).
attribute(create, slider, low).
attribute(create, slider, high).
attribute(create, slider, selection).

attribute(create, text_item, name).
attribute(create, text_item, label).
attribute(create, text_item, selection).
attribute(create, text_item, type).
attribute(create, text_item, pen).
attribute(create, text_item, length).
attribute(create, text_item, show_label).

attribute(create, menu, members).
attribute(create, menu, name).
attribute(create, menu, label).
attribute(create, menu, multiple_selection(@arg1?kind \== cycle)).
attribute(create, menu, feedback(@arg1?kind \== cycle)).
attribute(create, menu, off_image(@arg1?feedback == image)).
attribute(create, menu, on_image(@arg1?feedback == image)).
attribute(create, menu, pen(@arg1?feedback == box)).
attribute(create, menu, border).
attribute(create, menu, format).
attribute(create, menu, value_font).
attribute(create, menu, layout).
attribute(create, menu, show_label).
attribute(create, menu, columns).

attribute(create, image, Att)  :- attribute(create, label, Att).
attribute(create, choice, Att) :- attribute(create, menu, Att).
attribute(create, toggle, Att) :- attribute(create, menu, Att).
attribute(create, cycle, Att)  :- attribute(create, menu, Att).

attribute(create, dialog, name).
attribute(create, dialog, frame_label).

attribute(create, list_browser, name).
attribute(create, list_browser, show_label).
attribute(create, list_browser, label(@arg1?show_label == @on)).
attribute(create, list_browser, multiple_selection).
attribute(create, list_browser, has_popup).
attribute(create, list_browser, popup_items(@arg1?has_popup == @on)).

attribute(create, editor, name).
attribute(create, editor, font).
attribute(create, editor, fill_mode).
attribute(create, editor, has_popup).
attribute(create, editor, popup_items(@arg1?has_popup == @on)).

	% LAYOUT

attribute(layout, dialog, gap_x).
attribute(layout, dialog, gap_y).

attribute(layout, Item, Att) :-
	Item \== dialog,
	Item \== dialog_item,
	attribute(layout, dialog_item, Att).

attribute(layout, dialog_item, alignment).
attribute(layout, dialog_item, fixed_alignment).
attribute(layout, dialog_item, reference_x).
attribute(layout, dialog_item, reference_y).
attribute(layout, dialog_item, fixed_reference).

dependency(choice, F, T) :- !, dependency(menu, F, T).
dependency(toggle, F, T) :- !, dependency(menu, F, T).
dependency(cycle,  F, T) :- !, dependency(menu, F, T).

dependency(_,		name,		label).
dependency(menu,	feedback,	off_image).
dependency(menu,	feedback,	on_image).
dependency(menu,	feedback,	pen).
dependency(dialog,	name,		frame_label).
dependency(_,		reference_x,	fixed_reference).
dependency(_,		reference_y,	fixed_reference).


		 /*******************************
		 *	   ACTION MODEL		*
		 *******************************/

port(button,	obligatory,	message,	event).
port(button,	optional,	execute,	send).

port(text_item, optional,	execute,	send).
port(text_item, optional,	message,	event).
port(text_item, obligatory,	selection,	get).

port(slider,	optional,	message,	event).
port(slider,	obligatory,	selection,	get).

port(menu,	optional,	message,	event).
port(menu,	obligatory,	selection,	get).

port(list_browser, obligatory,	select_message,	event).
port(list_browser, obligatory,	open_message,	event).
port(list_browser, obligatory,	selection,	get).
port(list_browser, optional,	select_middle_message, event).
port(list_browser, optional,	append, 	send).
port(list_browser, optional,	members, 	send).

port(editor,	optional,	contents,	get).
port(editor,	optional,	load,		send).

port(dialog,	optional,	show,		send).
port(dialog,	optional,	return,		send).
port(dialog,	optional,	report,		send).
port(dialog, 	optional,	done_message,	event).
port(dialog, 	optional,	resize_message,	event).
port(dialog,	optional,	frame,		get).

port(menu_item,	obligatory,	message,	event).
port(menu_item,	optional,	value,		get).

port(pce,	optional,	write_ln,	send).
port(pce,	optional,	writef,		send).

port(host,	optional,	format,		send).
port(host,	optional,	write_ln,	send).

port(display,	optional,	inform,		send).
port(display,	optional,	confirm,	send).

port(class,	obligatory,	instance,	get).

port(choice, Priority, Port, Type) :- port(menu, Priority, Port, Type).
port(toggle, Priority, Port, Type) :- port(menu, Priority, Port, Type).
port(cycle,  Priority, Port, Type) :- port(menu, Priority, Port, Type).

port(Proto,	optional,	active,		send) :-
	get(@pce, convert, Proto, class, Class),
	get(Class, send_method, active, _).

port(quote_function, optional,	'_forward',	get).

port(_,		optional,	self,		get).

		 /*******************************
		 *	COMMON EXPANSIONS	*
		 *******************************/

port(dict_item,	obligatory,	key,		get).
port(dict_item,	optional,	label,		get).
port(dict_item,	optional,	object,		get).

port(directory, optional,	files,		get).
port(directory, optional,	file,		get).
port(directory, optional,	directories,	get).
port(directory, optional,	directory,	get).
