/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(emacs_prompt,
	  [ make_item/6
	  ]).
:- use_module(library(pce)).

:- pce_autoload(behaviour_item, library('man/behaviour_item')).
:- pce_autoload(directory_item, library('file_item')).


%	make_item(+Mode, +Label, +Default, +Type, +History, -Item)
%
%	Create a dialog item for editing an object of the specified
%	type.

					% totally unknown
make_item(_Mode, Label, Default, @default, _History, Item) :- !,
	new(Item, text_item(Label, Default)).
					% files and directories
make_item(Mode, Label, Default, Type, _History, Item) :-
	(   send(Type, includes, file)
	;   send(Type, includes, directory)
	), !,
	(   send(Default, instance_of, file)
	->  get(Default, name, D2)
	;   D2 = Default
	),
	(   send(D2, instance_of, char_array),
	    send(D2, prefix, '/')
	->  DefPath = D2
	;   send(D2, instance_of, char_array),
	    send(regex('[a-zA-Z0-9_-.]+$'), match, D2)
	->  new(DefPath, string('%s/%s', Mode?directory?path, D2))
	;   new(DefPath, string('%s', Mode?directory?path)),
	    send(DefPath, ensure_suffix, /)
	),
	(   send(Type, includes, file)
	->  new(Item, file_item(Label, DefPath))
	;   new(Item, directory_item(Label, DefPath))
	).
					% emacs commands
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, emacs_mode_command), !,
	default(Default, '', Selection),
	new(Item, emacs_command_item(Label, Selection)),
	send(Item, type, Type).
					% emacs buffer
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, emacs_buffer), !,
	default(Default, '', Selection),
	new(Item, text_item(Label, Selection)),
	send(Item, type, Type),
	get(@emacs, buffers, Buffers),
	send(Item, value_set, Buffers).
					% XPCE behaviour
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, behaviour), !,
	default(Default, '', Selection),
	new(Item, behaviour_item(Label, Selection)).
					% XPCE classes
make_item(_Mode, Label, Default, Type, _History, Item) :-
	send(Type, includes, class), !,
	default(Default, '', Selection),
	new(Item, text_item(Label, Selection)),
	send(Item, type, Type),
	new(Classes, chain),
	send(@classes, for_all, message(Classes, append, @arg1)),
	send(Item, value_set, Classes).
					% Anything else
make_item(_Mode, Label, Default, Type, History, Item) :-
	default(Default, '', Selection),
	new(Item, text_item(Label, Selection)),
	send(Item, type, Type),
	(   History \== @default,
	    \+ get(Type, value_set, _)
	->  send(Item, value_set, History)
	;   true
	).


		 /*******************************
		 *	   PROMPT DIALOG	*
		 *******************************/


:- pce_begin_class(emacs_prompt_dialog, dialog,
		   "Prompt for method arguments").

initialise(D, Mode:emacs_mode, Impl:any, Argv:vector) :->
	(   send(Impl, has_get_method, summary),
	    get(Impl, summary, Summary), Summary \== @nil
	->  Label = Summary
	;   get(Impl, name, Name),
	    Label = string('PceEmacs arguments for %s', Name)
	),
	send_super(D, initialise, Label).


prompt(D, V:emacs_view, Argv:vector) :->
	send(D, transient_for, V),
	send(D, modal, transient),
	get(V?editor, display_position, point(X,Y)),
	get(V?editor, size, size(W, H)),
	get(D, confirm_centered, point(X+W/2, Y+H/2), Ok),
	(   Ok == ok
	->  send(D, collect, Argv)
	;   send(D, destroy),
	    fail
	).

:- pce_end_class(emacs_prompt_dialog).
