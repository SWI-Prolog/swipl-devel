/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_preferences,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).

:- pce_help_file(plprefs, pce_help('plprefs.hlp')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Edit the SWI-Prolog preferences that are aved in the Windows registry.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
prefs :-
	Prefs = @prefs,
	free(Prefs),
	send(new(Prefs, prolog_preferences), open).
*/

stack(local,  locallimit,  localSize).
stack(global, globallimit, globalSize).
stack(trail,  traillimit,  trailSize).

:- pce_begin_class(prolog_preferences, dialog,
		   "Edit the default preferences").

initialise(D) :->
	send_super(D, initialise, 'SWI-Prolog registry preferences'),
	forall(stack(Name, _, _),
	       send(D, append, prolog_stack_size_item(Name))),
	send(D, append, button(cancel)),
	send(D, append, button(apply)),
	send(D, append, button(help)),
	send(D, default_button, apply).

cancel(D) :->
	send(D, destroy).

help(_D) :->
	"Show window with help-text"::
	send(@helper, give_help, plprefs, main).

apply(D) :->
	(   get(D?graphicals, find,
		and(message(@arg1, has_get_method, modified),
		    @arg1?modified == @on),
		_)
	->  send_super(D, apply),
	    send(@display, inform,
		 'Saved new stack limits in the Windows registry\n\
		  These settings become active the next time you start Prolog')
	;   true
	),
	send(D, destroy).

:- pce_end_class(prolog_preferences).


:- pce_begin_class(prolog_stack_size_item, label_box).

initialise(I, Stack:name) :->
	send_super(I, initialise, Stack),
	stack(Stack, Key, _),
	statistics(Key, Current),
	(   Current =:= 128*1024*1024
	->  DefLimit = @off,
	    Def = 0
	;   DefLimit = @on,
	    Def is round(Current/(1024*1024))
	),
	send(I, append, tick_box(limit_to, DefLimit)),
	send(I, append,
	     int_item('Mb', Def, @default, 0, 128), right).

limit_to(I, Limit:bool) :->
	"Trap change of limit"::
	get(I, member, 'Mb', IntItem),
	send(IntItem, active, Limit).

apply(I, Always:[bool]) :->
	"Save to the registry"::
	(   (   Always == @on
	    ;   get(I, modified, @on)
	    )
	->  get(I, name, Stack),
	    get(I, member, limit_to, LimitItem),
	    (   get(LimitItem, selection, @on)
	    ->  get(I, member, 'Mb', IntItem),
		get(IntItem, selection, Mb),
		Kb is Mb * 1024
	    ;   Kb = 0
	    ),
	    stack(Stack, _, KeyName),
	    registry_set_key(current_user/'Software'/'SWI'/'Prolog',
			     KeyName, Kb)
	;   true
	).

:- pce_end_class.
