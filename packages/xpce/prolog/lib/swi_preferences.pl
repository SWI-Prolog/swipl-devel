/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_preferences,
	  [ prolog_edit_preferences/1	% +What
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).

:- pce_help_file(plprefs, pce_help('plprefs.hlp')).

prolog_edit_preferences(stack_sizes) :-
	send(new(prolog_preferences), open_centered).
prolog_edit_preferences(What) :-
	locate_preferences(What, File),
	auto_call(start_emacs),
	(   \+ access_file(File, exist)
	->  send(@display, confirm,
		 'Preferences file %s doesn''t exist.\nCreate it?', File)
	;   access_file(File, write)
	->  true
	    ;   send(@display, inform,
		     'You cannot modify the preferences file %s', File)
	),
	send(@emacs, goto_source_location, File).

locate_preferences(xpce, File) :-
	get(@pce, home, Home),
	get(string('%s/Defaults', Home), value, File).
locate_preferences(xpce_user, File) :-
	ensure_xpce_config_dir(Dir),
	get(string('%s/Defaults', Dir), value, File).
locate_preferences(prolog, File) :-
	'$option'(init_file, Base, Base), % should be in current_prolog_flag!
	member(Access, [read, write]),
	absolute_file_name(Base, Here),
	absolute_file_name(user_profile(Base),
			   [ access(Access),
			     file_errors(fail),
			     solutions(all)
			   ], File),
	File \== Here, !.
	

ensure_xpce_config_dir(Dir) :-
	catch(expand_file_name('~/.xpce', [Dir]), _, fail),
	new(D, directory(Dir)),
	(   send(D, exists)
	->  true
	;   send(D, make)
	).


		 /*******************************
		 *	    STACK SIZES		*
		 *******************************/

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
