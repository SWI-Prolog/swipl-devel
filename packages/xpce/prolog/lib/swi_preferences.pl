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
		 'Preferences file %s doesn''t exist.\nCreate it?', File),
	    (	default_preferences(What, DefFile)
	    ->	copy_file(DefFile, File)
	    ;	true
	    )
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
	absolute_file_name(user_profile(Base),
			   [ access(Access),
			     file_errors(fail),
			     solutions(all)
			   ], File),
	(   Access == read
	;   valid_location(File)
	), !.
	
%	valid_location(+PrefFile)
%	
%	See whether this is a valid non-local location for the
%	preferences file. This should deal with the possibility that our
%	working directory is ~ or the Prolog home.

valid_location(PrefFile) :-
	file_directory_name(PrefFile, Dir),
	(   \+ absolute_file_name('.', Dir)
	->  true
	;   expand_file_name(~, [Dir])
	->  true
	;   current_prolog_flag(windows, true),
	    current_prolog_flag(home, Dir)
	).

%	default_preferences(+Id, -File)
%
%	If there is a default file for the preferences, return a path to
%	it, so the user can be presented a starting point.

default_preferences(prolog, File) :-
	member(Location, 
	       [ swi('custom/pl.ini'),
		 swi('dotfiles/dotplrc')
	       ]),
	absolute_file_name(Location,
			   [ access(read),
			     file_errors(fail)
			   ], File), !.
default_preferences(xpce_user, File) :-
	absolute_file_name(pce('Defaults.user'),
			   [ access(read),
			     file_errors(fail)
			   ], File), !.


ensure_xpce_config_dir(Dir) :-
	catch(expand_file_name('~/.xpce', [Dir]), _, fail),
	new(D, directory(Dir)),
	(   send(D, exists)
	->  true
	;   send(D, make)
	).


copy_file(From, To) :-
	send(file(To), copy, From).


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
		(   get(IntItem, selection, Mb)
		->  Kb is Mb * 1024
		;   send(I, report, error, 'Limit out of range (1..128 MB)'),
		    fail
		)
	    ;   Kb = 0
	    ),
	    stack(Stack, _, KeyName),
	    registry_set_key(current_user/'Software'/'SWI'/'Prolog',
			     KeyName, Kb)
	;   true
	).

:- pce_end_class.
