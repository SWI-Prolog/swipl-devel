/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Help the wise installation process
*/

:- module(wise_install,
	  [ wise_install/0,
	    wise_install_xpce/0
	  ]).
:- use_module(library(progman)).
:- use_module(library(registry)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Helper module to get SWI-Prolog and   XPCE  installed properly under the
Wise installation shield. This module is  *not* for end-users (but might
be useful as an example for handling some Windows infra-structure).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

get_wise_variable(Name, Value) :-
	open_dde_conversation('WiseInst', Name, Handle),
	dde_request(Handle, -, RawVal),
	close_dde_conversation(Handle),
	Value = RawVal.

ensure_group(Group) :-
	progman_groups(ExistingGroups),
	(   member(Group, ExistingGroups)
	->  true
	;   progman_make_group(Group, swi)
	).


wise_install :-
	(   get_wise_variable('EXT', Ext),
	    Ext \== ''
	->  shell_register_prolog(Ext),
	    current_prolog_flag(argv, [Me|_]),
	    format('Registered "~w" files to start ~w~n', [Ext, Me])
	;   true
	),
	(   get_wise_variable('GROUP', Group),
	    get_wise_variable('PLCWD', Cwd),
	    Cwd \== '',
	    Group \== ''
	->  Item = 'SWI-Prolog',
	    format('Installing icons in group ~w, for CWD=~w~n', [Group, Cwd]),
	    ensure_group(Group),
	    format('Created group ~w~n', [Group]),
	    current_prolog_flag(executable, PlFileName),
	    prolog_to_os_filename(PlFileName, CmdLine0),
	    concat_atom(['"', CmdLine0, '"'], CmdLine),
	    format('Commandline = ~w~n', [CmdLine]),
	    progman_make_item(Group, Item, CmdLine, Cwd)
	;   true
	), !,
	wise_install_xpce,
	format('~N~nAll done.', []),
	sleep(2),
	halt(0).
wise_install :-
	halt(1).

wise_install_xpce :-			% no XPCE around
	\+ absolute_file_name(swi(xpce),
			      [ access(exist),
				file_type(directory),
				file_errors(fail)
			      ], _), !.
wise_install_xpce :-
	qcompile_pce,
	qcompile_lib,
	halt(0).
wise_install_xpce :-
	halt(1).
	
		 /*******************************
		 *	 PRECOMPILED PARTS	*
		 *******************************/

qmodule(pce, library(pce)).
qmodule(lib, library(pce_manual)).
qmodule(lib, library(pcedraw)).
qmodule(lib, library('emacs/emacs')).
qmodule(lib, library('dialog/dialog')).
qmodule(lib, library('trace/trace')).

qcompile_pce :-
	set_prolog_flag(character_escapes, false),
	format('Checking library-index~n'),
	make,
	qcompile(library(pce)).

qcompile_lib :-
	format('Recompiling modules~n'),
	qmodule(lib, Module),
	format('~*c~n', [64, 0'*]),
	format('* Qcompile module ~w~n', [Module]),
	format('~*c~n', [64, 0'*]),
	once(qcompile(Module)),
	fail.
qcompile_lib.



