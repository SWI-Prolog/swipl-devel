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

get_wise_variable(Name, Value) :-
	open_dde_conversation('WiseInst', Name, Handle),
	dde_request(Handle, -, RawVal),
	close_dde_conversation(Handle),
	Value = RawVal.

set_wise_variable(Name, Value) :-
	open_dde_conversation('WiseInst', Name, Handle),
	dde_execute(Handle, -, Value),
	close_dde_conversation(Handle).


ensure_group(Group) :-
	progman_groups(ExistingGroups),
	(   member(Group, ExistingGroups)
	->  true
	;   progman_make_group(Group, swi)
	).


wise_install :-
	shell_register_prolog,
	(   get_wise_variable('GROUP', GroupString),
	    get_wise_variable('PLCWD', CwdString),
	    string_to_atom(GroupString, Group),
	    string_to_atom(CwdString, Cwd),
	    Cwd \== ''
	->  Item = 'SWI-Prolog',
	    format('Installing icons in group ~w, for CWD=~w~n', [Group, Cwd]),
	    ensure_group(Group),
	    feature(symbol_file, PlFileName),
	    prolog_to_os_filename(PlFileName, CmdLine0),
	    concat_atom(['"', CmdLine0, '"'], CmdLine),
	    progman_make_item(Group, Item, CmdLine, Cwd)
	;   true
	), !,
	make,				% update all library indexes
	halt(0).
wise_install :-
	halt(1).

wise_install_xpce :-
	qcompile_pce,
	qcompile_lib,
	(   get_wise_variable('GROUP', GroupString),
	    get_wise_variable('PLCWD', CwdString),
	    string_to_atom(GroupString, Group),
	    string_to_atom(CwdString, Cwd),
	    Cwd \== ''
	->  Item = 'XPCE',
	    format('Installing icons in group ~w, for CWD=~w~n', [Group, Cwd]),
	    ensure_group(Group),
	    feature(symbol_file, PlFileName),
	    prolog_to_os_filename(PlFileName, Prog),
	    concat_atom(['"', Prog, '" -- -pce'], CmdLine),
	    progman_make_item(Group, Item, CmdLine, Cwd, Prog:1)
	;   true
	), !,
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

qcompile_pce :-
	set_feature(character_escapes, false),
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



