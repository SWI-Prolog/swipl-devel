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
	pce_manual_index,
	halt(0).
wise_install_xpce :-
	halt(1).
	
:- set_feature(character_escapes, true).

pce_manual_index :-
	send(@display, confirm,
	     'I can now create the index for the XPCE manual tools\n\n\
	      This process takes several minutes and requires about 2MB\n\
	      disk-space.\n\n\
	      If you do not create the index now, you will be prompted\n\
	      when you use the manual search tool for the first time.\n\n\
	      Create the Index now?'),
	manpce,
	absolute_file_name(pce('/man/reference/index'),
			   [ extensions([obj]),
			     access(write)],
			   IndexFile), !,
	get(new(man_index_manager), make_index, IndexFile, _), !.
pce_manual_index.

:- set_feature(character_escapes, false).

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



