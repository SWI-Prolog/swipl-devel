/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Module PCE.  This module defines the core   of  XPCE.  It is designed in
such a way that it  may  be   compiled  using  the SWI-Prolog qcompile/1
compiler, which makes XPCE an autoloadable module of SWI-Prolog.

Various things are Prolog-implementation specific in this module and
therefore each Prolog system will require a different version of this
module.

This module only defines some  paths,  some   things  to  make  the .qlf
compiler work on it and  finally  it   just  loads  the XPCE modules and
reexports the content of these files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(pce,
	  [ new/2, free/1,		% pce_principal predicates

	    send/2, send/3, send/4, send/5, send/6, send/7,
	    send/8, send/9, send/10, send/11, send/12,

	    get/3, get/4, get/5, get/6, get/7, get/8,
	    get/9, get/10, get/11, get/12, get/13,

	    send_class/3,
	    get_class/4,
	    object/1, object/2,

	    pce_global/2,		% pce_global
	    pce_autoload/2,		% pce_autoload
	    pce_autoload_all/0,

	    pce_term_expansion/2,
	    pce_compiling/1,
	    pce_begin_recording/1,
	    pce_end_recording/0,

	    pce_register_class/1,
	    pce_extended_class/1,
	    pce_prolog_class/1,
	    pce_prolog_class/2,

	    pce_catch_error/2,		% pce_error
	    pce_open/3
	  ]).
:- use_module(library(lists), [append/3]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

		/********************************
		*      LOAD COMMON PLATFORM	*
		********************************/

%	At least the library must be absolute for require/1 to work
%	properly.

set_paths :-
	current_predicate(_, user:xpce_trial_run), !,
	absolute_file_name('../../..', PceDir),
	assert(user:file_search_path(pce, PceDir)),
	absolute_file_name('..', HereBootDir),
	absolute_file_name(pce('prolog/lib'), LibDir),
	assert(user:file_search_path(pce_boot, HereBootDir)),
	assert(user:file_search_path(pce_boot, pce('prolog/boot'))),
	absolute_file_name('.', HereLibDir),
	asserta(user:library_directory(LibDir)),
	asserta(user:library_directory(HereLibDir)).
set_paths :-
	absolute_file_name('../..', PceDir),
	assert(user:file_search_path(pce, PceDir)),
	assert(user:file_search_path(pce_boot, pce('prolog/boot'))).

:- set_paths.

:- load_files([ pce_boot(pce_expand),
		pce_boot(pce_sicstus),
		pce_boot(pce_principal),
		pce_boot(pce_error),
		pce_boot(pce_operator),
		pce_boot(pce_global),
		pce_boot(pce_expansion),
		pce_boot(pce_realise),
		pce_boot(pce_goal_expansion),
		pce_boot(pce_autoload),
		pce_boot(pce_editor),
		pce_boot(pce_portray)
	      ],
	      [ %silent(true)
	      ]).


set_version :-
	prolog_flag(version, PlId),
	atom_chars(PlId, Chars),
	append(" (", _, Rest),
	append(V0, Rest, Chars), !,
	atom_chars(Id, V0),
	send(@prolog, system, Id).
set_version :-
	send(@prolog, system, 'SICStus').

add_version :-
	get(@pce, version, Version),
	get(string('XPCE %s, Copyright (C) University of Amsterdam', Version),
	    value, V),
	version(V).

:- add_version.
:- initialization
   set_version.

:- multifile
	user:user_help/0.
:- require(auto_call/1).

user:user_help :-
	auto_call(manpce).

		/********************************
		*       PROLOG LIBRARIES	*
		********************************/

user:file_search_path(demo,    pce('prolog/demo')).
user:file_search_path(contrib, pce('prolog/contrib')).
user:file_search_path(image,   pce(bitmaps)).
