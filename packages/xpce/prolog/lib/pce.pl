/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
	    pce_compiling/1,		% -Class
	    pce_compiling/2,		% -Class, -Path
	    pce_begin_recording/1,
	    pce_end_recording/0,

	    pce_register_class/1,
	    pce_extended_class/1,
	    pce_prolog_class/1,
	    pce_prolog_class/2,

	    pce_catch_error/2,		% pce_error
	    pce_open/3,

	    pce_welcome/0
	  ]).

:- system_module.

		/********************************
		*      LOAD COMMON PLATFORM	*
		********************************/

:- prolog_load_context(directory, Dir),
   concat(Dir, '/../boot', RawBootDir),
   absolute_file_name(RawBootDir, BootDir),
   assert(user:file_search_path(pce_boot, BootDir)).

:- [ pce_boot(pce_expand),
     pce_boot(pce_pl),
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
   ].


set_version :-
	current_prolog_flag(version, PlVersion),
	send(@pce, catch_error_signals, @on),
	Major is PlVersion // 10000,
	Minor is (PlVersion // 100) mod 100,
	Patch is PlVersion mod 100,
	sformat(PlId, 'SWI-Prolog version ~w.~w.~w', [Major, Minor, Patch]),
	send(@prolog, system, PlId).

:- initialization set_version.
   
get_pce_version :-
	(   current_prolog_flag(xpce_version, _)
	->  true
	;   get(@pce, version, name, Version),
	    set_prolog_flag(xpce_version, Version)
	).

:- initialization get_pce_version.


		 /*******************************
		 *	     CONSOLE		*
		 *******************************/

%:- send(@pce, console_label, 'XPCE/SWI-Prolog').


		/********************************
		*       PROLOG LIBRARIES	*
		********************************/

:- multifile
	user:file_search_path/2.

user:file_search_path(demo,    pce('prolog/demo')).
user:file_search_path(contrib, pce('prolog/contrib')).
user:file_search_path(image,   pce(bitmaps)).


		 /*******************************
		 *	   EDIT HOOKS		*
		 *******************************/

%	make sure SWI-Prolog edit/0 loads the XPCE edit hooks.

:- multifile
	prolog_edit:load/0,
	prolog:locate_clauses/2.

prolog_edit:load :-
	ensure_loaded(library(swi_edit)).

		 /*******************************
		 *	    LIST HOOKS		*
		 *******************************/

%	prolog:locate_clauses(Term, Refs)
%
%	Locate a list of clause-references from a method-specification
%	like Class->Method.
%
%	see library(listing).

prolog:locate_clauses(Term, Refs) :-
	(   Term = ->(_,_)
	;   Term = <-(_,_)
	), !,
	findall(R, method_clause(Term, R), Refs).

match_id(->(Class, Method), Id) :-
	atomic(Class), atomic(Method), !,
	concat_atom([Class, (->), Method], Id).
match_id(->(_Class, _Method), _Id).
match_id(<-(Class, Method), Id) :-
	atomic(Class), atomic(Method), !,
	concat_atom([Class, (<-), Method], Id).
match_id(<-(_Class, _Method), _Id).

method_clause(->(Class, Send), Ref) :-
	match_id((Class->Send), Id),
	clause(pce_principal:send_implementation(Id, _M, _O), _B, Ref),
	atom(Id),
	concat_atom([Class,Send], '->', Id).
method_clause(<-(Class, Get), Ref) :-
	match_id(<-(Class, Get), Id),
	clause(pce_principal:get_implementation(Id, _M, _O, _R), _B, Ref),
	atom(Id),
	concat_atom([Class,Get], '->', Id).

       		 /*******************************
		 *	    DEBUG HOOKS		*
		 *******************************/

:- multifile
	prolog:debug_control_hook/1.

prolog:debug_control_hook(spy(Method)) :-
	auto_call(spypce(Method)).
prolog:debug_control_hook(nospy(Method)) :-
	auto_call(nospypce(Method)).

		 /*******************************
		 *	     HELP HOOK		*
		 *******************************/

:- multifile
	prolog:help_hook/1.

prolog:help_hook(help) :- !,
	auto_call(prolog_help(help/1)).
prolog:help_hook(apropose(What)) :- !,
	auto_call(prolog_apropos(What)).
prolog:help_hook(help(What)) :- !,
	(   auto_call(pce_to_method(What, Method))
	->  auto_call(manpce(Method))
	;   auto_call(prolog_help(What))
	).

		/********************************
		*            BANNER		*
		********************************/

pce_welcome :-
	current_prolog_flag(verbose, silent), !.
pce_welcome :-
	send(@pce, banner),
	(   get(@pce, is_runtime_system, @(off))
	->  format('~nFor HELP on prolog, please type help. or apropos(topic).~n'),
	    format('         on xpce, please type manpce.~n~n')
	;   format('~n', [])
	).


:- flag('$banner_goal', _, pce_welcome).
