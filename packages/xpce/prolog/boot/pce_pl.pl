/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_host,
	[ '$load_pce'/0
	, require/1
	, (meta_predicate)/1
	, '$call_atom'/1
	]).


:- module_transparent
	strip_module/3,
	'$load_pce'/0,
	require/1,
	auto_call/1.


:- use_module(library(quintus), [(meta_predicate)/1]).

		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

property(prolog(swi)).			% this is SWI-Prolog
property(file_extensions([pl])).	% list of file extensions
property(use_predicate_references).	% use direct predicate refs in methods
property(register_source_locations).	% register the source locations
property(string).			% Supports string datatype
property(runtime) :-
	get(@(pce), is_runtime_system, @(on)).
	

		/********************************
		*         STRIP_MODULE		*
		********************************/

strip_module(Raw, Module, Term) :-
	'$strip_module'(Raw, Module, Term).



		/********************************
		*            REQUIRE		*
		********************************/

require([]) :- !.
require([H|T]) :- !,
	require(H),
	require(T).
require(_Name/_Arity) :- !.
require(Term) :-
	'$warning'('require/1: malformed argument: ~w', [Term]).


		/********************************
		*      DEBUGGER SUPPORT		*
		********************************/

%	$call_atom(+Atom)
%	Transform `Atom' into a term and call it in the user module.
%	Used to implement the `@' option in the PCE tracer.

'$call_atom'(Atom) :-
	term_to_atom(Term, Atom),
	user:Term.


		/********************************
		*             ENTRY		*
		********************************/

pce_home(PceHome) :-
	absolute_file_name(pce('.'), [file_type(directory)], PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	getenv('XPCEHOME', PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	feature(home, PlHome),
	(   feature(xpce_version, Version),
	    concat('/xpce-', Version, Suffix)
	;   Suffix = '/xpce'
	),
	concat(PlHome, Suffix, RawHome),
	exists_directory(RawHome), !,
	absolute_file_name(RawHome, PceHome).
pce_home(_) :-
	$warning('Cannot find XPCE home directory'),
	halt(1).

'$load_pce' :-
	'$c_current_predicate'('$pce_init', user:'$pce_init'(_)), !,
	(   pce_home(PceHome),
	    pce_principal:'$pce_init'(PceHome),
	    set_feature(xpce, true)
	->  true
	;   format(user_error, '[PCE ERROR: Failed to initialise XPCE]~n'),
	    halt(1)
	).
'$load_pce' :-
	(   feature(dll, true)
	;   feature(open_shared_object, true),
	    push_library_dir
	), !,
	(   load_foreign_library(pce_principal:foreign(pl2xpce))
	->  true
	;   format(user_error,
		   '[PCE ERROR: Failed to load XPCE foreign library]~n'),
	    halt(1)
	),
	(   absolute_file_name(pce(.), [file_type(directory)], PceHome),
	    pce_principal:'$pce_init'(PceHome)
	->  true
	;   format(user_error, '[PCE ERROR: Failed to initialise XPCE]~n'),
	    halt(1)
	),
	set_feature(xpce, true), !.

%	Pushes LD_LIBRARY_PATH, so -lXPCE will be resolved correctly.
%	The LD_ELF_LIBRARY_PATH is preferred on Linux systems.  It is
%	unlikely to give problems on other systems.

push_library_dir :-
	(   getenv('LD_ELF_LIBRARY_PATH', OldPath)
	->  Var = 'LD_ELF_LIBRARY_PATH'
	;   (   getenv('LD_LIBRARY_PATH', OldPath)
	    ->	true
	    ;	OldPath = ''
	    )
	->  Var = 'LD_LIBRARY_PATH'
	),
	absolute_file_name(foreign(pl2xpce), LibFile),
	file_directory_name(LibFile, LibDirSlash),
	(   concat(LibDir, /, LibDirSlash)
	->  true
	;   LibDir = LibDirSlash
	),
	(   OldPath \== ''
	->  concat_atom([LibDir, :, OldPath], Path)
	;   Path = LibDir
	),
	setenv(Var, Path).
%	format(~w=~w~n', [Var, Path]).


		 /*******************************
		 *	  CONSULT-SUPPORT	*
		 *******************************/

%	callable_predicate(+Module:+Head)
%	True if Head is a callable predicate in Module.

callable_predicate(Module:Head) :-
	default_module(Module, DefModule),
	current_predicate(_, DefModule:Head), !.
callable_predicate(_:Head) :-
	functor(Head, Name, Arity),
	'$in_library'(Name, Arity).

default_module(M, M).
default_module(M, user) :- M \== user.
default_module(M, system) :- M \== system.

%	modified_since_last_loaded(Path)
%	True is file has been modified since the last time it was loaded.

modified_since_last_loaded(File) :-
	'$time_source_file'(File, LoadTime), !,
	time_file(File, Modified),
	Modified @> LoadTime.
modified_since_last_loaded(InFile) :-
	'$time_source_file'(File, LoadTime),
	same_file(InFile, File), !,
	time_file(File, Modified),
	Modified @> LoadTime.
