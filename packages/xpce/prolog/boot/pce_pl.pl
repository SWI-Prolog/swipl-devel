/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_host,
	[ '$load_pce'/0
	, (meta_predicate)/1
	, '$call_atom'/1
	]).


:- module_transparent
	strip_module/3,
	'$load_pce'/0.

:- use_module(library(quintus), [(meta_predicate)/1]).

		 /*******************************
		 *	    EXPANSION		*
		 *******************************/

user:term_expansion((:- require(_)), []).


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
		*      DEBUGGER SUPPORT		*
		********************************/

%	$call_atom(+Atom)
%	Transform `Atom' into a term and call it in the user module.
%	Used to implement the `@' option in the PCE tracer.

'$call_atom'(Atom) :-
	term_to_atom(Term, Atom),
	user:Term.

		 /*******************************
		 *	     ERRORS		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(T) -->
	{ current_module(pce_messages) }, % avoid problem while booting
	pce_messages:pce_message(T).

		/********************************
		*             ENTRY		*
		********************************/

pce_home(PceHome) :-
	absolute_file_name(pce('.'),
			   [ file_type(directory),
			     file_errors(fail)
			   ], PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	getenv('XPCEHOME', PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	(   current_prolog_flag(xpce_version, Version),
	    atom_concat('/xpce-', Version, Suffix)
	;   Suffix = '/xpce'
	),
	absolute_file_name(swi(Suffix),
			   [ file_type(directory),
			     file_errors(fail)
			   ], PceHome),
	exists_directory(PceHome), !.
pce_home(PceHome) :-
	current_prolog_flag(saved_program, true), !,
	(   current_prolog_flag(home, PceHome)
	->  true
	;   current_prolog_flag(symbol_file, Exe)
	->  file_directory_name(Exe, PceHome)
	;   PceHome = '.'
	).
pce_home(_) :-
	print_message(error, format('Cannot find XPCE home directory', [])),
	halt(1).

'$load_pce' :-
	'$c_current_predicate'('$pce_init', user:'$pce_init'(_)), !,
	init_pce.
'$load_pce' :-
	current_prolog_flag(open_shared_object, true),
	(   load_foreign_library(pce_principal:foreign(pl2xpce))
	->  true
	;   print_message(error,
			  format('Failed to load XPCE foreign library', [])),
	    halt(1)
	),
	init_pce.

init_threads :-
	current_prolog_flag(threads, true), !,
	(   pce_principal:send(@pce, multi_threading(@on))
	->  true
	;   print_message(warning, pce(no_threads))
	).
init_threads.

init_pce :-
	(   pce_home(PceHome),
	    pce_principal:'$pce_init'(PceHome)
	->  set_prolog_flag(xpce, true),
	    init_threads
	;   print_message(error,
			  format('Failed to initialise XPCE]', [])),
	    halt(1)
	).
