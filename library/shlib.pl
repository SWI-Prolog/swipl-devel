/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(shlib,
	  [ load_foreign_library/1,	% :LibFile
	    load_foreign_library/2,	% :LibFile, +InstallFunc
	    unload_foreign_library/1,	% +LibFile
	    unload_foreign_library/2,	% +LibFile, +UninstallFunc
	    current_foreign_library/2,	% ?LibFile, ?Public
	    reload_foreign_libraries/0
	  ]).
:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
	load_foreign_library/1,
	load_foreign_library/2.

:- dynamic
	loading/1,			% Lib
	error/2,			% File, Error
	foreign_predicate/2,		% Lib, Pred
	current_library/5.		% Lib, Entry, Path, Module, Handle

:- volatile				% Do not store in state
	loading/1,
	error/2,
	foreign_predicate/2,
	current_library/5.

:- (   current_prolog_flag(open_shared_object, true)
   ->  true
   ;   print_message(warning, shlib(not_supported)) % error?
   ).


		 /*******************************
		 *	     DISPATCHING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows: If libpl.dll is compiled for debugging, prefer loading <lib>D.dll
to allow for debugging.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

find_library(Spec, Lib) :-
	current_prolog_flag(windows, true),
	current_prolog_flag(kernel_compile_mode, debug),
	libd_spec(Spec, SpecD),
	catch(find_library2(SpecD, Lib), _, fail).
find_library(Spec, Lib) :-
	find_library2(Spec, Lib).

find_library2(Spec, Lib) :-
	absolute_file_name(Spec,
			   [ file_type(executable),
			     access(read),
			     file_errors(fail)
			   ], Lib), !.
find_library2(Spec, Spec) :-
	atom(Spec), !.			% use machines finding schema
find_library2(foreign(Spec), Spec) :-
	atom(Spec), !.			% use machines finding schema
find_library2(Spec, _) :-
	throw(error(existence_error(source_sink, Spec), _)).

libd_spec(Name, NameD) :-
	atomic(Name),
	file_name_extension(Base, Ext, Name),
	atom_concat(Base, 'D', BaseD),
	file_name_extension(BaseD, Ext, NameD).
libd_spec(Spec, SpecD) :-
	compound(Spec),
	Spec =.. [Alias,Name],
	libd_spec(Name, NameD),
	SpecD =.. [Alias,NameD].
libd_spec(Spec, Spec).			% delay errors

base(Path, Base) :-
	atomic(Path), !,
	file_base_name(Path, File),
	file_name_extension(Base, _Ext, File).
base(Path, Base) :-
	Path =.. [_,Arg],
	base(Arg, Base).
	
entry(_, Function, Function) :-
	Function \= default(_), !.
entry(Spec, default(FuncBase), Function) :-
	base(Spec, Base),
	concat_atom([FuncBase, Base], '_', Function).
entry(_, default(Function), Function).

		 /*******************************
		 *	    (UN)LOADING		*
		 *******************************/

load_foreign_library(Library) :-
	load_foreign_library(Library, default(install)).

load_foreign_library(LibFileSpec, Entry) :-
	strip_module(LibFileSpec, Module, LibFile),
	with_mutex('$foreign',
		   shlib:load_foreign_library(LibFile, Module, Entry)).

load_foreign_library(LibFile, _Module, _) :-
	current_library(LibFile, _, _, _, _), !.
load_foreign_library(LibFile, Module, DefEntry) :-
	retractall(error(_, _)),
	find_library(LibFile, Path),
	asserta(loading(LibFile)),
	catch(Module:open_shared_object(Path, Handle), E, true),
	(   nonvar(E)
	->  assert(error(Path, E)),
	    fail
	;   true
	), !,
	(   (	entry(LibFile, DefEntry, Entry),
		Module:call_shared_object_function(Handle, Entry)
	    ->	true
	    ;	DefEntry == default(install)
	    )
	->  retractall(loading(LibFile)),
	    assert_shlib(LibFile, Entry, Path, Module, Handle)
	;   retractall(loading(LibFile)),
	    close_shared_object(Handle),
	    print_message(error, shlib(LibFile, call_entry(DefEntry))),
	    fail
	).
load_foreign_library(LibFile, _, _) :-
	retractall(loading(LibFile)),
	(   error(_Path, E)
	->  retractall(error(_, _)),
	    throw(E)
	;   throw(error(existence_error(foreign_library, LibFile), _))
	).

unload_foreign_library(LibFile) :-
	unload_foreign_library(LibFile, default(uninstall)).

unload_foreign_library(LibFile, DefUninstall) :-
	with_mutex('$foreign', do_unload(LibFile, DefUninstall)).

do_unload(LibFile, DefUninstall) :-
	current_library(LibFile, _, _, Module, Handle),
	retractall(current_library(LibFile, _, _, _, _)),
	(   entry(LibFile, DefUninstall, Uninstall),
	    Module:call_shared_object_function(Handle, Uninstall)
	->  true
	;   true
	),
	abolish_foreign(LibFile),
	close_shared_object(Handle).
	    
abolish_foreign(LibFile) :-
	(   retract(foreign_predicate(LibFile, Module:Head)),
	    functor(Head, Name, Arity),
	    abolish(Module:Name, Arity),
	    fail
	;   true
	).

system:'$foreign_registered'(M, H) :-
	(   loading(Lib)
	->  true
	;   Lib = '<spontaneous>'
	),
	assert(foreign_predicate(Lib, M:H)).

assert_shlib(File, Entry, Path, Module, Handle) :-
	retractall(current_library(File, _, _, _, _)),
	asserta(current_library(File, Entry, Path, Module, Handle)).


		 /*******************************
		 *	 ADMINISTRATION		*
		 *******************************/

%	current_foreign_library(?File, ?Public)
%
%	Query currently loaded shared libraries

current_foreign_library(File, Public) :-
	current_library(File, _Entry, _Path, _Module, _Handle),
	findall(Pred, foreign_predicate(File, Pred), Public).


		 /*******************************
		 *	      RELOAD		*
		 *******************************/

%	reload_foreign_libraries
%
%	Reload all foreign libraries loaded (after restore of a state
%	craeted using qsave_program/2.

reload_foreign_libraries :-
	findall(lib(File, Entry, Module),
		(   retract(current_library(File, Entry, _, Module, _)),
		    File \== -
		),
		Libs),
	reverse(Libs, Reversed),
	reload_libraries(Reversed).

reload_libraries([]).
reload_libraries([lib(File, Entry, Module)|T]) :-
	(   load_foreign_library(File, Module, Entry)
	->  true
	;   print_message(error, shlib(File, load_failed))
	),
	reload_libraries(T).


		 /*******************************
		 *     CLEANUP (WINDOWS ...)	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from Halt() in pl-os.c (if it  is defined), *after* all at_halt/1
hooks have been executed, and after   dieIO(),  closing and flushing all
files has been called.

On Unix, this is not very useful, and can only lead to conflicts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unload_all_foreign_libraries :-
	current_prolog_flag(unix, true), !.
unload_all_foreign_libraries :-
	forall(current_library(File, _, _, _, _),
	       unload_foreign(File)).

%	unload_foreign(+File)
%	
%	Unload the given foreign file and all `spontaneous' foreign
%	predicates created afterwards. Handling these spontaneous
%	predicates is a bit hard, as we do not know who created them and
%	on which library they depend.

unload_foreign(File) :-
	unload_foreign_library(File),
	(   clause(foreign_predicate(Lib, M:H), true, Ref),
	    (	Lib == '<spontaneous>'
	    ->	functor(H, Name, Arity),
		abolish(M:Name, Arity),
		erase(Ref),
		fail
	    ;	!
	    )
	->  true
	;   true
	).

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(shlib(LibFile, call_entry(DefEntry))) -->
	[ '~w: Failed to call entry-point ~w'-[LibFile, DefEntry] ].
prolog:message(shlib(LibFile, load_failed)) -->
	[ '~w: Failed to load file'-[LibFile] ].
prolog:message(shlib(not_supported)) -->
	[ 'Emulator does not support foreign libraries' ].
