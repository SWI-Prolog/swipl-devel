/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Front-end for loading shared libraries
*/

:- module(shlib,
	  [ load_foreign_library/1,	% +LibFile
	    load_foreign_library/2,	% +LibFile, +InstallFunc
	    unload_foreign_library/1,	% +LibFile
	    unload_foreign_library/1,	% +LibFile, +UninstallFunc
	    current_foreign_library/2,	% ?LibFile, ?Public
	    find_and_open_shared_object/3
	  ]).

:- module_transparent
	load_foreign_library/1,
	load_foreign_library/2.

:- dynamic
	current_library/5,		% Lib x Entry x Path x Handle x Public
	fpublic/1.


:- (   (feature(dll, true) ; feature(open_shared_object, true))
   ->  true
   ;   '$warning'('library(shlib): Need .dll or .so based interface')
   ).

		 /*******************************
		 *	     DISPATCHING	*
		 *******************************/

open_goal(Lib, -, Handle, open_dll(Lib, Handle)) :-
	feature(dll, true), !.
open_goal(Lib, Path, Handle, find_and_open_shared_object(Lib, Path, Handle)) :-
	feature(open_shared_object, true).

call_goal(Handle, Function, call_dll_function(Handle, Function)) :-
	feature(dll, true), !.
call_goal(Handle, Function, call_shared_object_function(Handle, Function)) :-
	feature(open_shared_object, true), !.

close_goal(Handle, close_dll(Handle)) :-
	feature(dll, true), !.
close_goal(Handle, close_shared_object(Handle)) :-
	feature(open_shared_object, true), !.


		 /*******************************
		 *     DLOPEN() SEARCH STUFF	*
		 *******************************/

:- module_transparent
	find_and_open_shared_object/3.

find_and_open_shared_object(Lib, Path, Handle) :-
	'$chk_file'(Lib, ['.so'], Path), !,
	open_shared_object(Path, Handle).
find_and_open_shared_object(Lib, _Path, _Handle) :-
	'$warning'('open_shared_object/2: Cannot find ~w: no such file', [Lib]),
	fail.


		 /*******************************
		 *	    (UN)LOADING		*
		 *******************************/

load_foreign_library(Library) :-
	load_foreign_library(Library, install).

load_foreign_library(LibFile, Entry) :-
	current_library(LibFile, Entry, _, _, _), !.
load_foreign_library(LibFile, Entry) :-
	open_goal(LibFile, Path, Handle, OpenGoal),
	OpenGoal,
	(   retractall(fpublic(_)),
	    call_goal(Handle, Entry, CallGoal),
	    CallGoal
	->  assert_shlib(LibFile, Entry, Path, Handle),
	    retractall(fpublic(_))
	;   '$warning'('~w: failed to call entry point ~w', [LibFile, Entry]),
	    close_goal(Handle, CloseGoal),
	    CloseGoal,
	    fail
	).

unload_foreign_library(LibFile) :-
	unload_foreign_library(LibFile, uninstall).

unload_foreign_library(LibFile, Uninstall) :-
	current_library(LibFile, _, _, Public, Handle),
	retractall(current_library(LibFile, _, _, _, _)),
	call_goal(Handle, Uninstall, CallGoal),
	ignore(CallGoal),
	forall(member(Module:Head, Public),
	       (   functor(Head, Name, Arity),
		   abolish(Module:Name, Arity)
	       )),
	close_goal(Handle, CloseGoal),
	CloseGoal.
	    
:- system:asserta(('$foreign_registered'(M, H) :-
		  	shlib:foreign_registered(M, H))).

foreign_registered(M, H) :-
	assert(fpublic(M:H)).

assert_shlib(File, Entry, Path, Handle) :-
	findall(P, fpublic(P), Public),
	retractall(current_library(File, _, _, _, _)),
	asserta(current_library(File, Entry, Path, Public, Handle)).


		 /*******************************
		 *	 ADMINISTRATION		*
		 *******************************/

%	current_foreign_library(?File, ?Public)
%
%	Query currently loaded shared libraries

current_foreign_library(File, Public) :-
	current_library(File, _Entry, _Path, Public, _Handle).

		 /*******************************
		 *     CLEANUP (WINDOWS ...)	*
		 *******************************/

:- at_halt(unload_all_foreign_libraries).

unload_all_foreign_libraries :-
	forall(current_foreign_library(File, _),
	       unload_foreign_library(File)).
