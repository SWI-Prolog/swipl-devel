/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Manipulate system predicates and system mode
*/

:- module(system,
	[ lock_predicate/2
	, unlock_predicate/2
	, system_mode/1
	, system_module/0
	]).

:- style_check(+dollar).

%	system_mode(+OnOff)
%	Switch the system into system or user mode.  When in system mode,
%	system predicates loose most of their special properties, so it
%	becomes possible to trace and even redefine them.  Use the latter
%	with care as the system predicates call one another.  This should
%	once be fixed by defining all of them in a module ($system), so
%	the user can savely remove them from module user.

system_mode(on) :-
	style_check(+dollar).
system_mode(off) :-
	style_check(-dollar).

%	system_module
%	Any predicate defined after this declaraction uptill the end of
%	the file will become a system predicate. Normally invoked by a
%	directive immediately following the module declaration.

system_module :-
	system_mode(on).

:- module_transparent
	lock_predicate/2,
	unlock_predicate/2.

%	lock_predicate(+Name, Arity)
%	Transform a predicate into a system predicate. 

lock_predicate(Spec, Arity) :-
	$strip_module(Spec, Module, Name),
	functor(Head, Name, Arity ),
	$predicate_attribute(Module:Head, system, 1).

%	unlock_predicate(+Name, Arity)
%	Transform a system predicate into a normal system predicate.

unlock_predicate(Spec, Arity) :-
	$strip_module(Spec, Module, Name),
	functor(Head, Name, Arity ),
	$predicate_attribute(Module:Head, system, 0).
