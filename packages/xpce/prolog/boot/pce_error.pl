/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_error,
	  [ pce_catch_error/2
	  ]).

:- meta_predicate
	pce_catch_error(+, :).

:- use_module(pce_principal).


%	pce_catch_error(?Errors, Goal)
%
%	Run goal, fail silently on indicated errors.  If the first argument
%	is a variable, any error will be catched.

pce_catch_error(Error, Goal) :-
	var(Error), !,
	send(@pce, catch_error, @default),
	(   Goal
	->  send(@pce, catch_pop)
	;   send(@pce, catch_pop),
	    fail
	).
pce_catch_error(Errors, Goal) :-
	send(@pce, catch_error, Errors),
	(   Goal
	->  send(@pce, catch_pop)
	;   send(@pce, catch_pop),
	    fail
	).
