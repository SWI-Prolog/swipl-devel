/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_error,
	  [ pce_catch_error/2
	  ]).

:- meta_predicate
	pce_catch_error(+, :).

:- use_module(pce_boot(pce_principal)).


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
