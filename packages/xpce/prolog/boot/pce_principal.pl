/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_principal,
	  [ new/2, free/1

	  , send/2, send/3, send/4, send/5, send/6, send/7
	  , send/8, send/9, send/10, send/11, send/12

	  , get/3, get/4, get/5, get/6, get/7, get/8
	  , get/9, get/10, get/11, get/12, get/13

	  , object/1, object/2
	  , pce_predicate_reference/2

	  , '$pce_get_object'/4			  % to lib/pce_util.pl
	  ]).


:- meta_predicate
	send(+, :),
	send(+, :, +),
	send(+, :, +, +),
	send(+, :, +, +, +),
	send(+, :, +, +, +, +),
	send(+, :, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +, +),

	get(+, :, -),
	get(+, :, +, -),
	get(+, :, +, +, -),
	get(+, :, +, +, +, -),
	get(+, :, +, +, +, +, -),
	get(+, :, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, +, +, -),

	new(?, :),

	pce_predicate_reference(:, ?).


:- op(100, fx, @).
:- op(150, yfx, ?).
:- op(990, xfx, :=).

		/********************************
		*           LOAD C-PART		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The following predicate must be defined before loading this  file.  It
is  normally defined   in the   prolog-dependant   first  file of  the
interface, called pce_<prolog-name>.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_host:'$load_pce'.


		/********************************
		*          PROLOG LAYER		*
		********************************/


%   free(+Ref)
%   Delete object if it exists.

free(Ref) :-
	object(Ref), !,
	send(Ref, free).
free(_).


%   This file contains the predicates which make PCE-3 available to the
%   Prolog programmer.  The principal predicates define the interface between
%   Prolog and the PCE virtual machine instructions, the remaining predicates
%   are handy short-hands for a variety of common uses.
%
%   The PCE virtual machine contains three instructions: new (to create an
%   object), send (to manipulate an object) and get (to retrieve a value from
%   and object).  These instructions have complementary definitions in the
%   Prolog implementation.  This file assumes the complementary predicates
%   are called:
%
%	'$pce_new'(+@Object, +Description)
%	'$pce_send'(+@Object, +Selector, +Arguments)
%	'$pce_get'(+@Object, +Selector, +Arguments, -Value)
%
%   In addition the following predicates have been defined for convenience:
%
%	'$pce_object'(+@Object)
%	'$pce_object'(+@Object, -Description)
%	'$pce_get_object'(+@Object, +Selector, +Arguments, -Value)
%
%   [Technical note: '$pce_object'/[1, 2] is necessary given the current
%   implementation.  '$pce_get_object' could be defined in terms of '$pce_get'.]
%
%   Predicates which are basically similar except for multiple arguments
%   are hacked around (a little).  These predicates are defined as having at
%   most 10 (ten) arguments, and the arguments are packed in a single term
%   passed to the complementary interface predicates.  For example:
%
%	send(@window, free).
%	send(@view, print, hello).
%	send(@picture, display, circle(50), point(100, 100)).
%
%   become:
%
%	'$pce_send'(@window, free, arguments).
%	'$pce_send'(@view, print, arguments(hello)).
%	'$pce_send'(@picture, display, arguments(circle(50), point(100, 100))).

%   get(+@Object, +Selector, ...+Argument..., -Output)
%
%   Succeeds once if Output is the value returned by invoking get method
%   called Selector on Object.  Returns an object name, except for names, 
%   integers numbers and reals, which are returned as an object description.

/* Directly written in C
get(Obj, Sel, Out) :-
	'$pce_get'(Obj, Sel, arguments, Out).
get(Obj, Sel, A1, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1), Out).
get(Obj, Sel, A1, A2, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2), Out).
get(Obj, Sel, A1, A2, A3, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3), Out).
*/
get(Obj, Sel, A1, A2, A3, A4, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4), Out).
get(Obj, Sel, A1, A2, A3, A4, A5, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4, A5), Out).
get(Obj, Sel, A1, A2, A3, A4, A5, A6, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6), Out).
get(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7), Out).
get(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7, A8), Out).
get(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, A9, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7, A8, A9), Out).
get(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Out) :-
	'$pce_get'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Out).


%   send(+@Object, +Selector, ...+Arguments...)
%
%   Succeeds if sending a message to Object with Selector and the given
%   Arguments succeeds.

/* Directly written in C
send(Object, Selector) :-
	'$pce_send'(Object, Selector, arguments).
send(Object, Selector, A1) :-
	'$pce_send'(Object, Selector, arguments(A1)).
send(Obj, Sel, A1, A2) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2)).
send(Obj, Sel, A1, A2, A3) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3)).
*/
send(Obj, Sel, A1, A2, A3, A4) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4)).
send(Obj, Sel, A1, A2, A3, A4, A5) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4, A5)).
send(Obj, Sel, A1, A2, A3, A4, A5, A6) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6)).
send(Obj, Sel, A1, A2, A3, A4, A5, A6, A7) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7)).
send(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7, A8)).
send(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, A9) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7, A8, A9)).
send(Obj, Sel, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) :-
	'$pce_send'(Obj, Sel, arguments(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)).

:- initialization
   (object(@prolog) -> true ; send(@host, name_reference, prolog)).
