/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_renew, [pce_renew/1]).
:- use_module(library(pce)).
:- require([ between/3
	   , concat/3
	   , memberchk/2
	   ]).


:- dynamic renew_mode/1.

pce_renew(Mode) :-
	memberchk(Mode, [trace, free, rename, ask]), !,
	retractall(renew_mode(_)),
	assert(renew_mode(Mode)).
pce_renew(_Mode) :-
	send(@pce, report, error,
	     'pce_renew/1: mode is {trace,free,rename,ask}'),
	fail.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library traps the redefined_assoc exception generated on an attempt
to create an object with an already   existing reference.  A prompter is
displayed that allows the user to  free   the  old  object, rename it or
generate the normal error (thus allowing   the programmer to inspect the
context of the redefinition).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- send(@pce?exception_handlers, value, redefined_assoc,
	message(@prolog, call, confirm, @arg1)).


confirm(_Ref) :-			% trace
	renew_mode(trace), !,
	fail.
confirm(Ref) :-				% free
	renew_mode(free), !,
	send(@Ref, free).
confirm(Ref) :-				% rename
	renew_mode(rename), !,
	Object = @Ref,
	between(1, 100000, X),
	atom_concat(Ref, X, NewName),
	\+ object(@NewName), !,
	send(Object, name_reference, NewName).
confirm(Ref) :-				% ask
	Object = @Ref,
	between(1, 100000, X),
	atom_concat(Ref, X, NewName),
	\+ object(@NewName), !,

	new(D, dialog),
	send(D, kind, transient),
	send(D, append,
	     label(message, 
		   string('PCE: new: object %s/%s already exists.',
			  Ref, Object?'_class_name'))),
	send(D, append,
	     button('Free old object',
		    and(message(Object, '_free'),
			message(D, return, free)))),
	send(D, append,
	     button(string('Rename into @%s', NewName),
		    and(message(Object, name_reference, NewName),
			message(D, return, rename)))),
	send(D, append,
	     button(error,
		    message(D, return, error))),
	get(D, confirm_centered, Answer),
	send(D, destroy),
	Answer \== error.
	     
	
