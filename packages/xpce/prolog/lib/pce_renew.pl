/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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
	     
	
