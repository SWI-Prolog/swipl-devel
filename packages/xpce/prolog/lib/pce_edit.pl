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

:- module(pce_edit,
	  [ editpce/1
	  ]).
:- use_module(library(pce)).


editpce(Spec) :-
	method(Spec, Obj),
	(   get(Obj, source, Location),
	    Location \== @nil
	->  use_module(library(pce_emacs)),
	    Goal = start_emacs, Goal,	% fool xref
	    send(@emacs, goto_source_location, Location)
	;   send(Obj, report, warning, 'Can''t find source')
	).


method(Object, Object) :-
	object(Object),
	send(Object, has_get_method, source), !.
method(Object, Class) :-
	object(Object), !,
	get(Object, class, Class).
method(ClassName, Class) :-
	atom(ClassName), !,
	get(@pce, convert, ClassName, class, Class).
method(->(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, send_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, send_method, Selector, tuple(_, Method))
	).
method(<-(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, get_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, get_method, Selector, tuple(_, Method))
	).

	
