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


:- module(man_instance, []).

:- use_module(library(pce)).
:- require([ concat/3
	   , send_list/3
	   ]).

:- pce_begin_class(man_instance_browser, man_frame,
		   "browser with instances of some class").

variable(created_message,	code*,	get).
variable(freed_message,		code*,	get).
variable(class,			class*, get).


initialise(IB, Manual:man_manual) :->
	"Create from manual"::
	send(IB, send_super, initialise, Manual, 'Instance Browser'),
	send(IB, append, new(B, browser)),
	new(Obj, ?(IB, object, @arg1)),
	send(B, select_message, message(@prolog, portray_object, Obj)),
	send(B, popup, new(P, popup)),
	send_list(P, append,
		  [ menu_item(flash,
			      message(Obj, flash),
			      @default, @off,
			      ?(Obj?class, send_method, flash))
		  , menu_item(portray,
			      message(@prolog, portray_object, Obj))
		  , menu_item(inspect,
			      message(Manual, inspect, Obj),
			      @default, @on)
		  , menu_item(free,
			      message(Obj, free))
		  ]).


unlink(IB) :->
	send(IB, detach),
	send(IB, send_super, unlink).


browser(IB, B) :<-
	"Browser of the frame"::
	get(IB, member, browser, B).


object(_IB, Di:dict_item, Obj) :<-
	get(Di, object, Ref),
	Obj = @Ref.


detach(IB) :->
	"Detach from class"::
	(   get(IB, slot, class, @nil)
	->  true
	;   send(IB, label, 'Instance Browser'),
	    send(IB?class?created_messages, delete, IB?created_message),
	    send(IB?class?freed_messages, delete, IB?created_message),
	    send(IB, slot, class, @nil),
	    send(IB, slot, created_message, @nil),
	    send(IB, slot, freed_messages, @nil)
	).
	   

class(IB, Class:class*) :->
	"Monitor instances of some class"::
	send(IB, detach),
	(   Class \== @nil
	->  send(IB, label, string('Instances of %s', Class?name)),
	    send(IB, slot, class, Class),
	    send(IB, slot, created_message, message(IB, created, @arg2)),
	    send(IB, slot, freed_message, message(IB, freed, @arg1)),
	    send(Class, created_message, IB?created_message),
	    send(Class, freed_message, IB?freed_message)
	;   true
	).


created(IB, Obj:object) :->
	"Add object to the browser"::
	send(Obj, '_inspect', @on),
	Obj = @Ref,
	atom_concat(@, Ref, Key),
	send(IB?browser, append, dict_item(Key, @default, Ref)),
	send(IB?browser, normalise, Key),
	send(IB, flush).


freed(IB, Obj:object) :->
	"Delete object from browser"::
	Obj = @Ref,
	atom_concat(@, Ref, Key),
	send(IB?browser, delete, Key),
	send(IB, flush).

:- pce_end_class.
