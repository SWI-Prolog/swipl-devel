/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
