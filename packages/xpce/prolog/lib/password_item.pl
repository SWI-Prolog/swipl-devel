/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_password_item,
	  [
	  ]).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This class realises a GUI password  item, visualising the typed password
as a list of stars. The returned value   is  an XPCE string to avoid the
password entering the XPCE symbol table where it would be much easier to
find.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	 CLASS PASSWD_ITEM	*
		 *******************************/
	    
:- pce_begin_class(password_item, text_item, "text-item for entering a passwd").

variable(shadow,	text_item,	get, "The real (invisible) item").

initialise(I, Name:[name], Message:[message]) :->
	default(Name, password, TheName),
	send_super(I, initialise, TheName, string('')),
	send(I, slot, shadow, text_item(TheName, string(''), Message)).


unlink(I) :->
	get(I, shadow, Shadow),
	free(Shadow),
	send_super(I, unlink).


event(I, Ev:event) :->
	get(I, shadow, Shadow),
	(   get(Shadow, message, @default),
	    get(Ev, id, 13)
	->  send_super(I, event)
	;   send(Shadow, event, Ev),
	    send(I, update),
	    (   send(Ev, is_a, keyboard)
	    ->  true
	    ;   send_super(I, event, Ev)
	    )
	).


update(I) :->
	"Update visual representation"::
	get(I, shadow, Shadow),
	get(Shadow, selection, String),
	get(Shadow, caret, Caret),
	get(String, size, Size),
	make_star_string(Size, Stars),
	send_super(I, selection, Stars),
	send(I, caret, Caret).
	    

selection(I, Passwd:string) :<-
	get(I, shadow, Shadow),
	get(Shadow, selection, Passwd).

selection(I, Passwd:string) :->
	get(I, shadow, Shadow),
	send(Shadow, selection, Passwd),
	send(I, update).

make_star_string(Size, S) :-
	new(S, string),
	forall(between(1, Size, _), send(S, append, '*')).

:- pce_end_class(password_item).
