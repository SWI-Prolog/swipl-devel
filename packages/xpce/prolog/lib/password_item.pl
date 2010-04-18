/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2010, University of Amsterdam

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
	    get(Ev, id, 13)		% RET
	->  send_super(I, event)
	;   get(Ev, id, 9)		% TAB
	->  send_super(I, event, Ev)
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
