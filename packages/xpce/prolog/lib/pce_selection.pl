/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_selection, []).
:- use_module(library(pce)).
:- require([ ignore/1
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
X applications from different origin seem   to  obey different rules for
passing data around.  Some use  the   X-display  cut_buffers,  other the
X-display primrary selection  and  yet   other  the  X-display clipboard
selection.

This library is an atempt to  talk  to   everybody.   It  is used by the
manual tools as well as all the other XPCE/Prolog library modules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(display).

selection(D, Which:[name], Value:char_array) :->
	"Set the (textual) selection"::
	new(Selection, string('%s', Value)),
	send(Selection, lock_object, @on),
	send(D, selection_owner, Selection, Which,
	     @receiver?self,
	     message(@receiver, free)).


copy(D, Value:char_array) :->
	"Copy the given value to selection and cut_buffer"::
	ignore(send(D, cut_buffer, 0, Value)),
	ignore(send(D, selection, primary, Value)),
	ignore(send(D, selection, clipboard, Value)).


paste(D, Value:string) :<-
	"Get value to paste"::
	(   get(D, selection, clipboard, Value)
	;   get(D, selection, primary, Value)
	;   get(D, cut_buffer, 0, Value)
	), !.

:- pce_end_class.
