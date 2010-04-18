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

:- module(pce_prompt,
	  [ prompter/2
	  ]).
:- use_module(library(pce)).
:- require([ checklist/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a standard prompter-box for PCE applications.  It is
invoked with:

    prompter(+Tile, +ListOfAttributes)

where each attribute is a term of the form

   +Label:+Type = -Value[/+Default]

Examples:

prompter('Create class',
       [ name:name = Name
       , super:name = Super
       ]).

NOTE:	Package is under development.  Needs support for more types;
	optional/obligatory fields and better error-messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@prompter, new(dialog)).
:- pce_global(@prompter_execute_message,
	      new(message(@receiver?window?ok_member, execute))).

prompter(Title, Attributes) :-
	send(@prompter, clear),
	checklist(append_prompter(@prompter), Attributes),
	send(@prompter, append,
	     button(ok, message(@prompter, return, ok)), next_row),
	send(@prompter, append,
	     button(cancel, message(@prompter, return, cancel))),
	send(@prompter?frame, label, Title),
	send(@prompter, fit),
	repeat,
	    get(@prompter, confirm_centered, ?(@event, position, @display), OK),
	    (   OK == ok
	    ->  checklist(read_prompter(@prompter), Attributes), !,
	        send(@prompter, show, @off)
	    ;   !,
		send(@prompter, show, @off),
		fail
	    ).


		/********************************
		*      CREATE DIALOG ITEMS	*
		********************************/

append_prompter(P, Label:Type = Value) :-
	make_dialog_item(DI, Label, Type),
	set_default(Value, DI),
	send(P, append, DI).

						  % TBD: specialised types
make_dialog_item(DI, Label, _) :- !,
	new(DI, text_item(Label, '', @prompter_execute_message)).


		/********************************
		*          SET DEFAULTS		*
		********************************/

set_default(Value, DI) :-
	nonvar(Value),
	Value = _RVal/Default, !,
	send(DI, selection, Default).
set_default(_, _).

		/********************************
		*           READ VALUES		*
		********************************/

read_prompter(P, Label:Type = Value) :-
	get(P, member, Label, DI),
	get(DI, selection, V0),
	canonise(DI, V0, V1),
	(   get(@pce, convert, V1, Type, Val)
	->  (   nonvar(Value),
	        Value = RVal/_
	    ->  RVal = Val
	    ;   Value = Val
	    )
	;   send(@display, inform, '%s should be a %s', Label, Type),
	    fail
	).


canonise(DI, A, B) :-
	send(DI?class, is_a, text_item), !,
	new(S, string(A)),
	send(S, strip),
	get(S, value, B),
	send(S, done).
canonise(_, Val, Val).				  % TBD
