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


:- module(pce_float_item, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Text-item specialised for editing floating   point (real) numbers. Usage
should be quite straigh-forward.

See also the built-in class int_item.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(float_item, text_item).

variable(low,		real*,	      both, "Lowest value").
variable(high,		real*,	      both, "highest value").
variable(format,	name := '%g', get,  "How values are formatted").
variable(allow_default,	bool := @off, get,  "'' <-> @default").
variable(step,		real*,	      get,  "Step for up/down").
variable(apply_step,	bool := @on,  both, "Apply stepping immediately").

initialise(RI, Label:name, Default:[real], Msg:[code]*,
	   Low:[real], High:[real]*) :->
	send_super(RI, initialise, Label, '', Msg),
	send(RI, type, real),
	send(RI, length, 10),
	send(RI, style, normal),
	(   Low \== @default
	->  send(RI, low, Low)
	;   true
	),
	(   High \== @default
	->  send(RI, high, High)
	;   true
	),
	(   Default \== @default
	->  send(RI, selection, Default)
	;   true
	).


format(RI, Fmt:name) :->
	send(RI, slot, format, Fmt),
	(   get(RI, slot, selection, Sel),
	    number(Sel)
	->  send(RI, selection, Sel)
	;   true
	).


allow_default(RI, Val:bool) :->
	send(RI, slot, allow_default, Val),
	(   Val == @on
	->  send(RI, type, '[real]')
	;   send(RI, type, real)
	).


selection(RI, Sel:[real]) :<-
	get(RI?value_text, string, Text),
	(   get(@pce, convert, Text, real, Sel)
	->  get(RI, low, Low),
	    get(RI, high, High),
	    (   (   Low == @nil
		->  true
		;   Sel >= Low
		),
		(   High == @nil
		->  true
		;   Sel =< High
		)
	    ->  true
	    ;   (   High == @nil
		->  send(RI, report, error, 'Minimum value is %g', Low)
		;   send(RI, report, error,
			 'Value out of range (%g .. %g)', Low, High)
		),
		fail
	    )
	;   get(RI, allow_default, @on),
	    new(S, string('%s', Text)),
	    send(S, strip),
	    get(S, size, 0)
	->  Sel = @default
	;   send(RI, report, error,
		 'Please enter a valid number'),
	    fail
	).


clear(RI) :->
	send(RI, string, ''),
	send(RI, slot, selection, @default).


value(RI, Value:real) :->
	"Set the displayed value"::
	get(RI, format, Fmt),
	send(RI, string, string(Fmt, Value)).


value(RI, Value:real) :<-
	"Set the displayed value"::
	get(RI, string, Text),
	get(@pce, convert, Text, real, Value).


selection(RI, Sel:[real]) :->
	(   Sel == @default
	->  send(RI, clear)
	;   get(RI, format, Fmt),
	    send(RI, string, string(Fmt, Sel)),
	    send(RI, modified, @off),
	    send(RI, slot, selection, Sel)
	).

type(RI, Type:type) :->
	"Set type, updating <-low and <-high"::
	(   (   get(Type, kind, real_range)
	    ->  get(Type, context, tuple(Low, High)),
		send(RI, low, Low),
		send(RI, high, High)
	    ;   send(Type, includes, real)
	    ->  send(RI, low, @nil),
		send(RI, high, @nil)
	    )
	->  send_super(RI, type, Type)
	;   send(Type, error, domainError, real)
	),
	(   get(RI, step, @nil)
	->  send(RI, style, normal)
	;   send(RI, style, stepper)
	).

step(RI, Step:real*, Apply:[bool]) :->
	send(RI, slot, step, Step),
	(   Step == @nil
	->  send(RI, style, normal)
	;   send(RI, style, stepper)
	),
	(   Apply \== @default
	->  send(RI, apply_step, Apply)
	;   true
	).


increment(RI) :->
	"Handle stepper"::
	get(RI, step, Step),
	Step \== @nil,
	get(RI, value, Now),
	NewVal is Now+Step,
	send(RI, value, NewVal),
	(   get(RI, apply_step, @on)
	->  send(RI, apply)
	;   true
	).

decrement(RI) :->
	"Handle stepper"::
	get(RI, step, Step),
	Step \== @nil,
	get(RI, value, Now),
	NewVal is Now-Step,
	send(RI, value, NewVal),
	(   get(RI, apply_step, @on)
	->  send(RI, apply)
	;   true
	).


:- pce_end_class(float_item).


