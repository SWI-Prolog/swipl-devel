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

:- module(plot_axis, []).
:- use_module(library(pce)).
:- require([ ceiling/2
	   , forall/2
	   , round/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library defines class plot_axis, an   axis used for plotting graphs
or barcharts. Normally this library  is   used  in  combination with the
library('plot/plotter'), but it can also be used stand-alone.

For example:

	?- send(new(P, picture), open),
	   send(P, display,
		plot_axis(x, 0, 100, @default, 400, point(40, 320))),
	   send(P, display,
		plot_axis(y, 0, 500, @default, 300, point(40, 320))).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(plot_axis(type, low, high), line, "Coordinate system axis").

class_variable(tag_font, font, normal, "Font for values").

:- pce_group(attributes).
variable(origin,	point,		get,  "Location of the origin").
variable(low,		'int|real',	get,  "Low end of the range").
variable(high,		'int|real',	get,  "High end of the range").
variable(step,		'int|real',	get,  "Indication steps").
variable(small_step,	'int|real*',	get,  "Unnumbered indication steps").
variable(length,	int,		get,  "Total length").
variable(type,		{x,y},		get,  "Horizontal/vertical").
variable(value_format,	[name],		get,  "Value format specification").
variable(label,		graphical*,	get,  "Label along the axis").
variable(lines,		{none,normal,all} := normal,
				      	get,  "Draw support lines").
:- pce_group(internal).
variable(support,	chain,		none, "Supporting graphicals").


:- pce_group(@default).

initialise(A, 
	   Type:'type={x,y}',
	   Low:'low=int|real', High:'high=int|real', Step:step=[int|real],
	   Length:length=[int],
	   O:'origin=[point]') :->
	"Create from low, high, step, length and type"::
	default(Length, 200, Len),
	(   Step == @default
	->  determine_steps(Low, High, Length, Type, TheStep, SmallStep)
	;   TheStep = Step,
	    SmallStep = @nil
	),
	send_super(A, initialise),

	send(A, slot, origin,  new(P, point)),
	(   O \== @default
	->  send(P, copy, O)
	;   true
	),
	send(A, slot, low,        Low),
	send(A, slot, high,       High),
	send(A, slot, length,     Len),
	send(A, slot, small_step, SmallStep),
	send(A, slot, step,       TheStep),
	send(A, slot, type,       Type),
	send(A, slot, support,    new(chain)),
	send(A, slot, value_format, @default),
	send(A, request_compute).


determine_steps(Low, High, Len, XY, Step, SmallStep) :-
	Step0 is 10**round(log10(High-Low)-1),
	Dist is Len/((High-Low)/Step0),
	(   okdist(XY, Dist)
	->  Step = Step0,
	    SmallStep = @nil
	;   Step is Step0*5,
	    SmallStep = Step0
	).

okdist(x, Dist) :- Dist > 50.
okdist(y, Dist) :- Dist > 25.

unlink(A) :->
	"Delete all <-support objects"::
	get(A, slot, support, Support),
	send(Support, for_all, message(@arg1, free)),
	send_super(A, unlink).


device(A, Dev:device*) :->
	"Trap device changes"::
	get(A, slot, support, Support),
	send(Support, for_all, message(@arg1, device, Dev)),
	(   get(A, label, Label),
	    Label \== @nil
	->  send(Label, device, Dev)
	;   true
	),
	send_super(A, device, Dev).


		 /*******************************
		 *	  USER COMPUTATION      *
		 *******************************/

:- pce_group(compute).

location(A, Val:'int|real', Loc:int) :<-
	"Location for value"::
	loc(A, Val, Loc).

value_from_coordinate(A, Loc:int, Val:'int|real') :<-
	"Translate location into value"::
	loc(A, Val, Loc).
       
pixel_range(A, Tuple:tuple) :<-
	"Pixel range covered"::
	get(A, length, L),
	get(A, origin, point(OX, OY)),
	(   get(A, type, x)
	->  Min is OX, Max is OX + L
	;   Min is OY, Max is OY - L
	),
	new(Tuple, tuple(Min,Max)).


		 /*******************************
		 *	    SLOT CHANGES        *
		 *******************************/

:- pce_group(update).

changed(A) :->
	"A has changed: force update"::
	send(A, request_compute).

:- pce_group(@default).

origin(A, P:point) :->
	send(A?origin, copy, P),
	send(A, changed).


label(A, L:graphical*) :->
	set(A, label, L).

low(A, L:'int|real') :->
	set(A, low, L).

high(A, L:'int|real') :->
	set(A, high, L).

step(A, L:'int|real') :->
	set(A, step, L).

small_step(A, L:'int|real*') :->
	set(A, small_step, L).

length(A, Length:int) :->
	set(A, length, Length).

type(A, T:{x,y}) :->
	set(A, type, T).

format(A, Fmt:[name]) :->
	set(A, format, Fmt).

lines(A, Lines:{none,normal,all}) :->
	set(A, lines, Lines).

set(A, Slot, Value) :-
	get(A, Slot, Value), !.
set(A, Slot, Value) :-
	send(A, slot, Slot, Value),
	send(A, changed).

default_steps(A) :->
	"Determine default steps from other fields"::
	get(A, low, Low),
	get(A, high, High),
	get(A, length, Length),
	get(A, type, Type),
	determine_steps(Low, High, Length, Type, Step, SmallStep),
	send(A, step, Step),
	send(A, small_step, SmallStep).


geometry(A, X:[int], Y:[int], W:[int], H:[int]) :->
	"Trap geometry changes"::
	(   get(A, request_compute, computing)
	->  send_super(A, geometry, X, Y, W, H)
	;   (   get(A, type, x)
	    ->  send_super(A, geometry, X, Y, W, @default),
		send(A, length, A?width)
	    ;   send_super(A, geometry, X, Y, @default, H),
		send(A, length, -A?height)
	    )
	).


		 /*******************************
		 *	    COMPUTING		*
		 *******************************/

loc(A, V, X) :-
	get(A, origin, O),
	get(A, type, T),
	get(O, T, Origin),
	get(A, low, Low),
	get(A, high, High),
	get(A, length, Length),
	(   T == x
	->  (   nonvar(V)
	    ->  X is round(Origin + ((V-Low) * Length) / (High - Low))
	    ;	V is Low + (X-Origin) * (High-Low)/Length 
	    )
	;   (	nonvar(V)
	    ->	X is round(Origin - ((V-Low) * Length) / (High - Low))
	    ;	V is Low + (Origin-X) * (High-Low)/Length % not tested
	    )
	).


tick(Low, High, Step, Tick) :-
	TheLow is Low - Step / 10000,
	TheHigh is High + Step / 10000,	% avoid floating point rounding
	L0 is ceil(TheLow/Step) * Step,
	tick_(L0, TheHigh, Step, Tick).

tick_(L, _, _, L).
tick_(L, H, S, V) :-
	L1 is L + S,
	L1 =< H,
	tick_(L1, H, S, V).


format(A, Fmt:[char_array]) :<-
	"Compute format to use"::
	(   get(A, slot, value_format, SlotValue),
	    SlotValue \== @default
	->  Fmt = SlotValue
	;   get(A, step, Step),
	    integer(Step), !,
	    Fmt = '%d'
	;   get(A, step, Step),
	    new(S, string('%f', Step)),
	    new(Re, regex('\\..*[^0]')),
	    send(Re, search, S),
	    get(Re, register_end, End),
	    get(Re, register_start, Start),
	    send(Re, done),
	    AfterDot is End - Start - 1,	% 1 for the dot
	    new(Fmt, string('%%.%df', AfterDot))
	).


compute_small_steps(A) :->
	get(A, small_step, SmallStep),
	(   SmallStep \== @nil
	->  get(A, low, Low),
	    get(A, high, High),
	    get(A, step, Step),
	    get(A, device, Dev),
	    get(A, origin, point(OX, OY)),
	    get(A, slot, support, Support),

	    lines_extend(A, small, LStart, LEnd),

	    (   get(A, type, x)
	    ->  (   tick(Low, High, SmallStep, TV),
		    (   Val is TV / Step, integer(Val)
		    ->  true
		    ;   loc(A, TV, TX),
			send(Dev, display, new(L, line(TX, OY, TX, OY+3))),
			send(Support, append, L),
			(   LStart \== LEnd
			->  send(Dev, display,
				 new(SL, line(TX, LStart, TX, LEnd))),
			    send(SL, texture, dotted),
			    send(SL, colour, grey50),
			    send(Support, append, SL)
			;   true 
			)
		    ),
		    fail
		;   true
		)
	    ;   (   tick(Low, High, SmallStep, TV),
		    (   Val is TV / Step, integer(Val)
		    ->  true
		    ;   loc(A, TV, TY),
			send(Dev, display, new(L, line(OX-3, TY, OX, TY))),
			send(Support, append, L),
			(   LStart \== LEnd
			->  send(Dev, display,
				 new(SL, line(LStart, TY, LEnd, TY))),
			    send(SL, texture, dotted),
			    send(SL, colour, grey50),
			    send(Support, append, SL)
			;   true 
			)
		    ),
		    fail
		;   true
		)
	    )
	;   true
	).

place_label(_, @nil, _, _, _) :- !.
place_label(Dev, Label, Dir, X, Y) :-
	get(Label, size, size(W, H)),
	(   Dir == x
	->  send(Label, do_set, X-W, Y-H-3)
	;   send(Label, do_set, X+3, Y)
	),
	send(Dev, display, Label).


lines_extend(A, Which, Min, Max) :-
	get(A, lines, Lines),
	(   (   Which == normal, Lines \= none
	    ->	true
	    ;	Which == small, Lines == all
	    ),
	    get(A, device, Dev),
	    (	get(A, type, y)
	    ->	get(Dev, pixel_range, x, tuple(Min, Max))
	    ;	get(Dev, pixel_range, y, tuple(Min, Max))
	    )
	->  true
	;   Min = 0,
	    Max = 0
	).

	
compute(A) :->
	send(A, slot, request_compute, computing),
	get(A, low, Low),
	get(A, high, High),
	get(A, step, Step),
	get(A, format, Fmt),
	get(A, origin, point(OX, OY)),
	get(A, slot, support, Support),
	get(A, tag_font, Font),
	get(A, label, Label),
	send(Support, for_all, message(@arg1, free)),
	send(Support, clear),

	lines_extend(A, normal, LStart, LEnd),

	send(A, compute_small_steps),

	loc(A, Low, Start),
	loc(A, High, End),
	get(A, device, Dev),
	(   get(A, type, x)
	->  send(A, points, Start, OY, End, OY),
	    OY5 is OY+5,
	    place_label(Dev, Label, x, End, OY),
	    (	tick(Low, High, Step, TV),
		(   loc(A, TV, TX),
		    send(Dev, display,
			 new(L, line(TX, OY, TX, OY5))),
		    send(Dev, display,
			 new(T, text(string(Fmt, TV), font := Font)),
			 point(TX-3, OY5)),
		    (	LStart \== LEnd
		    ->	send(Dev, display,
			     new(SL, line(TX, LStart, TX, LEnd))),
			send(SL, texture, dotted),
			send(Support, append, SL)
		    ;	true
		    ),
		    send(Support, append, L),
		    send(Support, append, T)
		),
		fail
	    ;	true
	    )
	;   send(A, points, OX, Start, OX, End),
	    OX5 is OX-5,
	    place_label(Dev, Label, y, OX, End),
	    (   tick(Low, High, Step, TV),
		(   loc(A, TV, TY),
		    send(Dev, display,
			 new(L, line(OX5, TY, OX, TY))),
		    new(T, text(string(Fmt, TV), font := Font)),
		    get(T, width, TW),
		    get(T, height, TH),
		    TxtX is OX5-TW, TxtY is TY -TH + 3,
		    send(T, set, TxtX, TxtY),
		    send(Dev, display, T),
		    (	LStart \== LEnd
		    ->	send(Dev, display,
			     new(SL, line(LStart, TY, LEnd, TY))),
			send(SL, texture, dotted),
			send(Support, append, SL)
		    ;	true
		    ),
		    send(Support, append, L),
		    send(Support, append, T)
		), fail
	    ;	true
	    )
	),
	(   send(Dev, has_send_method, modified_plot_axis)
	->  send(Dev, modified_plot_axis, A)
	;   true
	),
	send_super(A, compute).

:- pce_end_class.

		 /*******************************
		 *	      RULER		*
		 *******************************/

:- pce_begin_class(plot_ruler, device, "Show X/Y value of point").

variable(value,		real,		get, 	"Displayed value").
variable(value_format,	name := '%g',	both,	"Format used for values").

initialise(R) :->
	send_super(R, initialise),
	send(R, display, new(L, line)),
	send(L, texture, dotted),
	send(R, display, new(T, text(0))),
	send(T, border, 3),
	send(T, background, navajo_white),
	send(R, slot, value, 0).

attach(R, Axis:axis=plot_axis, Value:real, Length:length=[int]) :->
	send(R, detach),
	get(R, member, line, Line),
	get(R, member, text, Text),
	get(Axis, device, Plotter),
	get(Axis, format, Format),
	send(R, slot, value_format, Format),
	(   get(Axis, type, x)		% X-axis
	->  send(Text, format, center),
	    send(Text, center_x, 0),
	    get(Axis, y, Bottom),
	    send(R, y, Bottom),
	    (	Length == @default
	    ->	get(Plotter, y_axis, YAxis),
		get(YAxis, length, Len)
	    ;	Len = Length
	    ),
	    send(Line, points, 0, 5, 0, -Len),
	    send(Text, y, 5)
	;   send(Text, format, right),	% Y-axis
	    send(Text, center_y, 0),
	    get(Axis, x, Left),
	    send(R, x, Left),
	    (	Length == @default
	    ->	get(Plotter, x_axis, XAxis),
		get(XAxis, length, Right)
	    ;	Right = Length
	    ),
	    send(Line, points, -5, 0, Right, 0),
	    send(Text, x, -5-Text?width)
	),
	send(Plotter, display, R),
	new(_, hyper(Axis, R, rules, axis)),
	send(R, value, Value).

detach(R) :->
	send(R, delete_hypers, axis),
	send(R, device, @nil).

value(R, Val:real) :->
	send(R, slot, value, Val),
	send(R, request_compute, value).

compute(R) :->
	(   get(R, request_compute, value),
	    send(R, slot, request_compute, @default),
	    get(R, hypered, axis, Axis),
	    get(R, device, Plotter),
	    Plotter \== @nil
	->  get(R, value, Value),
	    (	get(Axis, type, x)
	    ->	get(Plotter, translate_x, Value, X),
		send(R, x, X)
	    ;	get(Plotter, translate_y, Value, Y),
		send(R, y, Y)
	    ),
	    get(R, member, text, Text),
	    get(R, value_format, Format),
	    send(Text, string, string(Format, Value))
	;   true
	),
	send_super(R, compute).
		
:- pce_end_class.

