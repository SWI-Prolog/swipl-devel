/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(plotter, []).
:- use_module(axis, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

:- pce_begin_class(plotter, device, "A plot").

axis(P, Axe:plot_axis) :->
	"Associate an axis with the plotter"::
	get(Axe, type, Type),
	(   get(P, member, Type, Old)
	->  free(Old)
	;   true
	),
	send(P, display, Axe),
	send(Axe, name, Type).

x_axis(P, Axe:plot_axis) :<-
	"Find the X-axis"::
	get(P, member, x, Axe).

y_axis(P, Axe:plot_axis) :<-
	"Find the Y-axis"::
	get(P, member, y, Axe).

translate(P, X:'int|real', Y:'int|real', Point) :<-
	"Translate a coordinate"::
	get(P, member, x, XAxe),
	get(P, member, y, YAxe),
	get(XAxe, location, X, PX),
	get(YAxe, location, Y, PY),
	new(Point, point(PX, PY)).


translate_x(P, X:'int|real', Xpoint:int) :<-
	"Translate an X- coordinate"::
	get(P, member, x, XAxe),
	get(XAxe, location, X, Xpoint).
translate_y(P, Y:'int|real', Ypoint:int) :<-
	"Translate an Y- coordinate"::
	get(P, member, y, YAxe),
	get(YAxe, location, Y, Ypoint).

value_from_x(P, X:int, Value:'int|real') :<-
	"Translate X- coordinate to value"::
	get(P, member, x, XAxe),
	get(XAxe, value_from_coordinate, X, Value).
value_from_y(P, Y:int, Value:'int|real') :<-
	"Translate Y- coordinate to value"::
	get(P, member, y, YAxe),
	get(YAxe, value_from_coordinate, Y, Value).


translate_plot_point(P, PP:plot_point, Pt:point) :<-
	"Translate plot-point to physical point"::
	get(PP, x, X),
	get(PP, y, Y),
	get(P, translate, X, Y, Pt).

graph(P, Gr:plot_graph) :->
	"Display a graph on the plotter"::
	send(P, display, Gr).

clear(P) :->
	"Remove all graphs"::
	send(P?graphicals, for_all,
	     if(message(@arg1, instance_of, plot_graph),
		message(@arg1, free))).

modified_plot_axis(P, _A:plot_axis) :->
	"Trap changed axis parameters"::
	send(P?graphicals, for_all,
	     if(message(@arg1, instance_of, plot_graph),
		message(@arg1, request_compute))).

:- pce_end_class.

		 /*******************************
		 *	    PLOT-POINT		*
		 *******************************/

:- pce_begin_class(plot_point(x_value, y_value), point, "Plotter point").

variable(modified,	bool := @on,	both,	"X/Y value is modified").
variable(x_value,	'int|real',	get,	"X-value").
variable(y_value,	'int|real',	get,	"Y-value").
variable(curve,		plot_graph,	get,	"Curve I'm associated with").

initialise(P, C:plot_graph, X:'x=int|real', Y:'y=int|real') :->
	"Create from X and Y"::
	send_super(P, initialise),
	send(P, slot, curve, C),
	send(P, slot, x_value, X),
	send(P, slot, y_value, Y),
	send(C, request_compute).

modified(P) :->
	"Indicate point and curve of modification"::
	(   get(P, slot, modified, @on)
	->  true
	;   send(P, slot, modified, @on),
	    get(P, curve, Curve),
	    send(Curve, request_compute)
	).

compute(P) :->
	"Update the represented point"::
	(   get(P, modified, @on)
	->  send(P, slot, modified, @off),
	    get(P, curve, Curve),
	    get(P, x_value, XVal),
	    get(P, y_value, YVal),
	    get(Curve, device, Plotter),
	    get(Plotter, translate_x, XVal, X),
	    get(Plotter, translate_y, YVal, Y),
	    send(Curve, set_point, P, X, Y)
	;   true
	).

x(P, X:'int|real') :->
	send(P, slot, x_value, X),
	send(P, modified).
y(P, Y:'int|real') :->
	send(P, slot, y_value, Y),
	send(P, modified).

x(P, X:'int|real') :<-
	get(P, slot, x_value, X).
y(P, Y:'int|real') :<-
	get(P, slot, y_value, Y).

redundant(P) :->
	"Succeed if I'm right between my neighbours"::
	get(P, curve, Curve),
	get(Curve, kind, poly),
	get(Curve, points, Points),
	get(Points, previous, P, P1),
	get(Points, next, P, P2),
	get(P1, x, X1),
	get(P1, y, Y1),
	get(P2, x, X2),
	get(P2, y, Y2),
	get(P, x, X),
	get(P, y, Y),
	catch(abs(((Y-Y1)/(X-X1)) / ((Y2-Y1)/(X2-X1)) - 1) < 0.1,
	      _,
	      fail).

:- pce_end_class.

		 /*******************************
		 *	    PLOT-GRAPH		*
		 *******************************/

:- pce_begin_class(plot_graph, path, "A graph for the plotter").

initialise(PG,
	   Kind:'type=[{poly,smooth,points_only}]',
	   Mark:'mark=[image]*') :->
	"Create from visualisation and mark"::
	default(Kind, poly, K),
	default(Mark, @nil, M),
	send_super(PG, initialise),
	send(PG, kind, K),
	send(PG, mark, M).


values(PG, Values:chain) :<-
	"Same as <-points"::
	get(PG, points, Values).


kind(PG, T:{poly,smooth,points_only}) :->
	(   T == points_only
	->  send(PG, pen, 0)
	;   send(PG, pen, 1),
	    send_super(PG, kind, T)
	).


append(PG, X:'x=int|real', Y:'y=int|real') :->
	"Append a plot_point to <-values"::
	send_super(PG, append, plot_point(PG, X, Y)).


compute(PG) :->
	"Update points"::
	send(PG?points, for_all, message(@arg1, compute)),
	send_super(PG, compute).

:- pce_end_class.
