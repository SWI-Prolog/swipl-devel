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

:- module(plotter, []).
:- use_module(axis, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

:- pce_begin_class(plotter, device,
		   "Diagram for graphs and barcharts").

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
