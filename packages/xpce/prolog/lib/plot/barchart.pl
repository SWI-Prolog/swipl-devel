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

:- module(barchart, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(plotter).
:- use_module(axis).

:- pce_autoload(partof_hyper, library(hyper)).
:- use_module(library(help_message)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(library(barchart))

This library defines primitives for handling   various types of (simple)
bar-charts.  The classes defines are:

	# Class bar
	Bar in a bar-chart.  A subclass of class box.

	# Class bar_stack
	Class bar_stack is a subclass of class device, defining a stack
	of bars for visualising an accumulated value.

	# Class bar_group
	Class bar_group is a subclass of bar_stack, defining a group of
	related bars. A typical usage for such bars is showing the
	values of various parameters `last year' and `now'.
	
	# Class bar_chart
	A subclass of class plotter for showing curves.  It displays
	an X- or Y-axis and a base-line for the bars.  Bars, bar stacks
	or bar-groups can be appended to the chart.

A message and/or drag_message may be associated with a bar, allowing the
user to modify the value of the   bar. While dragging, the current value
is displayed in the axis.

Packages used:

	# library(help_message)
	For showing the current value as a number if the pointer is
	positioned on a bar.

	# library(library(plotter))
	Generic curve plotting device
	
	# library(library(axis))
	Generic X/Y axis defining coordinate translations 

	# library(library(hyper))
	using partof_hyper for dependency relations between bars and
	their labels.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	    BAR TEMPLATE	*
		 *******************************/

:- pce_begin_class(bar_template, template,
		   "Common for bar and bar_stack").

unlink(B) :->
	"Delete from <-bars of the chart"::
	(   get(B, device, Chart),
	    send(Chart, instance_of, bar_chart),
	    get(Chart, bars, Bars),
	    send(Bars, delete, B),
	    send(Chart, request_compute, place_bars)
	;   true
	), !,
	send_super(B, unlink).

label(B, Label:bar_label) :<-
	"Find related label"::
	get(B, hypered, label, Label).

center_base(BS, Base:point) :<-
	"Return the location of the center of the base"::
	send(BS, compute),
	(   get(BS, orientation, horizontal)
	->  get(BS, center_y, Y),
	    get(BS, x, X)
	;   get(BS, center_x, X),
	    get(BS, y, Y)
	),
	new(Base, point(X, Y)).

center_end(BS, End:point) :<-
	"Return the location of the center of the (maximal) end"::
	plotter(BS, Plotter),
	(   get(BS, orientation, horizontal)
	->  get(BS, center_y, Y),
	    get(Plotter, x_axis, XAxis),
	    get(XAxis, length, Len),
	    get(BS, x, X0),
	    X is X0+Len
	;   get(BS, center_x, X),
	    get(Plotter, y_axis, YAxis),
	    get(YAxis, length, Len),
	    get(BS, y, Y0),
	    Y is Y0-Len
	),
	new(End, point(X, Y)).

:- pce_end_class.


		 /*******************************
		 *    SIMPLE (EDITABLE) BAR	*
		 *******************************/

:- pce_begin_class(bar(name, value), box, "Bar for a bar-chart").
:- use_class_template(bar_template).

class_variable(thickness, '0..', 20, "Default thickness of the bar").
class_variable(pen,       '0..', 0,  "Drawing pen for the bar").

variable(low,		real*,			get, "Minimum value").
variable(high,		real*,			get, "Maximum value").
variable(value,		real,			get, "Value represented").
variable(orientation,	{horizontal,vertical},	get, "laying or standing").
variable(message,	code*,			both, "Executed after edit").
variable(drag_message,	code*,			both, "Executed on drags").

initialise(B,
	   Name:name=name,
	   Value:value=real,
	   Colour:colour=[colour|image]*,
	   Orientation:orientation=[{horizontal,vertical}]) :->
	default(Value, 0, TheValue),
	default(Colour, @nil, TheColour),
	default(Orientation, vertical, TheOrientation),
	get(B, thickness, W),
	send_super(B, initialise, W, W),
	send(B, name, Name),
	send(B, slot, value, TheValue),
	send(B, slot, orientation, TheOrientation),
	send(B, request_compute),
	send(B, fill_offset, point(0,0)),
	send(B, colour, TheColour).

value(B, Val:real) :->
	send(B, slot, value, Val),
	send(B, request_compute).

orientation(B, O:{horizontal,vertical}) :->
	send(B, slot, orientation, O),
	send(B, request_compute).

thickness(B, W:'0..') :->
	(   get(B, orientation, horizontal)
	->  send(B, height, W)
	;   send(B, width, W)
	).

range(B, Low:low=real*, High:high=real*) :->
	send(B, slot, low, Low),
	send(B, slot, high, High).

colour(B, Colour:'colour|image*') :->
	"Colour of the interior"::
	send(B, fill_pattern, Colour),
	(   Colour == @nil
	->  send(B, pen, 1)
	;   send(B, pen, 0)
	).
colour(B, Colour:'colour|image*') :<-
	"Colour of the interior"::
	get(B, fill_pattern, Colour).

value_format(B, Fmt:name) :<-
	"Format for displaying values"::
	(   plotter(B, Plotter),
	    get(Plotter, value_format, Fmt)
	->  true
	;   Fmt = '%g'
	).
	    
plotter(@nil, _) :- !,
	fail.
plotter(Plotter, Plotter) :-
	send(Plotter, instance_of, plotter), !.
plotter(Gr, Plotter) :-
	get(Gr, device, Dev),
	plotter(Dev, Plotter).

compute(B) :->
	"Update geometry"::
	(   plotter(B, Plotter)
	->  get(B, value, Value),
	    (   get(B, orientation, vertical)
	    ->	get(Plotter, member, y, Axis),
	        get(Axis, location, Value, YVal),
		get(Axis, location, Axis?low, YVal0),
		H is YVal - YVal0,
		send(B, height, H)
	    ;	get(Plotter, member, x, Axis),
		get(Axis, location, Value, XVal),
		get(Axis, location, Axis?low, X0),
		W is XVal - X0,
		send(B, width, W)
	    ),
	    send_super(B, compute)
%	    ignore(send(B, send_hyper, label, update_position))
	;   true
	).
	

:- free(@bar_recogniser).
:- pce_global(@bar_recogniser, new(bar_drag_gesture)).

event(B, Ev:event) :->
	(   send_super(B, event, Ev)
	;   send(@bar_recogniser, event, Ev)
	).

help_message(B, What:{tag,summary}, _Ev:[event], Msg:string) :<-
	"Return balloon while pointer is still"::
	What == tag,
	get(B, name, Name),
	get(B, value, Value),
	get(B, value_format, Format),
	atom_concat('%s = ', Format, Fmt),
	get(Name, label_name, Label),
	new(Msg, string(Fmt, Label, Value)).

:- pce_end_class.

		 /*******************************
		 *	       STACK		*
		 *******************************/

:- pce_begin_class(bar_stack, device, "Stacked bars").
:- use_class_template(bar_template).

variable(orientation, {horizontal,vertical}, get, "Current orientation").

initialise(BS, Name:name, Bars:bar ...) :->
	"Create from multiple bars"::
	send_super(BS, initialise),
	send(BS, name, Name),
	(   get(@pce, version, number, V), V>=50000
	->  send_list(BS, append, Bars)
	;   send(Bars, for_all, message(BS, append, @arg1))
	),
	(   Bars = [FirstBar|_],
	    get(FirstBar, orientation, Orientation)
	->  true
	;   get(@pce, version, number, V), V<50000,
	    get(Bars?head, orientation, Orientation)
	->  true
	;   Orientation = vertical
	),
	send(BS, slot, orientation, Orientation).

orientation(BS, Orientation:{horizontal,vertical}) :->
	"Change the orientation"::
	send(BS, slot, orientation, Orientation),
	send(BS?graphicals, for_all,
	     message(@arg1, orientation, Orientation)).

thickness(BS, Thickness:'0..') :->
	"Change the thickness of member bars"::
	send(BS?graphicals, for_all,
	     message(@arg1, thickness, Thickness)).

compute(BS) :->
	send(BS, place_bars),
	send_super(BS, compute).
%	ignore(send(BS, send_hyper, label, update_position)).
	
place_bars(BS) :->
	get_chain(BS, graphicals, Grs),
	place_stacked_bars(Grs, 0).

place_stacked_bars([], _).
place_stacked_bars([B|T], XY) :-
	send(B, compute),
	(   get(B, orientation, vertical),
	    SelXY = y, SelWH = height
	;   SelXY = x, SelWH = width
	),
	send(B, SelXY, XY),
	get(B, SelWH, WH),
	NewXY is XY + WH,
	place_stacked_bars(T, NewXY).
	    

append(BS, Bar:bar) :->
	"Append a bar"::
	send(BS, display, Bar),
	send(Bar, hide).

device(BS, Dev:device*) :->
	send_super(BS, device, Dev),
	send(BS?graphicals, for_all, message(@arg1, request_compute)).

:- pce_end_class.

		 /*******************************
		 *	       GROUP		*
		 *******************************/

:- pce_begin_class(bar_group, bar_stack,
		   "Group of related bars").

place_bars(BS) :->
	plotter(BS, Plotter),
	get(Plotter, bar_width, BW),
	get_chain(BS, graphicals, Grs0),
	reverse(Grs0, Grs),
	length(Grs, N),
	SubBW is 2*BW/(N+1),
	place_grouped_bars(Grs, SubBW, 0).

place_grouped_bars([], _, _).
place_grouped_bars([B|T], BW, XY) :-
	send(B, compute),
	(   get(B, orientation, vertical),
	    SelXY = x, SelWH = width
	;   SelXY = y, SelWH = height
	),
	send(B, SelXY, XY),
	Skip is BW/2,
	send(B, SelWH, BW),
	NewXY is XY + Skip,
	place_grouped_bars(T, BW, NewXY).

:- pce_end_class.


		 /*******************************
		 *	       LABEL		*
		 *******************************/

:- pce_begin_class(bar_button_group, dialog_group,
		   "Set of buttons for controlling the bar").

initialise(BBG, Bar:'bar|bar_stack', Buttons:graphical ...) :->
	send_super(BBG, initialise, @default, group),
	send(BBG, gap, size(0,0)),
	send_list(BBG, append, Buttons),
	send(BBG, layout_dialog),
	new(_, partof_hyper(Bar, BBG, controller, bar)),
	send(BBG, update_position),
	send(Bar?device, display, BBG).

bar(BL, Bar:'bar|bar_stack') :<-
	"Get bar I'm related too"::
	get(BL, hypered, bar, Bar).

update_position(BBG) :->
	"Place myself at the end of the bar"::
	get(BBG, bar, Bar),
	get(Bar, center_end, point(X, Y)),
	(   get(Bar, orientation, horizontal)
	->  send(BBG, set, X+10),
	    send(BBG, center_y, Y)
	;   send(BBG, set, @default, Y-10-BBG?height),
	    send(BBG, center_x, X)
	).

:- pce_end_class.


:- pce_begin_class(bar_label, device,
		   "Label attached to a bar").

variable(text, text, get, "Represented text object").

initialise(BL, Bar:'bar|bar_stack') :->
	send_super(BL, initialise),
	new(_, partof_hyper(Bar, BL, label, bar)),
	new(T, text(Bar?name?label_name)),
	send(BL, slot, text, T),
	send(BL, update_label),
	get(Bar, center_base, Pos),
	send(BL, position, Pos),
	send(Bar?device, display, BL).

update_label(BL) :->
	send(BL, clear),
	get(BL, text, T),
	get(BL, bar, Bar),
	(   get(Bar, orientation, horizontal),
	    send(T, alignment, right),
	    send(T, center_y, 0),
	    get(T, width, TW),
	    TX is -TW - 5,
	    send(T, x, TX),
	    send(BL, display, T)
	;   new(I, image(@nil, T?width, T?height)),
	    send(I, hot_spot, point(T?width+5, T?height/2)),
	    send(I, draw_in, T),
	    get(I, rotate, 90, I2),	% 60?
	    free(I),
	    get(I2, hot_spot, point(HX, HY)),
	    new(BM, bitmap(I2)),
	    send(BM, transparent, @on),
	    send(BL, display, BM, point(-HX, -HY))
	).

font(BL, Font:font) :->
	"Change font of the label"::
	get(BL, text, T),
	send(T, font, Font),
	send(BL, update_label).

bar(BL, Bar:'bar|bar_stack') :<-
	"Get bar I'm related too"::
	get(BL, hypered, bar, Bar).

update_position(BL) :->
	get(BL, hypered, bar, Bar),
	get(Bar, center_base, CB),
	send(BL, position, CB).

:- pce_global(@bar_label_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@receiver?device,
						      select,
						      @receiver?bar)),
				click_gesture(left, c, single,
					      message(@receiver?device,
						      select,
						      @receiver?bar,
						      toggle))))).

event(BL, Ev:event) :->
	"Allow (de)select"::
	(   send_super(BL, event, Ev)
	;   send(@bar_label_recogniser, event, Ev)
	).

:- pce_end_class.


		 /*******************************
		 *	    BAR EDITING		*
		 *******************************/

:- pce_begin_class(bar_drag_gesture, gesture, "Drag a bar lower/higher").

variable(feedback,	plot_ruler,	get, "Box shown for feedback").

initialise(G) :->
	send_super(G, initialise, left, ''),
	send(G, slot, feedback, new(plot_ruler)).

verify(_, Ev:event) :->
	get(Ev, receiver, Bar),
	(   get(Bar, message, Code)
	;   get(Bar, drag_message, Code)
	),
	Code \== @nil.

initiate(G, Ev:event) :->
	get(Ev, receiver, Bar),
	get(G, feedback, FB),
	plotter(Bar, Plotter),
	get(Bar, absolute_position, Plotter, point(BX, BY)),
	(   get(Bar, orientation, horizontal)
	->  get(Plotter, x_axis, Axis),
	    get(Axis, y, Y),
	    Len is Y-BY
	;   get(Plotter, y_axis, Axis),
	    get(Axis, x, X),
	    get(Bar, width, BW),
	    Len is BX+BW-X
	),
	send(FB, attach, Axis, Bar?value, Len+5),
	send(G, update_value).

drag(G, Ev:event) :->
	get(Ev, receiver, Bar),
	send(G, update_value, Ev),
	(   get(Bar, drag_message, Msg),
	    Msg \== @nil
	->  get(G, feedback, FB),
	    get(FB, value, NewVal),
	    send(Bar, value, NewVal),
	    send(Msg, forward, NewVal)
	).

update_value(G, Ev:event) :->
	get(Ev, receiver, Bar),
	plotter(Bar, Plotter),
	get(Bar, absolute_position, Plotter, point(BX, BY)),
	(   get(Bar, orientation, horizontal)
	->  get(Ev, x, Plotter, PointsVal),
	    get(Plotter, value_from_x, BX,        BarV0),
	    get(Plotter, value_from_x, PointsVal, NewVal0)
	;   get(Ev, y, Plotter, PointsVal),
	    get(Plotter, value_from_y, BY,        BarV0),
	    get(Plotter, value_from_y, PointsVal, NewVal0)
	),
	get(Plotter, low, Low),
	get(Plotter, high, High),
	bounds(NewVal0-BarV0, Low, High, NewVal),
	get(G, feedback, FB),
	send(FB, value, NewVal).

bounds(Val0, Min, Max, Val) :-
	Val is min(max(Val0, Min), Max).

terminate(G, Ev:event) :->
	get(Ev, receiver, Bar),
	get(G, feedback, FB),
	get(FB, value, NewVal),
	send(FB, detach),
	send(Bar, value, NewVal),
	(   get(Bar, message, Msg),
	    Msg \== @nil
	->  send(Msg, forward, NewVal)
	;   true
	).

:- pce_end_class.


		 /*******************************
		 *	     BAR-CHART		*
		 *******************************/

:- pce_begin_class(bar_chart, plotter,
		   "Display a bar-chart").

variable(orientation, {horizontal,vertical}, get, "Orientation of the bars").
variable(bar_width,   '1..',		     get, "Width of the bars").
variable(bar_gap,     int,		     get, "Distance between bars").
variable(low,	      real,		     get, "Lowest value").
variable(high,	      real,		     get, "Highest value").
variable(bars,	      chain,		     get, "List of bars").
variable(show_labels, bool := @on,	     get, "Show labels on the bars").
variable(value_format,name,		     both,"Format displayed values").

initialise(BC,
	   Orientation:orientation={horizontal,vertical},
	   Low:low=real, High:high=real, ScaleLength:'scale_length=[0..]',
	   NBars:'nbars=[0..]',
	   BarWidth:'bar_width=[0..]',
	   BarGap:'bar_gap=[0..]') :->
	default(ScaleLength, 200, SL),
	default(NBars, 5, NB),
	default(BarWidth, 20, BW),
	default(BarGap, 10, BG),
	send_super(BC, initialise),
	send(BC, slot, orientation, Orientation),
	send(BC, slot, bars, new(chain)),
	send(BC, slot, bar_width, BW),
	send(BC, slot, bar_gap, BG),
	send(BC, slot, low, Low),
	send(BC, slot, high, High),
	determine_format(Low, High, Fmt),
	send(BC, slot, value_format, Fmt),
	BL is NB * (BW+BG) + BG,
	(   Orientation == vertical
	->  send(BC, axis,
		 plot_axis(y, Low, High, @default, SL, point(0, SL))),
	    send(BC, display, new(L, line(0, SL, BL, SL)))
	;   send(BC, axis,
		 plot_axis(x, Low, High, @default, SL, point(0, 0))),
	    send(BC, display, new(L, line(0, -BL, 0, 0)))
	),
	send(L, name, bar_base).

determine_format(Low, High, '%d') :-
	Low  =:= integer(Low), 
	High =:= integer(High),
	High - Low > 10.
determine_format(Low, High, Format) :-
	Digits is 2-round(log10(High-Low)),
	Digits > 0,
	concat_atom(['%.', Digits, f], Format).
determine_format(_, _, '%g').

:- pce_group(layout).

nbars(BC, NBars:[int]) :->
	"Adjust length of the bar_base line"::
	(   NBars == @default
	->  get(BC?bars, size, NB)
	;   NB = NBars
	),
	get(BC, bar_width, BW),
	get(BC, bar_gap, BG),
	BL is NB * (BW+BG) + BG,
	get(BC, member, bar_base, L),	% the base-line
	(   get(BC, orientation, vertical)
	->  send(L, end_x, BL)
	;   send(L, start_y, -BL)
	),
	(   get(BC, x_axis, Xaxis)
	->  send(Xaxis, request_compute)
	;   true
	),
	(   get(BC, y_axis, Yaxis)
	->  send(Yaxis, request_compute)
	;   true
	).

pixel_range(BC, Dir:{x,y}, Range:tuple) :<-
	(   get_super(BC, pixel_range, Dir, Range)
	->  true
	;   get(BC, member, bar_base, Line),
	    (	Dir == x
	    ->	get(Line, start_x, Min),
		get(Line, end_x, Max)
	    ;	get(Line, start_y, Min),
		get(Line, end_y, Max)
	    ),
	    new(Range, tuple(Min, Max))
	).
	    

:- pce_group(contents).

append(BC, Bar:'bar|bar_stack') :->
	"Append a bar to the chart"::
	get(BC, orientation, Orientation),
	send(Bar, orientation, Orientation),
	get(BC, bars, Bars),
	send(Bars, append, Bar),
	get(Bars, size, NBars),
	get(BC, bar_width, BW),
	get(BC, bar_gap, BG),
	get(BC, member, bar_base, BaseLine),
	(   Orientation == vertical
	->  get(BaseLine, y, BarY),
	    get(BC, y_axis, YAxis),
	    get(YAxis, x, X0),
	    BarX is X0+(NBars-1)*BW + NBars*BG
	;   get(BaseLine, x, BarX),
	    get(BC, x_axis, XAxis),
	    get(XAxis, y, Y0),
	    BarY is Y0-NBars*BW - NBars*BG
	),
	send(Bar, thickness, BW),
	send(BC, display, Bar, point(BarX, BarY)),
	(   get(BC, show_labels, @on)
	->  new(_, bar_label(Bar))
	;   true
	).

delete(BC, Bar:'member:bar|bar_stack') :->
	"Delete (named) bar of bar-stack"::
	free(Bar),
	send(BC, request_compute, place_bars).

compute(BC) :->
	"Restore the placement of the bars"::
	(   get(BC, request_compute, place_bars)
	->  send(BC, place_bars)
	;   true
	),
	send_super(BC, compute).

place_bars(BC) :->
	"Restore the placing of the bars after a delete/insert/reorder"::
	get(BC, bars, Bars),
	get(Bars, size, Size),
	get(BC, bar_width, BW),
	get(BC, bar_gap, BG),
	(   get(BC, orientation, vertical)
	->  (   get(BC, y_axis, YAxis),
	        get(YAxis, x, X0),
		between(1, Size, N),
	        get(Bars, nth1, N, Bar),
		BarX is X0+(N-1)*BW + N*BG,
		send(Bar, x, BarX),
		fail
	    ;	true
	    )
	;   (   get(BC, x_axis, XAxis),
	        get(XAxis, y, Y0),
		between(1, Size, N),
	        get(Bars, nth1, N, Bar),
		BarY is Y0-N*BW - N*BG,
		send(Bar, y, BarY),
		fail
	    ;	true
	    )
	),
	send(BC?graphicals, for_all,
	     if(message(@arg1, has_send_method, update_position),
		message(@arg1, update_position))).


clear(BC) :->
	"Remove all bars"::
	get(BC, bars, Bars),
	send(Bars, for_all, message(@arg1, destroy)),
	send(Bars, clear).

modified_plot_axis(P, A:[plot_axis]) :->
	send_super(P, modified_plot_axis, A),
	send(P, expose_member, bar_base).

:- pce_group(value).

value(BC, BarName:name, Value:real) :->
	"Set value of the named bar"::
	get(BC, member, BarName, Bar),
	send(Bar, value, Value).
value(BC, BarName:name, Value:real) :<-
	"Get value of the named bar"::
	get(BC, member, BarName, Bar),
	get(Bar, value, Value).

:- pce_group(event).

:- pce_global(@bar_chart_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, selection, @nil)))).

event(BC, Ev:event) :->
	(   send_super(BC, event, Ev)
	;   send(@bar_chart_recogniser, event, Ev)
	).

:- pce_group(selection).

select(BC, What:'bar|bar_stack', How:[{toggle,set}]) :->
	"Modify selection (invokes ->selection)"::
	(   How == toggle,
	    get(BC, selection, Chain)
	->  (	send(Chain, delete, What)
	    ->	true
	    ;	send(Chain, append, What)
	    ),
	    send(BC, selection, Chain)
	;   send(BC, selection, What)
	).

selection(BC, Bars:chain) :<-
	"Get selected bars/bar stacks"::
	get(BC, get_super, selection, Labels),
	get(Labels, map, @arg1?bar, Bars).
selection(BC, Bars:'bar|bar_stack|chain*') :->
	"Set selected bars/bar stacks"::
	(   Bars == @nil
	->  send_super(BC, selection, @nil)
	;   send(Bars, instance_of, chain)
	->  get(Bars, map, @arg1?label, Labels),
	    send_super(BC, selection, Labels)
	;   get(Bars, label, Label),
	    send_super(BC, selection, Label)
	).

:- pce_end_class.

