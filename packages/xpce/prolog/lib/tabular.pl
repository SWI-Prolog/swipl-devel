/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(tabular, []).
:- use_module(library(pce)).

:- pce_begin_class(tabular, device,
		   "Device with associated table <-layout_manager").

delegate_to(layout_manager).

initialise(TD) :->
	send_super(TD, initialise),
	send(TD, layout_manager, new(_, table)).

:- pce_group(appearance).


:- pce_global(@tabular_device_recogniser,
	      new(resize_table_slice_gesture(column, left))).

event(RT, Ev:event) :->
	get(RT, table, Table),
	(   get(Table, cell_from_position, Ev, Cell),
	    send(Cell, instance_of, table_cell),
	    get(Cell, image, Gr),
	    (   send(Gr, has_send_method, on_mark_clicked),
	        send(Ev, is_a, button),
		get(Cell, note_mark, Mark),
		Mark \== @nil,
		get(Cell, area, area(X, Y, W, _)),
		get(Ev, position, RT, point(EX, EY)),
		get(Mark, size, size(MW, MH)),
		EX > X+W-MW,
		EY < Y+MH
	    ->	(   send(Ev, is_up)
		->  send(Gr, on_mark_clicked)
		;   true
		)
	    ;   send(Ev, post, Gr)
	    )
	;   send(@tabular_device_recogniser, event, Ev)
	).

:- pce_group(geometry).

table_width(TD, W:int) :->
	"Set width of the table"::
	send(TD?table, width, W).

:- pce_group(parts).

table(TD, Table:table) :<-
	"Get the table layout_manager"::
	get(TD, layout_manager, Table).

:- pce_group(fill).

append(TD,
       Label:label='name|graphical',
       Font:font=[font],
       HAlign:halign=[{left,center,right}],
       VAlign:valign=[{top,center,bottom}],
       Span:colspan='[1..]',
       RSpan:rowspan='[1..]',
       BG:background=[colour],
       FG:colour=[colour]) :->
	"Append a cell to the table"::
	get(TD, table, Table),
	(   atom(Label)
	->  new(Gr, text(Label, @default, Font))
	;   Gr = Label,
	    (   Font \== @default
	    ->	send(Gr, font, Font)
	    ;	true
	    )
	),
	new(TC, table_cell(Gr)),
	(   HAlign \== @default
	->  send(TC, halign, HAlign)
	;   true
	),
	send(TC, background, BG),
	(   FG \== @default
	->  send(TC?image, colour, FG)
	;   true
	),
	(   Span \== @default
	->  send(TC, col_span, Span)
	;   true
	),
	(   RSpan \== @default
	->  send(TC, row_span, RSpan)
	;   true
	),
	(   VAlign \== @default
	->  send(TC, valign, VAlign)
	;   true
	),
	send(Table, append, TC).

:- pce_group(label).

append_label_button(TD, Field:name) :->
	"Append a button to sort the field"::
	get(TD, layout_manager, Table),
	get(Table, current, point(X, Y)),
	send(Table, append,
	     new(TC, table_cell(new(B, button(Field,
					      message(TD, sort_rows,
						      X, Y+1)))))),
	send(B, radius, 0),
	get(class(dialog), class_variable, background, BGVar),
	send(TC, background, BGVar?value),
	send(TC, cell_padding, size(0,0)),
	send(TC, halign, stretch).

:- pce_group(sort).

sort_rows(TD, Col:int, FromRow:int) :->
	"Sort rows starting at FromRow on the indicated column"::
	format('~p: Sorting rows below ~w on column ~w~n', [TD, FromRow, Col]).

:- pce_end_class(tabular).
