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
