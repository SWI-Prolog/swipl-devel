/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(doc_table, []).
:- use_module(library(pce)).
:- use_module(doc(util)).
	  

		 /*******************************
		 *	      TABLE		*
		 *******************************/

:- pce_begin_class(doc_table, figure, "Format a table/tabular environment").

variable(line_width,	     int*, get,	"Table computed for this width").
variable(adjusted_for_width, int*, get,	"Table is adjusted for this width").
variable(natural_width,	     int*, get, "Width I would like to have").
variable(def_alignment,      {left,center,right} := left, both,
					"Default cell alignment").

initialise(T, Options:prolog) :->
	send_super(T, initialise),
	send(T, layout_manager, new(Table, doc_table_manager)),
	send(Table, border, 0),
	send(Table, frame, void),
	send(Table, rules, none),
	apply_options(Options, table_option, T).

make_cell(T, Options:prolog, PB:parbox) :<-
	"Add a new cell"::
	get(T, layout_manager, Table),
	(   get(Table, current, point(X, _Y)),
	    get(Table, column, X, Col),
	    get(Col, attribute, fixed_width, Width),
	    option(colspan(Span), Options, 1),
	    Span == 1
	->  debug(table, 'Fixed width (~w) cell. Span: ~q~n',
		  [Width, Span]),
	    new(PB, pbox(Width, left))
	;   new(PB, pbox(1000, left)),
	    send(PB, auto_crop, @on)
	),
	new(Cell, table_cell(PB)),
	send(Cell, halign, stretch),
	(   select(Options, align=Align, Options1)
	->  (   Align == char
	    ->	print_message(warning, doc(ignored_attribute(td, align=char)))
	    ;   send(PB, alignment, Align)
	    )
	;   get(T, def_alignment, Align),
	    send(PB, alignment, Align)
	),
	send(Table, append, Cell),
	apply_options(Options1, cell_option, Cell).

next_row(T) :->
	"Move to the next row"::
	get(T, layout_manager, Table),
	(   get(Table, current, point(1, _))
	->  true			% is at start of row
	;   send(T?layout_manager, next_row)
	).

compute_cell_rubber(_T, PB:parbox) :->
	"Compute ->hrubber or table_cell"::
	get(PB, layout_interface, Cell),
	get(PB, width, NW),		% TBD: <-natural_width?
	get(PB, minimum_width, MW),
	Shrink is (NW-MW)**2,
	Stretch is NW,
	new(R, rubber(1, Stretch, Shrink)),
	send(R, natural, NW),
	send(R, minimum, MW),
	send(Cell, hrubber, R),
	debug(table, print_rubber(Cell, hrubber)).

:- pce_group(attributes).

frame(T, Frame:name) :->
	send(T?layout_manager, frame, Frame).

rules(T, Rules:name) :->
	send(T?layout_manager, rules, Rules).

border(T, Border:'0..') :->
	get(T, layout_manager, Table),
	send(Table, border, Border),
	(   Border > 0
	->  send(Table, rules, all),
	    get(Table, cell_padding, size(W, H)),
	    PW is max(W, Border//2+5),
	    PH is max(H, Border//2+2),
	    send(Table, cell_padding, size(PW, PH))
	;   true
	).
	
specify_table_width(T, Width:name) :->	% from HTML spec
	table_width(Width, CW),
	(   CW = percent(Rel)
	->  send(T, attribute, relative_width, Rel)
	;   send(T, attribute, fixed_width, CW)
	).

col_spec(T, Options:prolog) :->
	"Set attributes for the next column(s)"::
	get(T, layout_manager, Table),
	get(Table?columns, high_index, HI),
	Start is HI+1,
	option(span(Span), Options, 1),
	End is Start+Span-1,
	(   between(Start, End, ColN),
	        get(Table, column, ColN, @on, Col),
	        set_col_options(Col, Options),
	    fail
	;   true
	).

set_col_options(Col, Options) :-
	(   option(width(Width), Options),
	    column_width(Width, CW)
	->  (	CW = *(Rel)
	    ->	send(Col, attribute, relative_width, Rel)
	    ;	(   get(Col, attribute, fixed_width, CW0)
		->  CW1 is max(CW0, CW)
		;   CW1 = CW
		),
		send(Col, attribute, fixed_width, CW1)
	    )
	).
set_col_options(Col, Options) :-
	(   option(align(Align), Options)
	->  send(Col, halign, Align)
	).
set_col_options(Col, Options) :-
	(   option(bgcolor(Colour), Options)
	->  catch(new(C, colour(Colour)), _, fail),
	    send(Col, background, C)
	).
	
row_group(T, _Options:prolog) :->
	"Start a new row group using options"::
	get(T, layout_manager, Table),
	get(Table, current, point(_, Y)),
	(   get(Table, row, Y, Row)
	->  send(Row, end_group, @on)
	;   true
	).

table_option(width(W), T) :-
	send(T, specify_table_width, W).
table_option(align(_), _).
table_option(bgcolor(Colour), O) :-
	catch(new(C, colour(Colour)), _, fail),
	send(O, background, C).
table_option(cellpadding(X), T) :-
	send(T?layout_manager, cell_padding, X).
table_option(cellspacing(X), T) :-
	send(T?layout_manager, cell_spacing, X).

cell_option(colspan(X), Cell) :-
	send(Cell, col_span, X).
cell_option(rowspan(X), Cell) :-
	send(Cell, row_span, X).
cell_option(cellpadding(X), Cell) :-
	send(Cell, cell_padding, X).
cell_option(valign(HTML), Cell) :-
	valign(HTML, L),
	send(Cell, valign, L).
cell_option(align(X), Cell) :-
	send(Cell, halign, X).
cell_option(bgcolor(C), Cell) :-
	send(Cell, background, colour(C)).
cell_option(background(_), Cell) :-
	debug(table, 'Ignoring background for ~p~n', [Cell]).
cell_option(width(W), Cell) :-
	get(Cell, column, ColN),
	get(Cell, table, Table),
	get(Table, column, ColN, @on, Col),
	set_col_options(Col, [width(W)]).

valign(middle,   center) :- !.
valign(baseline, bottom) :- !.		% for now
valign(X, X).


		 /*******************************
		 *	GEOMETRY HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Table geometry handling is all about managing the width of the table.  There
are three types of columns:

	* Unspecified width
	* Pixel-size specified width
	* Relative width

Resolving the column width distribution, we will:

	* Walk along the columns, setting the parbox at that cell to

		+ The correct with on relative-width columns
		+ Infinite (autocropping) on unspecified columns

        * Compute the columns
	* See how much should be changed
	* Distribute the pain, increasingly on the wider columns
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(geometry).

container_size_changed(T, W:[int], _H:[int]) :->
	(   get(T, natural_width, NW),
	    integer(NW),		% we have had ->compute_dimensions
	    W \== @default,
	    \+ get(T, line_width, W)
	->  send(T, slot, line_width, W),
	    get(T, layout_manager, Table),
	    (	get(T, attribute, fixed_width, FW)
	    ->  NewW is min(W, FW),
		send(Table, width, NewW)
	    ;	get(T, attribute, relative_width, RW)
	    ->  NewW is (W*RW)//100,
		send(Table, width, NewW)
	    ;	(   NW > W
		->  send(Table, width, W)
		;   send(Table, width, @default)
		)
	    )
	;   true
	).


compute_dimensions(T) :->
	"(re)compute the column-width specification"::
	get(T, width, NW),
	send(T, slot, natural_width, NW),
	get(T, layout_manager, Table),
	get(Table, column_range, tuple(L, H)),
	get(Table, row_range, tuple(RL, RH)),
	compute_columns(L, H, RL, RH, Table).


compute_columns(N, H, RL, RH, Table) :-
	N =< H, !,
	NN is N + 1,
	get(Table, column, N, C),
	compute_column(C, RL, RH),
	compute_columns(NN, H, RL, RH, Table).
compute_columns(_, _, _, _, _).

compute_column(C, _, _) :-
	get(C, attribute, fixed_width, W), !,
	get(C, index, ColN),
	debug(table, 'Col ~w: fixed width = ~w~n', [ColN, W]),
	new(R, rubber(1, 10, 10)),
	send(R, natural, W),
	send(C, rubber, R).
compute_column(C, _, _) :-
	get(C, attribute, relative_width, W), !,
	get(C, index, ColN),
	debug(table, 'Col ~w: Reletive width = *~w~n', [ColN, W]),
	Stretch is 100*W,		% ???
	new(R, rubber(1, Stretch, 0)),
	send(R, shrink, 0),
	send(R, stretch, Stretch),
	send(R, natural, 0),
	send(C, rubber, R).
compute_column(C, _L, _H) :-
	send(C, rubber, @default),	% make column compute rubber
	debug(table, print_rubber(C, rubber)).

:- pce_end_class.


		 /*******************************
		 *	  LAYOUT MANAGER	*
		 *******************************/

:- pce_begin_class(doc_table_manager, table,
		   "Table for the document rendering system").

stretched_column(T, Col:table_column, W:int) :->
	"Column has been stretched to specified width"::
	get(Col, index, I),
	debug(table, 'Stretching column ~w to width = ~w~n', [I, W]),
	send(Col, for_all, message(T, stretched_cell, @arg1, W)),
	send_super(T, stretched_column, Col, W).

stretched_cell(T, Cell:table_cell, W:int) :->
	(   get(Cell, col_span, 1)
	->  image_width(Cell, W, IW),
	    get(Cell, image, Image),
	    get(Cell, row, R),
	    get(Cell, column, C),
	    debug(table, '~p: Cell ~w,~w to width = ~w~n', [T, C, R, IW]),
	    send(Image, auto_crop, @off),
	    send(Image, line_width, IW)
	;   true
	).

image_width(Cell, W, IW) :-
	get(Cell, cell_padding, CP),
	CP \== @default, !,
	get(CP, width, PW),
	IW is max(0, W - 2*PW).
image_width(Cell, W, IW) :-
	get(Cell?table, cell_padding, CP),
	get(CP, width, PW),
	IW is max(0, W - 2*PW).

:- pce_end_class.


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

print_rubber(Cell, Sel) :-
	send(Cell, instance_of, table_cell), !,
	get(Cell, column, Col),
	get(Cell, row, Row),
	get(Cell, Sel, Rubber),
	format('Cell at ~w,~w: ', [Col, Row]),
	print_rubber(Rubber).
print_rubber(Col, Sel) :-
	send(Col, instance_of, table_column), !,
	get(Col, index, ColN),
	get(Col, Sel, Rubber),
	format('Col ~w: ', [ColN]),
	print_rubber(Rubber).

print_rubber(R) :-
	get(R, natural, N),
	get(R, minimum, Min),
	get(R, maximum, Max),
	get(R, stretch, Stretch),
	get(R, shrink, Shrink),
	format('~w<~w<~w <~w >~w~n', [Min, N, Max, Shrink, Stretch]).


