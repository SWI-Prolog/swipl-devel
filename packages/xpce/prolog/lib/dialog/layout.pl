/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_dialog_layout,
	  [ layout_dialog/1
	  ]).
:- use_module(library(pce)).
:- require([ append/3
	   , checklist/2
	   , delete/3
	   , forall/2
	   , get_chain/3
	   , ignore/1
	   , last/2
	   , maplist/3
	   , member/2
	   , sublist/3
	   , subset/2
	   , subtract/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

To be recognised:

	1) rows/columns of objects.  Alignment: top/bottom/reference/center
	2) nesting?

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


layout_dialog(D) :-
	send(D?graphicals, for_all,
	     and(message(@arg1, right, @nil),
		 message(@arg1, above, @nil),
		 message(@arg1, left, @nil),
		 message(@arg1, below, @nil))),
	get_chain(D, graphicals, Grs),
	get(D, overlay, Overlay),
	delete(Grs, Overlay, Items),
	make_rows(Items, Rows),
	sort_rows(Rows, Sorted),
	update_order(Sorted),
	make_alignments(Sorted),
	relate_dialog_items(Sorted),
	send(D, layout),
	send(Overlay, expose),
	send(D, fit).


%	make_rows/2
%	Split list of graphicals into nested list of graphicals with
%	approximately the same Y

make_rows([], []).
make_rows([Gr1|Rest], [[Gr1|RestRow]|RestRows]) :-
	sublist(same_row(Gr1), Rest, RestRow),
	subtract(Rest, RestRow, Grs),
	make_rows(Grs, RestRows).

same_row(Gr1, Gr2) :-
	get(Gr1, top_side, T1),
	get(Gr1, bottom_side, B1),
	get(Gr2, top_side, T2),
	get(Gr2, bottom_side, B2),
	Overlap is min(B1, B2) - max(T1, T2),
	Smallest is min(B1-T1, B2-T2),
	Overlap >= Smallest // 2.
	
		 /*******************************
		 *	 GEOMETRIC SORTS	*
		 *******************************/

%	sort_rows/2
%	Sort rows in accending y-direction.

sort_rows(Rows, Sorted) :-
	maplist(sort_row, Rows, LSorted),
	maplist(key_with_top_side, LSorted, KeyedRows),
	keysort(KeyedRows, SortedKeyedRows),
	maplist(unkey, SortedKeyedRows, Sorted).

%	sort_row/1
%	Sort row left-to-right

sort_row(Row, Sorted) :-
	maplist(key_with_left_side, Row, KeyedRow),
	keysort(KeyedRow, SortedKeyedRow),
	maplist(unkey, SortedKeyedRow, Sorted).

key_with_top_side(Row, Top-Row) :-
	Row = [Gr1|_],
	get(Gr1, top_side, Top).

key_with_left_side(Gr, Left-Gr) :-
	get(Gr, left_side, Left).

unkey(_-Value, Value).

		 /*******************************
		 *	  GRAPHICAL ORDER	*
		 *******************************/

%	update_order/1
%	Update the order of the graphical objects.

update_order([]) :- !.
update_order([H|T]) :- !,
	update_order(H),
	update_order(T).
update_order(Ref) :-
	send(Ref, expose),
	send(Ref, auto_align, @on).


		 /*******************************
		 *	   ALIGNMENTS		*
		 *******************************/

%	make_alignments/1
%	Fix the alignments of the dialog items.

make_alignments(Rows) :-
	forall(item(Item, Rows),
	       (   get(Item, resource_value, alignment, Alignment),
		   Alignment \== column
	       ->  send(Item, alignment, Alignment)
	       ;   send(Item, alignment, left)
	       )),
	forall(item(Item, Rows),
	       (   get(Item, auto_label_align, @on)
	       ->  ignore(send(Item, label_width, @default))
	       ;   true
	       )),
	make_column_alignments(Rows),
	checklist(make_row_alignment, Rows),
	checklist(make_y_references, Rows).

make_column_alignments([]).
make_column_alignments([_]).
make_column_alignments([R1, R2|Rows]) :-
	(   R1 \== [], R2 \== [],
	    has_column_alignment(R1, R2)
	->  align_columns(R1, R2)
	;   true
	),
	make_column_alignments([R2|Rows]).


has_column_alignment([], _) :- !.
has_column_alignment(_, []) :- !.
has_column_alignment([H1|T1], [H2|T2]) :-
	above(H1, H2),
	has_column_alignment(T1, T2).
	
align_columns([], _) :- !.
align_columns(_, []) :- !.
align_columns([H1|T1], [H2|T2]) :-
	align_above(H1, H2),
	align_columns(T1, T2).


align_above(I1, I2) :-
	send(I1, above, I2),
	send(I1, alignment, column),
	send(I2, alignment, column).


above(Gr1, Gr2) :-
	(   \+ can_be_in_column(Gr1)
	;   \+ can_be_in_column(Gr2)
	), !, fail.
above(Gr1, Gr2) :-
	get(Gr1, auto_label_align, @on),
	get(Gr2, auto_label_align, @on),
	get(Gr1, label_format, right),
	get(Gr2, label_format, right),
	get(Gr1, left_side, L1),
	get(Gr1, label_width, W1),
	get(Gr2, left_side, L2),
	get(Gr2, label_width, W2),
	abs((L1+W1) - (L2+W2)) < 15.
above(Gr1, Gr2) :-
	get(Gr1, left_side, L1),
	get(Gr2, left_side, L2),
	abs(L1 - L2) < 15.


can_be_in_column(Gr) :-
	get(Gr, alignment, column), !.
can_be_in_column(Gr) :-
	get(Gr, fixed_alignment, @off).

make_row_alignment([_]) :- !.
make_row_alignment(Row) :-
	findall(align_row(Sub, Alignment),
		row_alignment(Row, Sub, Alignment),
		Alignments),
format('~p~n', [Alignments]),
	delete_sub_alignments(Alignments, Alignments, Cleaned),
format('~p~n', [Cleaned]),
	checklist(call, Cleaned).


delete_sub_alignments([], _, []).
delete_sub_alignments([H|T], As, L) :-
	H = align_row(S1, _),
	length(S1, L1),
	member(align_row(S2, _), As),
	length(S2, L2),
	L1 < L2,
	subset(S1, S2), !,
	delete_sub_alignments(T, As, L).
delete_sub_alignments([H|T], As, [H|L]) :-
	delete_sub_alignments(T, As, L).


row_alignment(Row, Sub, Alignment) :-
	first(First, Row),
	get(First, device, Device),
	append(Before, R0, Row),
	left(Before, Device, LeftEdge),
	append(Sub, After, R0),
	Sub \== [],
	right(After, Device, RightEdge),
	\+((member(X, Sub), get(X, alignment, column))),
	left_to_right(Sub),
	first(Head, Sub),
	last(Tail, Sub),
	get(Head, left_side, Left),
	get(Tail, right_side, Right),
	left_aligned(LeftEdge, RightEdge, Left, Right, LH),
	right_aligned(LeftEdge, RightEdge, Left, Right, RH),
	center_aligned(LeftEdge, RightEdge, Left, Right, CH),
	(   LH < RH, LH < CH
	->  Alignment = left
	;   RH < CH
	->  Alignment = right
	;   Alignment = center
	).

left([], Device, Left) :-
	get(Device?bounding_box, left_side, Left).
left(L, _, Left) :-
	last(T, L),
%	get(T, alignment, column),
	get(T, right_side, Left).

right([], Device, Right) :-
	get(Device?bounding_box, right_side, Right).
right(L, _, Right) :-
	first(H, L),
%	get(H, alignment, column),
	get(H, left_side, Right).

first(H, [H|_]).

align_row(Row, Alignment) :-
	forall(member(Gr, Row), send(Gr, alignment, Alignment)).

left_to_right([]).
left_to_right([_]).
left_to_right([H1, H2|T]) :-
	get(H1, right_side, R),
	get(H2, left_side, L),
	L - R < 30,
	left_to_right([H2|T]).


left_aligned(X, _W, Left, _Right, H) :-
	H is abs(X-Left).
right_aligned(_X, W, _Left, Right, H) :-
	H is abs(Right - W).
center_aligned(X, W, Left, Right, H) :-
	H is abs((Left+Right)//2 - ((X + W)//2)).

%	make_y_references/1
%	Fix the (Y)-coordinate of the reference points

make_y_references([]).
make_y_references([_]).			% 1 element: doesn't matter
make_y_references(Row) :-
	forall((member(I, Row), get(I, fixed_reference, @off)),
	       make_y_reference(I, Row)).

make_y_reference(I, Row) :-
	findall(Proto, alignment_proto(I, Row, Proto), Protos),
	sort(Protos, [proto(_, I2, Side)|_]), !,
	get(I,  top_side, Y1),
	get(I2, top_side, Y2),
	get(I,  Side, S1),
	get(I2, Side, S2),
	get(I2?reference, y, YR),
	YR2 is (Y2-S2) + YR - (Y1-S1),
	get(I?reference, x, RX),
	send(I, reference, point(RX, YR2)).
make_y_reference(_, _).


alignment_proto(I1, Row, proto(H, I2, Side)) :-
	alignment_side(Side),
	member(I2, Row),
	I2 \== I1,
	get(I2, fixed_reference, @on),
	get(I1, Side, V1),
	get(I2, Side, V2),
	H is abs(V1 - V2).

alignment_side(top_side).
alignment_side(center_y).
alignment_side(bottom_side).


		 /*******************************
		 *	 RELATE THE ITEMS	*
		 *******************************/

%	relate_dialog_items/1
%	Relate the dialog items top/bottom/left/right

relate_dialog_items([]).
relate_dialog_items([H|T]) :-
	relate_dialog_item_row(H),
	relate_dialog_item_rows(H, T),
	relate_dialog_items(T).

relate_dialog_item_row([_]) :- !.
relate_dialog_item_row([H1,H2|T]) :-
	send(H2, right, H1),
	relate_dialog_item_row([H2|T]).

relate_dialog_item_rows(_, []) :- !.
relate_dialog_item_rows([H1|_], [[H2|_]|_]) :-
	send(H2, below, H1).


		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

item(Item, [Row|_]) :-
	member(Item, Row).
item(Item, [_|T]) :-
	item(Item, T).
