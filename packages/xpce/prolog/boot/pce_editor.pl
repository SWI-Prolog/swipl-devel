/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_editor_buttons, []).
:- use_module(pce_realise).
:- use_module(pce_expansion).
:- use_module(pce_global).
:- use_module(pce_principal).

:- pce_global(@editor_recogniser,  make_editor_recogniser).

make_editor_recogniser(G) :-
	new(Editor, @event?receiver),
	new(G, handler_group(new(select_editor_text_gesture),
			     click_gesture(middle, '', single,
					   and(message(Editor, paste),
					       message(Editor, mark_undo))))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines @editor_recogniser, a recogniser called from 

Parts of the specs by Uwe Lesta.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(select_editor_text_gesture, gesture,
		   "Select text in an editor").

variable(selecting,	bool := @off,	get, "Are we making a selection").
variable(down_position, point*,		get, "Position of down-event").
variable(select_timer,  timer*,		get, "Timer for scrolling selection").
variable(select_scroll, {up,down}*,     get, "Direction for scrolling").
variable(editor,	editor*,	get, "Client object").

unlink(G) :->
	send(G, kill_select_timer),
	send_super(G, unlink).

initiate(G, Ev:event) :->
	"Set caret and prepare for selectiong"::
	send(G, slot, down_position, Ev?position),
	get(Ev, receiver, Editor),
	send(G, slot, editor, Editor),
	get(Editor, image, Image),
	get(Image, index, Ev, Index),
	send(Editor, caret, Index),
	get(Ev, multiclick, Multi),
	(   Multi == single
	->  send(G, slot, selecting, @off)
	;   send(G, slot, selecting, @on)
	),
	selection_unit(Multi, Unit),
	send(Editor, selection_unit, Unit),
	send(Editor, selection_origin, Index),
	send(G, place_caret).

selection_unit(single, character).
selection_unit(double, word).
selection_unit(triple, line).

drag(G, Ev:event) :->
	"Extend the selection if selecting"::
	(   (   get(G, selecting, @on)
	    ->	true
	    ;	get(G, down_position, DownPos),
		get(Ev, position, EvPos),
		get(DownPos, distance, EvPos, D),
		D > 25
	    ->  send(G, slot, selecting, @on)
	    )
	->  get(Ev, receiver, Editor),
	    get(Editor, image, Image),
	    (	get(Image, index, Ev, Index)
	    ->  send(G, kill_select_timer),
	        send(Editor, selection_extend, Index),
		send(G, place_caret)
	    ;	get(G, select_timer, Timer),
		Timer \== @nil
	    ->	true
	    ;	get(Ev, y, Y),
		get(Editor, height, H),
		(   Y > H
		->  send(G, setup_select_scroll, up)
		;   Y < 0
		->  send(G, setup_select_scroll, down)
		)
	    ->	true
	    ;   true
	    )
	;   true
	).

terminate(G, _Ev:event) :->
	"If we are selecting, copy the selection"::
	send(G, kill_select_timer),
	get(G, editor, Editor),
	send(G, slot, editor, @nil),
	(   get(G, selecting, @on),
	    get(Editor, class_variable_value, auto_copy, @on)
	->  send(Editor, copy)
	;   true
	).

:- pce_group(select_scroll).

setup_select_scroll(G, Direction:{up,down}) :->
	"Initiate scrolling while extending the selection"::
	send(G, slot, select_scroll, Direction),
	get(G, repeat_interval, Time),
	send(G, slot, select_timer,
	     new(T, timer(Time, message(G, select_scroll)))),
	send(T, start).

repeat_interval(_, I:real) :<-
	"Speed for scrolling"::
	(   get(class(scroll_bar), class_variable, repeat_interval, CV),
	    get(CV, value, I),
	    number(I)
	->  true
	;   I = 0.1
	).

kill_select_timer(G) :->
	"Stop and remove the scroll-select timer"::
	(   get(G, select_timer, Timer),
	    Timer \== @nil
	->  send(Timer, stop),
	    send(G, slot, select_timer, @nil)
	;   true
	).

select_scroll(G) :->
	"Scroll and extend the selection"::
	get(G, select_scroll, UpDown),
	get(G, editor, Editor),
	(   UpDown == up
	->  send(Editor, scroll_up, 1),
	    send(Editor, compute),
	    get(Editor?image, end, Index)
	;   send(Editor, scroll_down, 1),
	    send(Editor, compute),
	    get(Editor?image, start, Index)
	),
	send(Editor, selection_extend, Index),
	send(G, place_caret),
	send(Editor, flush).

:- pce_group(util).

place_caret(G) :->
	"Place caret at start of selection"::
	(   get(G, selecting, @on)
	->  get(G, editor, Editor),
	    get(Editor, selection_start, Start),
	    send(Editor, caret, Start)
	;   true
	).

:- pce_end_class.
