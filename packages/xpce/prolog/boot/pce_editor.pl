/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(editor_buttons, []).

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
variable(editor,	editor*,	get, "Client object").

initialise(G) :->
	send_super(G, initialise),
	send(G, drag_scroll, self).
	

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
	send(G, place_caret, Index).

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
	    ->  send(Editor, selection_extend, Index),
		send(G, place_caret, Index)
	    ;	true
	    )
	;   true
	).

terminate(G, _Ev:event) :->
	"If we are selecting, copy the selection"::
	get(G, editor, Editor),
	send(G, slot, editor, @nil),
	(   get(G, selecting, @on),
	    get(Editor, class_variable_value, auto_copy, @on)
	->  send(Editor, copy)
	;   true
	).

:- pce_group(util).

place_caret(G, Index:int) :->
	"Place caret at start of selection"::
	(   get(G, selecting, @on)
	->  get(G, editor, Editor),
	    get(Editor, selection_start, Start),
	    get(Editor, selection_end, End),
	    (	abs(Start-Index) < abs(End-Index)
	    ->  send(Editor, caret, Start)
	    ;	send(Editor, caret, End)
	    )
	;   true
	).

:- pce_end_class.

:- free(@editor_recogniser).
:- make_editor_recogniser(@editor_recogniser).
