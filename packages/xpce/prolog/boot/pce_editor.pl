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

:- module(editor_buttons, []).
:- use_module(pce_boot(pce_principal)).
:- use_module(pce_boot(pce_realise), [pce_register_class/1]).

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
:- initialization
	make_editor_recogniser(@editor_recogniser).
