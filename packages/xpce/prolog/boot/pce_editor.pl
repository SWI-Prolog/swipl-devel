/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_editor_buttons, []).
:- use_module(pce_global).
:- use_module(pce_principal, [send/2, send/3, send/4, send/6,
			      get/3, get/4,
			      new/2]).
:- use_module(pce_realise, [pce_extended_class/1]).
:- use_module(pce_error, [pce_catch_error/2]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines how the pointer-buttons are handled by class editor.
The   method   `editor   ->event'   will   check   the   global   object
@editor_recogniser.  If it exists,  it  will   be  invoked  *after*  the
general device event method, typing and area enter/exit events have been
tried.  If it does not exist, the predefined button actions are invoked.

This module attempts to avoid using modifiers and be consistent with
other X11 applications:

	* Left-click sets the caret
	* Left-drag makes a selection
	* Right click/drag extends the selection if it exists
	* left-double/tripple click selects a word/line.  Subsequent
	  dragging extends the selection with this unit.

Comments are welcome!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_global(@editor_recogniser, make_editor_recogniser).

make_editor_recogniser(R) :-
	new(Editor, @event?receiver),
	new(Image, Editor?image),
	new(Index, ?(Image, index, @event)),

	new(R, handler_group),
	send(R, attribute, attribute(saved_caret, 0)),
	send(R, attribute, attribute(down_index, 0)),

	new(DownIndex, R?down_index),

	send(R, append,
	     new(L, click_gesture(left, '', single,
			  and(message(Editor, caret, DownIndex),
			      message(Editor, selection_unit, character),
			      message(Editor, selection_origin, DownIndex)),
			  and(message(R, saved_caret, Editor?caret),
			      message(R, down_index, Index),
			      if(?(Editor, resource_value,
				   caret_moves_on_select) == @on,
				 message(Editor, caret, DownIndex)))))),
	send(L, max_drag_distance, 25),			  
				  
	send(R, append,
	     click_gesture(middle, '', single,
			   and(message(Editor, paste),
			       message(Editor, mark_undo)))),

	make_make_selection_gesture(R, MakeSelectionGesture),
	make_extend_selection_gesture(ExtendGesture),
	send(R, append, MakeSelectionGesture),
	send(R, append, ExtendGesture).

	
make_make_selection_gesture(R, G) :-
	new(G, gesture),
	new(Editor, @arg1?receiver),
	new(Image, Editor?image),
	new(Index, ?(Image, index, @arg1)),
	
	send(G, send_method,
	     send_method(initiate, vector(event),
		 and(message(Editor, selection_unit,
			     when(@arg1?multiclick == single,
				  character,
				  progn(if(?(Editor, resource_value,
					     caret_moves_on_select) == @off,
					   message(Editor, caret,
						   R?saved_caret)),
					when(@arg1?multiclick == double,
					     word, line)))),
		     message(Editor, selection_origin, Index)))),
	send(G, send_method,
	     send_method(drag, vector(event),
			 message(Editor, selection_extend, Index))),
	send(G, send_method,
	     send_method(terminate, vector(event),
			 and(message(Editor, selection_extend, Index),
			     if(?(Editor, resource_value, auto_copy) == @on,
				message(Editor, copy))))).
					 

make_extend_selection_gesture(G) :-
	new(G, gesture(right)),
	new(Editor, @arg1?receiver),
	new(Image, Editor?image),
	new(Index, ?(Image, index, @arg1)),
	
	send(G, condition, Editor?selection_start \== Editor?selection_end),

	send(G, send_method,
	     send_method(initiate, vector(event),
		 and(if(@arg1?multiclick \== single,
			message(Editor, selection_unit,
			     when(@arg1?multiclick == double,
				  word,
				  line))),
		     message(Editor, selection_extend, Index)))),
	send(G, send_method,
	     send_method(drag, vector(event),
			 message(Editor, selection_extend, Index))),
	send(G, send_method,
	     send_method(terminate, vector(event),
			 and(message(Editor, selection_extend, Index),
			     if(?(Editor, resource_value, auto_copy) == @on,
				message(Editor, copy))))).
					 
