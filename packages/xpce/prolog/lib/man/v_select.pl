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

:- module(man_select_graphical, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library lets the user select an   XPCE graphical by pointing at it.
It is an example of low-level focus and cursor management and is used by
the visual hierarchy and inspector  tools   to  select  objects from the
screen.

A typical sequence using this class is:

select_graphical(Cond, Gr) :-
	get(new(D, select_graphical), select, Cond, Gr),
	send(D, destroy),
	Gr \== @nil.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(select_graphical, dialog,
		   "Select a graphical by clicking it").

variable(condition,	 [code],  get, "Condition for target").
variable(current_target, visual*, get, "Current target").

initialise(D, Label:[char_array]) :->
	default(Label, 'Please select an object', TheLabel),
	send_super(D, initialise, 'Selector'),
	send(D, kind, popup),
	send(D, append, label(help, TheLabel)),
	send(D, append, new(Cancel, button(cancel))),
	send(Cancel, alignment, right),
	send(D, resize_message, message(D, layout, @arg2)).

cancel(D) :->
	send(D, grab_pointer, @off),
	send(D, return, @nil).

select(D, Condition:[code], Pos:[point], V:visual) :<-
	"Actually select the target"::
	send(D, slot, condition, Condition),
	send(D, open_centered, Pos),
	send(D, wait),
	send(D, grab_pointer, @on),
	send(D, cursor, crosshair),
	get(D, confirm, V),
	send(D, grab_pointer, @off).

event(D, Ev:event) :->
	"Process and event"::
	(   send_super(D, event, Ev)
	->  true
	;   send(Ev, is_a, 27)		% ESC
	->  send(D, cancel)
	;   send(Ev, is_a, loc_move)
	->  send(D, indicate, Ev)
	;   send(Ev, is_a, ms_left_up),
	    send(D, done, Ev)
	).


indicate(D, Ev:event) :->
	"Indicate current target"::
	(   get(D, target, Ev, Target)
	->  send(D, cursor, hand2),
	    (	get(D, current_target, Target)
	    ->	true
	    ;	send(D, slot, current_target, Target),
		send(D, indicate_target, Target)
	    )
	;   send(D, cursor, crosshair)
	).


indicate_target(D, Target:visual) :->
	"Report the current target to the user"::
	portray_object(Target, Term),
	term_to_atom(Term, Text),
	send(D, report, status, '%s', Text).


done(D, Ev:event) :->
	"User has left-clicked"::
	get(Ev, click_displacement, Displacement),
	(   Displacement > 5
	->  send(D, cancel)
	;   get(D, target, Ev, Target),
	    send(D, return, Target)
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a sequence to find a   graphical satisfying <-condition from the
current cursor location. A similar algorithm is by library(dragdrop).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

target(D, Ev:event, Target:visual) :<-
	"Get current target from event"::
	get(D, condition, Cond),
	get(Ev, inside_sub_window, Frame),
	get(Ev, inside_sub_window, Frame, Window),
	(   Window == D
	->  !, fail
	;   get(Window, find, Ev, Cond, Target0),
	    target(Target0, Ev, Cond, Target)
	->  true
	;   (   Cond == @default
	    ;	send(Cond, forward, Frame)
	    )
	->  Target = Frame
	).

%	Deal with windows displayed on windows

target(Here, Ev, Cond, Target) :-
	send(Here, instance_of, window),
	get(Here, find, Ev, Cond, Target0),
	Target0 \== Here, !,
	target(Target0, Ev, Cond, Target).
target(Here, _, @default, Here) :- !.
target(Here, _, Cond, Here) :-
	send(Cond, forward, Here).

:- pce_end_class.

