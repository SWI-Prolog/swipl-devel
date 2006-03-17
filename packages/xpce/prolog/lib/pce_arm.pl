/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2006, University of Amsterdam

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

:- module(pce_arm, []).
:- use_module(pce).
:- use_module(pce_template).
:- use_module(library(debug)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Library  pce_arm.pl  provides  a  mechanism    for  feedback  on  active
graphicals while to mouse is moving  over   them.  It guaranteed that at
most one graphical is `armed' at any time.

Graphical objects that wishes to play a role in this must

	* Ensure to ->event activates @arm_recogniser
	* define the method ->arm: Bool, where @on is send to arm
	  the object and @off to unarm it.  Objects that accepted
	  @on are guaranteed to receive ->arm: @off.

The currently armed object can be requested using

	?- get(@display, armed, Graphical).

The arm library was initially developed for  Triple20. It has been moved
to  the  XPCE  core  library  in   version    5.6.9   for   use  in  the
cross-referencing GUI (See pce_xref.pl).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(arm, template,
		   "(Un)arm objects in a window").

:- pce_group(event).

define_event(Name, Parent) :-
	(   get(@event_tree, node, Name, Node)
	->  (   get(Node?parent, value, Parent)
	    ->	true
	    ;	print_message(error, format('Redefined event ~w', [Name]))
	    )
	;   new(_, event_node(Name, Parent))
	).

:- pce_global(@arm_recogniser,
	      new(handler(arm,
			  message(@event?display, try_arm,
				  @event?receiver)))).
:- initialization
   define_event(arm, user).

event(W, Ev:event) :->
	(   send(Ev, is_a, loc_move),
	    get(W, arm, _Target)
	->  true
	;   send(Ev, is_a, area_exit)
	->  send(@display, arm_object, @nil)
	;   true
	),
	send_super(W, event, Ev).

:- pce_group(arm).

arm(W, For:[name|code], Target:graphical) :<-
	(   get(@event, position, W, point(X, Y)),
	    get(W, slot, scroll_offset, point(OX, OY)),
	    AX is X + OX, AY is Y+OY,
	    new(Ev, event(arm, W, AX, AY)),
	    (	For == @default
	    ->	true
	    ;	send(Ev, attribute, arm_for, For)
	    ),
	    debug(arm, 'Posting arm to ~p at ~d,~d', [W, AX, AY]),
	    send(Ev, post, W)
	->  get(@display, hypered, arm, Target)
	;   send(@display, arm_object, @nil),
	    fail
	).
	
:- pce_end_class(arm).


:- pce_extend_class(display).

try_arm(W, Gr:graphical) :->
	(   get(@event, attribute, arm_for, For)
	->  (   atom(For)
	    ->	send(Gr, has_send_method, For)
	    ;	send(For, forward_receiver, Gr)
	    )
	;   true
	),
	send(W, arm_object, Gr).

arm_object(W, Gr:graphical*) :->
	(   get(W, hypered, arm, Old)
	->  (   Old == Gr
	    ->	true			% no change
	    ;	send(W, delete_hypers, arm),
		send(Old, arm, @off),
		(   Gr \== @nil
		->  send(Gr, arm, @on),
		    new(_, hyper(W, Gr, arm, arm_window))
		;   true
		)
	    )
	;   (   Gr \== @nil
	    ->  send(Gr, arm, @on),
		new(_, hyper(W, Gr, arm, arm_window))
	    ;   true
	    )
	).

armed(W, Gr:graphical) :<-
	"Find currently armed graphical"::
	get(W, hypered, arm, Gr).

:- pce_end_class(display).

