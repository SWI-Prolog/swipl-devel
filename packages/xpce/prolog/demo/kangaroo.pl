/*  $Id$

    Part of XPCE
    Written by Anja van der Hulst and addapted for the latest XPCE
    version by Jan Wielemaker
    E-mail: {anja,jan}@swi.psy.uva.nl
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This demo illustrates (like the juggler  demo) annimation in  xpce.  A
timer is attached to the drawing area that moves all graphicals in the
window by 5 points and sets them in the next annimation status.

Left-click in the drawing area adds a kangaroo to the diagram.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(kangaroo, [ kangaroo/0]).
:- use_module(library(pce)).
:- require([ forall/2
	   , member/2
	   ]).

kangaroo :-
	new(Pict, window('Kangaroo animation demo', size(400, 200))),
	new(Msg, message(Pict?graphicals, for_all,
			 and(message(@arg1, next_status),
			     if(@arg1?right_side > Pict?visible?right_side,
				message(@arg1, x, 0),
				message(@arg1, relative_move, point(5, 0)))))),
	send(Pict, attribute, attribute(timer, new(T, timer(0.1, Msg)))),
	send(T, start),

	send(Pict, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, new_kangaroo,
				   @receiver, @event?position))),

	send(new(D, dialog), below, Pict),
	send(D, append,
	     label(help, 'Left-click in the main window for a new kangaroo')),
	send(D, append,
	     slider(speed, 0, 10, 2, message(@prolog, set_speed, T, @arg1))),

	send(D, append, button(clear, message(Pict, clear))),
	send(D, append, new(Quit, button(quit, and(message(T, free),
						   message(D, destroy))))),
	send(D, done_message, message(Quit, execute)),

	new_kangaroo(Pict, point(20,100)),

	send(Pict, open).


set_speed(T, 0) :- !,
	send(T, stop).
set_speed(T, N) :-
	Time is 1/(9*N),
	send(T, interval, Time),
	send(T, running, @on).


new_kangaroo(Pict, Pos) :-
	new(F, figure),
	forall(member(N, [1,2,3,4,5,6,7,8,9,10,11]),
	       (send(F, display, new(BM, bitmap(string('kangro%d.bm', N)))),
		send(BM, transparent, @on),
	        send(BM, name, N))),
	send(F, status, 1),
	send(Pict, display, F, Pos).
