connect_demo :-
	send(new(P, picture('Connect Demo')), open),
	send(P, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, display_box,
				   P, @event?position))).

:- pce_global(@east,  new(handle(0, h/2, any, east))).
:- pce_global(@north, new(handle(w/2, 0, any, north))).
:- pce_global(@west,  new(handle(w, h/2, any, west))).
:- pce_global(@south, new(handle(w/2, h, any, south))).

:- pce_global(@connect_recogniser,
	      new(handler_group(connect_gesture(left, '', link(any)),
				new(move_gesture)))).

display_box(P, Pos) :-
	send(P, display, new(B, box(30,30)), Pos),
	send_list(B, handle,
		  [ @east, @north, @west, @south
		  ]),
	send(B, recogniser, @connect_recogniser).
	
