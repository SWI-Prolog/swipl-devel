:- pce_begin_class(fill5, frame).

initialise(F) :->
	send(F, send_super, initialise, 'Fill 5'),
	send(F, append, new(D, dialog)),
	make_fill_pattern_menu(M),
	send(D, append, M),
	send(new(picture), below, D).

current_fill_pattern(F, P:image) :<-
	get(F, member, dialog, D),
	get(D, member, fill_pattern, M),
	get(M, selection, P).

draw_box(F) :->
	get(F, member, picture, P),
	send(P, display, fillbox(100,100), point(20,20)).

:- pce_end_class.

:- pce_begin_class(fillbox, box).

:- pce_global(@fillbox_recogniser, make_fillbox_recogniser).
make_fillbox_recogniser(G) :-
	Gr = @arg1,
	new(G, popup_gesture(new(P, popup))),
	send(P, append,
	     menu_item(fill, message(Gr, fill_pattern,
				     Gr?frame?current_fill_pattern))).

event(B, Ev:event) :->
	(   send(B, send_super, event, Ev)
	;   send(@fillbox_recogniser, event, Ev)
	).
:- pce_end_class.

fill5 :-
	send(new(F, fill5), open),
	send(F, draw_box).
