colour(white).
colour(red).
colour(green).
colour(blue).
colour(black).

append_colour(M, C) :-
	new(Img, pixmap(@nil, white, black, 32, 16)),
	send(Img, fill, colour(C)),
	send(M, append, menu_item(colour(C), label := Img)).

edit_graphical(Gr) :-
	new(D, dialog(string('Edit graphical %s', Gr?name))),
	send(D, append,
	     new(M, menu(colour, choice,
			 message(Gr, fill_pattern, @arg1)))),
	send(M, layout, horizontal),
	forall(colour(C), append_colour(M, C)),
	send(M, default, Gr?fill_pattern),
	send(D, append, slider(pen, 0, 10, Gr?pen,
			       message(Gr, pen, @arg1))),
	send(D, append, button(apply)),
	send(D, append, button(restore)),
	send(D, append, button(quit, message(D, destroy))),
	send(D, default_button, apply),
	send(D, open).

attributedemo :-
	send(new(P, picture('Attribute Demo')), open),
	send(P, display, new(B, box(100, 100)), point(20, 20)),
	send(P, display, new(E, ellipse(100, 50)), point(150, 20)),
	send_list([B, E], fill_pattern, colour(white)),
	new(C, click_gesture(left, '', double,
			     message(@prolog, edit_graphical, @receiver))),
	send(B, recogniser, C),
	send(E, recogniser, C).
