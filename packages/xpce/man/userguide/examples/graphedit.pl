/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

colour(white).
colour(red).
colour(green).
colour(blue).
colour(black).

append_colour(M, C) :-
	new(Img, pixmap(width := 32, height := 16)),
	send(Img, fill, colour(C)),
	send(M, append, menu_item(C, label := Img)).

edit_graphical(Gr) :-
	new(D, dialog(string('Edit graphical %s', Gr?name))),
	send(D, append, new(M, menu(colour, choice))),
	send(M, layout, horizontal),
	forall(colour(C), append_colour(M, C)),
	send(M, default, Gr?colour),
	send(M, message, message(Gr, colour, @arg1)),
	send(D, append,
	     new(X, text_item(x, Gr?x,
			      message(Gr, x, @arg1)))),
	send(D, append,
	     new(Y, text_item(y, Gr?y,
			      message(Gr, y, @arg1)))),
	send(D, append,
	     new(W, text_item(width, Gr?width,
			      message(Gr, width, @arg1)))),
	send(D, append,
	     new(H, text_item(height, Gr?height,
			      message(Gr, height, @arg1)))),
	send_list([X, Y,W,H], length, 4),
	send(D, append, button(apply)),
	send(D, append, button(restore)),
	send(D, append, button(quit, message(D, destroy))),
	send(D, open).

attributedemo :-
	send(new(P, picture('Attribute Demo')), open),
	send(P, display, new(B, box(100, 100)), point(20, 20)),
	send(P, display, new(E, ellipse(100, 50)), point(150, 20)),
	new(C, click_gesture(left, '', double,
			     message(@prolog, edit_graphical, @receiver))),
	send(B, recogniser, C),
	send(E, recogniser, C).
