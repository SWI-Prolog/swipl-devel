fill_pattern(@white_image).
fill_pattern(@grey12_image).
fill_pattern(@grey25_image).
fill_pattern(@grey50_image).
fill_pattern(@grey75_image).
fill_pattern(@black_image).

make_fill_pattern_menu(M) :-
	new(M, menu(fill_pattern, marked)),
	send(M, layout, horizontal),
	(   fill_pattern(P),
	    send(M, append, menu_item(P, @default, P)),
	    fail
	;   true
	).

fill_1(P) :-
	new(D, dialog('Fill 1')),
	make_fill_pattern_menu(@fill_pattern_menu),
	send(D, append, @fill_pattern_menu),
	send(new(P, picture), below, D),
	send(D, open).

add_box_1(P) :-
	send(P, display, new(B, box(100,100)), point(20,20)),
	send(B, popup, new(Pop, popup)),
	send(Pop, append,
	     menu_item(fill,
		       message(B, fill_pattern,
			       @fill_pattern_menu?selection))).

fill1 :-
	fill_1(P),
	add_box_1(P).
