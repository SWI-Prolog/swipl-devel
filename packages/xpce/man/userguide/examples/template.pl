:- use_module(library(pce_template)).

:- pce_begin_class(editable_graphical, template).

:- pce_global(@editable_graphical_recogniser,
	      make_editable_graphical_recogniser).

make_editable_graphical_recogniser(G) :-
	Gr = @arg1,
	new(Dev, Gr?device),
	new(P, popup),
	send_list(P, append,
		  [ menu_item(cut, message(Gr, free)),
		    menu_item(duplicate,
			      message(Dev, display, Gr?clone,
				      ?(Gr?position, plus,
					point(10, 10))))
		  ]),
	new(G, handler_group(new(resize_gesture),
			     new(move_gesture),
			     popup_gesture(P))).


event(G, Ev:event) :->
	(   send(G, send_super, event, Ev)
	;   send(@editable_graphical_recogniser, event, Ev)
	).
:- pce_end_class.

:- require([use_class_template/1]).

:- pce_begin_class(editable_box, box).
:- use_class_template(editable_graphical).
:- pce_end_class.

:- pce_begin_class(editable_ellipse, ellipse).
:- use_class_template(editable_graphical).
:- pce_end_class.

test :-
	send(new(P, picture('Template Demo')), open),
	send(P, display,
	     editable_box(100,50), point(20,20)),
	send(P, display,
	     editable_ellipse(100, 50), point(20, 90)).
