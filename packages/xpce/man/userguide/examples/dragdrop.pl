/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Below is a complete example that allows the user to drag objects for
moving and copying on another window.

Class drop_picture defines a  graphical   window  that imports graphical
objects when they are  dropped  onto  it.    The  feedback  is  a dotted
rectangle indicating the area of  the   graphical  to  be imported.  See
`graphical->preview_drop' for a description of the arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(drop_picture, picture).

preview_drop(P, Gr:graphical*, Pos:[point]) :->
	(   Gr == @nil			% pointer leaves area
	->  (   get(P, attribute, drop_outline, OL)
	    ->	send(OL, free),
		send(P, delete_attribute, drop_outline)
	    ;	true
	    )
	;   (   get(P, attribute, drop_outline, OL)
	    ->	send(OL, position, Pos)
	    ;	get(Gr?area, size, size(W, H)),
		new(OL, box(W, H)),
		send(OL, texture, dotted),
		send(P, display, OL, Pos),
		send(P, attribute, drop_outline, OL)
	    )
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method ->drop. If the  graphical   originates  from the same picture
just move it. Otherwise <-clone the graphical and display the clone.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drop(P, Gr:graphical, Pos:point) :->
	(   get(Gr, device, P)
	->  send(Gr, position, Pos)
	;   get(Gr, clone, Gr2),
	    send(P, display, Gr2, Pos)
	).

:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class dragbox defines a simple subclass of class box that can be resized
and dragged.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(dragbox, box).

:- pce_autoload(drag_and_drop_gesture, library(dragdrop)).
:- pce_global(@dragbox_recogniser, make_dragbox_recogniser).

make_dragbox_recogniser(G) :-
	new(G, handler_group(resize_gesture(left),
			     drag_and_drop_gesture(left))).

event(B, Ev:event) :->
	(   send(B, send_super, event, Ev)
	;   send(@dragbox_recogniser, event, Ev)
	).

:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The toplevel predicate creates two drop_pictures in one frame (note that
drag-and-drop-gestures work accross frames,  but   not  accross multiple
XPCE processes at the moment).  It displays   one  dragbox in one of the
windows.  Dragging it inside a picture moves the box, dragging it to the
other windows makes a copy of the box.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

dragdropdemo :-
	new(F, frame('Drag and Drop Demo')),
	send(F, append, new(P1, drop_picture)),
	send(new(drop_picture), right, P1),
	send(P1, display, dragbox(100, 50), point(20,20)),
	send(F, open).
