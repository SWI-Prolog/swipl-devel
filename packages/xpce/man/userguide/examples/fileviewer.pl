fileviewer(Dir) :-
	new(F, frame('File Viewer')),
	send(F, append, new(B, browser)),
	send(new(D, dialog), below, B),
	send(D, append,
	     button(view, message(@prolog, view,
				  B?selection?key))),
	send(D, append,
	     button(quit, message(F, destroy))),
	send(B, members, directory(Dir)?files),
	send(F, open).

view(F) :-
	send(new(V, view(F)), open),
	send(V, load, F).
