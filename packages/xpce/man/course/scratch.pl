edit_box(Box) :-
	new(D, dialog(string('Edit box %N', Box))),
	send(D, append,
	     text_item(name, Box?name, message(Box, name, @arg1))),
	send(D, append,
	     new(S, slider(pen, 0, 10, Box?pen, message(Box, pen, @arg1)))),
	send(S, width, 50),
	send(D, append, button(apply)),
	send(D, append, button(restore)),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, default_button, apply),
	send(D, open).

create_window :-
	new(D, dialog('Create a new window')),
	send(D, append, new(Label, text_item(label, 'Untitled'))),
	send(D, append, new(Class, menu(class, choice))),
	send_list(Class, append,
		  [ class(window),
		    class(picture),
		    class(dialog),
		    class(browser),
		    class(view)
		  ]),
	send(D, append,
	     new(Width, slider(width, 100, 1000, 400))),
	send(D, append,
	     new(Height, slider(height, 100, 1000, 200))),
	send(D, append,
	     button(create,
		    message(@prolog, create_window,
			    Class?selection,
			    Label?selection,
			    Width?selection,
			    Height?selection))),
	send(D, append,
	     button(cancel, message(D, destroy))),
	send(D, open).


create_window(Class, Label, Width, Height) :-
	get(Class, instance, Label, Window),
	send(Window?frame, set, @default, @default, Width, Height),
	send(Window, open).
	

quit :-
	new(D, dialog('Quit my lovely application?')),
	(   application_is_modified
	->  send(D, append,
		 button(save_and_quit,
			message(D, return, save_and_quit)))
	;   true
	),
	send(D, append, button(quit, message(D, return, quit))),
	send(D, append, button(cancel, message(D, return, cancel))),

	get(D, confirm, Action),
	send(D, destroy),
	(   Action == save_and_quit
	->  save_application
	;   Action == quit
	->  true
	),
	send(@pce, die).
