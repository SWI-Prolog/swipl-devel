myapp :-
	new(Frame, frame('Reporter demo')),
	new(B, browser),
	send(new(picture), right, B),
	send(new(MD, dialog), above, B),
	send(new(RD, dialog), below, B),
	send(Frame, append, B),
	
	send(MD, append, new(MB, menu_bar)),
	send(MD, gap, size(0,0)),
	send(MD, pen, 0),
	send(MB, append, new(File, popup(file))),
	send_list(File, append,
		  [ menu_item(load, message(@prolog, load, Frame),
			      end_group := @on),
		    menu_item(quit, message(Frame, destroy))
		  ]),

	send(RD, append, label(reporter)),
	send(RD, gap, size(5, 0)),
	send(Frame, open).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

load(Frame) :-
	get(@finder, file, exists := @on, FileName),
	send(Frame, report, progress, 'Loading %s ...', FileName),
	get(Frame, member, browser, Browser),
	new(File, file(FileName)),
	send(File, open, read),
	(   repeat,
		(   get(File, read_line, Line)
		->  send(Line, strip),
		    send(Browser, append, Line),
		    fail
		;   !,
		    send(File, close)
		)
	),
	send(Frame, report, done).
