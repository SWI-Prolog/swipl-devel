:- pce_begin_class(mail_tool, frame, 'Mail(1) front-end').

variable(mail,		process,	get,	"The mail process").

initialise(MT) :->
	send(MT, send_super, initialise, 'PceMail --- version 0.0'),
	send(MT, append, new(Headers, browser)),
	send(new(Cmd, dialog), below, Headers),
	send(new(View, view), below, Cmd),
	
	setup_headers(Headers),
	setup_cmd(Cmd),
	setup_process(MT),
	send(MT, open).

setup_headers(Browser) :-
	get(Browser, frame, Tool),
	new(Current, Tool?current),

	send(Browser, multiple_selection, @on),
	send(Browser, open_message, message(Tool, view, Current)).

setup_cmd(Dialog) :-
	get(Dialog, frame, Tool),
	new(Current, Tool?current),
	
	send(Dialog, append, label(reporter)),
	send(Dialog, append, button(print, message(Tool, view, Current))),
	send(Dialog, append, button(reply, message(Tool, reply, Current))),
	send(Dialog, append, button(compose, message(Tool, compose))),
	send(Dialog, append, button(send, message(Tool, send))),
	send(Dialog, append, button(quit, message(Tool, quit))).
	     
setup_process(Tool) :-
	send(Tool, slot, mail, new(Mail, process(mail))),
	send(Tool, wait_for_prompt),
	send(Tool, format, '

open(Tool, Pos:[point]) :->
	"Start process, read headers and open frame"::
	send(Tool, open),
	send(Tool, read_headers).

read_headers(Tool) :->
	send(Tool, wait_for_prompt),
	send(Tool, format, 'h *\n'),
	


:- pce_end_class.





