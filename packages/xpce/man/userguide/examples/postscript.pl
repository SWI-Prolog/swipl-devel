postscript(Data) :-
	  new(D, dialog('Print destination')),
	  send(D, append, new(T, menu(destination, marked))),
	  send_list(T, append, [printer, file]),
	  send(T, layout, horizontal),
	  send(D, append, new(A, text_item(printer_name, 'PostScript'))),
	  send(T, message,
	       if(T?selection == printer,
		  message(A, label, ?(A, label_name, printer_name)),
		  message(A, label, ?(A, label_name, file_name)))),
	  send(D, append,
	       button(ok, and(message(@prolog, print_postscript,
				      T?selection, A?selection, Data),
			      message(D, destroy)))),
	  send(D, append,
	       button(cancel, message(D, destroy))),
	  send(D, open).
	  
print_postscript(printer, Address, Data) :- !,
	  new(F, file),
	  send(F, open, write),
	  send(F, append, Data),
	  send(F, close),
	  get(F, name, TmpFile),
	  get(string('lpr -P%s %s &', Address, TmpFile),
	      value, Command),
	  unix(shell(Command)).
print_postscript(file, Address, Data) :-
	  new(F, file(Address)),
	  send(F, open, write),
	  send(F, append, Data),
	  send(F, close).
