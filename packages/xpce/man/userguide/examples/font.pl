:- module(wysiwyg,
	  [ wysiwyg/1		% +File
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).

wysiwyg(File) :-
	new(Fr, frame(File)),
	send(Fr, append, new(D, dialog)),
	send(D, append,
	     button(define_style, message(@prolog, define_style, Fr))),
	send(D, append,
	     menu(style, toggle,
		  and(message(@prolog, set_style, Fr, @arg1),
		      message(@receiver, clear_selection))),
	     right),
	send(new(V, view), below, D),
	send(V, load, File),
	send(Fr, open).
			    
set_style(Fr, Style) :-
	get(Fr, member, view, V),
	get(V, selection, point(Start, End)),
	(   Start == End
	->  send(Fr, report, warning, 'No selection')
	;   get(V, text_buffer, TB),
	    new(_, fragment(TB, Start, End-Start, Style))
	).

define_style(Fr) :-
	ask_style(Fr, Name, Style),
	get(Fr, member, dialog, D),
	get(D, member, style, Menu),
	send(Menu, append, Name),
	send(Menu, active, @on),
	get(Fr, member, view, View),
	send(View, style, Name, Style).

ask_style(Fr, Name, Style) :-
	new(D, dialog('Define Style')),
	send(D, append, new(N, text_item(name, ''))),
	send(D, append, new(S, style_item(style))),
	send(D, append, button(ok, message(D, return, ok))),
	send(D, append, button(cancel, message(D, return, cancel))),
	send(D, default_button, ok),
	send(D, transient_for, Fr),
	repeat,
	get(D, confirm_centered, Fr?area?center, Answer),
	(   Answer == ok
	->  get(N, selection, Name),
	    (	Name == ''
	    ->	send(D, report, error, 'Please enter a name'),
		fail
	    ;	!,
	        get(S, selection, Style),
		send(Style, lock_object, @on),
		send(D, destroy)
	    )
	;   !,
	    send(D, destroy),
	    fail
	).
	
