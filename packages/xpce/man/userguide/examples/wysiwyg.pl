/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A typical XPCE/Prolog module header.  Always   make  sure to load module
library(pce) explicitly if you want  to   write  modules portable to the
various Prolog dialects supported by XPCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(wysiwyg,
	  [ wysiwyg/1		% +File
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the main window, consisting of a frame holding a dialog window with
a button for defining new styles and a menu for setting the style of the
selection.  Both dialog items use call-back to @prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

wysiwyg(File) :-
	new(Fr, frame(File)),
	send(Fr, append, new(D, dialog)),
	send(new(V, view), below, D),
	send(V, font, normal),
	send(D, append,
	     button(define_style,
		    message(@prolog, define_style, Fr))),
	send(D, append,
	     menu(style, toggle,
		  and(message(@prolog, set_style, Fr, @arg1),
		      message(V, selection, 0, 0),
		      message(@receiver, clear_selection))),
	     right),
	append_style(Fr, bold,   style(font := bold)),
	append_style(Fr, italic, style(font := italic)),
	send(V, load, File),
	send(Fr, open).
			    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the style for the current selection. Simply pick the selection start
and end and make a  fragment  using   the  selection  parameters and the
style-name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

set_style(Fr, Style) :-
	get(Fr, member, view, V),
	get(V, selection, point(Start, End)),
	(   Start == End
	->  send(Fr, report, warning, 'No selection')
	;   get(V, text_buffer, TB),
	    new(_, fragment(TB, Start, End-Start, Style))
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define a new style and add it to the menu and the view.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

define_style(Fr) :-
	ask_style(Fr, Name, Style),
	append_style(Fr, Name, Style).

append_style(Fr, Name, Style) :-
	get(Fr, member, dialog, D),
	get(D, member, style, Menu),
	send(Menu, append, Name),
	send(Menu, active, @on),
	get(Fr, member, view, View),
	send(View, style, Name, Style).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prompt for the style-name and style-object.  Class style_item is defined
in the library(pce_style_item). `frame ->transient_for' tells the window
manager the dialog is  a  supporting   frame  for  the main application.
`frame <-confirm_centered' opens the  frame   centered  around the given
location  and  starts  processing  events    until   `frame->return'  is
activated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ask_style(Fr, Name, Style) :-
	new(D, dialog('Define Style')),
	send(D, append,
	     new(N, text_item(name, ''))),
	send(D, append,
	     new(S, style_item(style))),
	send(D, append,
	     button(ok, message(D, return, ok))),
	send(D, append,
	     button(cancel, message(D, return, cancel))),
	send(D, default_button, ok),
	send(D, transient_for, Fr),
	repeat,
	get(D, confirm_centered, Fr?area?center, Answer),
	(   Answer == ok
	->  get(N, selection, Name),
	    (	Name == ''
	    ->	send(D, report, error,
		     'Please enter a name'),
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
	
