/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(demo_dialog,
	  [ demo_dialog/0
	  , demo_dialog/1
	  ]).

:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

demo_dialog :-
	demo_dialog(_).

demo_dialog(D) :-
	new(D, dialog('Demo Dialog Window')),
						  % menu_bar
	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(PF, popup(file))),
	send(MB, append, new(ED, popup(edit))),
	send_list(PF, append,
		  [ menu_item(version,
			      message(@display, inform,
				      'XPCE Version %s', @pce?version),
			      @default, @on)
		  , menu_item(quit_pce,
			      message(@pce, die))
		  ]),
	send_list(ED, append,
		  [ menu_item(cut,
			      message(@pce, write_ln, 'Cut'))
		  , menu_item(copy,
			      message(@pce, write_ln, 'Copy'),
			      @default, @on)
		  , menu_item(paste,
			      message(@pce, write_ln, 'Paste'))
		  ]),
	
	send_list(D, append,
		  [ label(feedback, 'I''m a label')
		  , text_item(user, @pce?user,
			      message(@pce, format, 'Hello %s\n', @arg1))
		  , slider(speed, 0, 100, 20,
			   message(@pce, format, 'Speed %d\n', @arg1))
		  ]),

	send(D, append, new(M, menu(fill_pattern, marked,
				    message(@pce, format,
					    'Fill with %O\n', @arg1)))),
	send(M, layout, horizontal),
	fill_pattern_menu(M),

	send_list(D, append,
		  [ button(ok, message(@pce, write_ln, ok))
		  , button(quit, message(D, destroy))
		  ]),

	send(D, open).

fill_pattern(@white_image).
fill_pattern(@grey12_image).
fill_pattern(@grey25_image).
fill_pattern(@grey50_image).
fill_pattern(@grey75_image).
fill_pattern(@black_image).


fill_pattern_menu(M) :-
	fill_pattern(Pattern),
	send(M, append, menu_item(Pattern, @default, Pattern)),
	fail ; true.
