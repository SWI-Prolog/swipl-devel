/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After this file is loaded, pressing ALT-ESC releases event focussing. It
may be used for debugging purposes  as   well  as for making screendumps
holding popup-windows: load this file, open   the requested popups using
`clicking' so they remain open and then   hit  ALT-ESC to keep the popup
open while releasing the pointer and keyboard.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(stayup_popup,
	  [ install_unfocus_hook/0
	  ]).
:- use_module(library(pce)).

install_unfocus_hook :-
	send(@display, inspect_handler,
	     handler(65563,		% ALT-ESC
		     message(@prolog, unfocus))).

unfocus :-
	format('Destroying XPCE focus~n'),
	get(@event, window, W),
	format('Releasing ~p~n', [W]),
	send(W, focus, @nil),
	send(W, grab_pointer, @off),
	send(W, grab_keyboard, @off).

:- initialization(install_unfocus_hook).




