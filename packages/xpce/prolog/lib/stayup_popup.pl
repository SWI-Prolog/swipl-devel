/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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




