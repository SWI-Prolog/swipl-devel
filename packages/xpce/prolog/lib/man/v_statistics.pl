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



:- module(pce_statistics, []).

:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

:- pce_begin_class(man_statistics, man_frame,
		   "Statistics tool").


variable(timer, timer*,	get,
	 "Timer that forces updates").

initialise(S, Manual:man_manual) :->
	"Create from manual"::
	send(S, send_super, initialise, Manual, 'PCE Statistics'),
	
	send(S, append, new(D, dialog)),
	send(D, append, new(CU, text_item(core_in_use,    0))),
	send(D, append, new(CW, text_item(core_wasted,    0))),
	send(D, append, new(OU, text_item(objects_in_use, 0))),
	Items = [CU, CW, OU],
	send_list(Items, length, 10),
	send_list(Items, editable, @off),

	new(T, timer(5,
		     and(message(CU, selection, @pce?core_usage),
			 message(CW, selection, @pce?core_wasted),
			 message(OU, selection, @pce?objects_allocated -
				                @pce?objects_freed)))),

	send(S, slot, timer, T),
	send(T, execute),
	send(T, start),

	send(D, append, button(update, message(T, execute))),
	send(D, append, button(help,   message(S, help))),
	send(D, append, button(quit,   message(S, quit))),

	send(S, open).


unlink(S) :->
	send(S?timer, stop),
	send(S, send_super, unlink).

:- pce_end_class.
