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

:- module(man_error_browser, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ ignore/1
	   ]).

:- pce_begin_class(man_error_browser, man_frame,
		   "Error browser").

variable(errors,	man_module,		get).
variable(current,	error*,			get).

initialise(EB, Manual:man_manual) :->
	"Create from Manual"::
	send(EB, send_super, initialise, Manual, 'Error Browser'),

	get(Manual, module, errors, @on, Module),
	send(EB, slot, errors, Module),
	
	new(B, man_summary_browser(man_summary, size(100, 15))),
	send(B?image, tab_stops, vector(15, 220, 280)),
	send(B, name, browser),
	dialog(Dialog),

	send(EB, append, B),
	send(Dialog, below, B),
	send(EB, fill, ''),
	
	send(EB, open).


		/********************************
		*            DIALOG		*
		********************************/

dialog(D) :-
	new(D, dialog),
	new(EB, D?frame),
	new(B, ?(EB, member, browser)),
	send(D, append,
	     new(T, menu(kind, cycle,
			 if(EB?current \== @nil,
			    and(message(EB?current, kind, @arg1),
				message(B, update, EB?current)))))),
	get(class(error), instance_variable, kind, Var),
	get(Var, type, Type),
	get(Type, context, TypeNames),
	send(TypeNames, for_all, message(T, append, @arg1)),
	send(T, active, @off),
	send(D, append, new(SS, text_item(search, regex(''))), right),
	send(D, append, button(apply, message(EB, fill, SS?selection)), right),
	send(D, append, button(help,  message(EB, help))),
	send(D, append, button(quit,  message(EB, quit))),

	send(D, default_button, apply).
	


		/********************************
		*             FILL		*
		********************************/

fill(EB, Pattern:regex) :->
	"Fill with all errors matching pattern"::
	get(EB, member, browser, B),
	send(B, clear),
	new(Chain, chain),
	send(@errors, for_all,
	     if(message(Pattern, search, @arg2?man_summary),
		message(Chain, append, @arg2))),
	get(Chain, size, S),
	send(EB, report, progress, 'Sorting %d objects ...', S),
	send(Chain, sort, ?(@arg1?id, compare, @arg2?id)),
	send(EB, report, done),
	send(Chain, for_all, message(B, append_card, @arg1)),
	ignore(send(EB, selected, EB?current)).


		/********************************
		*          COMMUNICATION	*
		********************************/

selected(EB, Obj:object*) :->
	"Set the selection"::
	send(Obj, instance_of, error),
	send(EB, slot, current, Obj),
	get(?(EB, member, dialog), member, kind, KindMenu),
	send(KindMenu, selection, Obj?kind),
	send(KindMenu, active, @on),
	get(EB, member, browser, B),
	send(B, selected, Obj).


release_selection(EB) :->
	"Clear the selection"::
	get(EB, member, browser, B),
	send(B, release_selection),
	send(?(?(EB, member, dialog), member, kind), active, @off).

:- pce_end_class.

