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

:- module(pce_label_item, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

:- pce_begin_class(dia_label_item, device).

variable(align,		graphical,	get,  "Aligned item").
variable(message,	code*,		both, "Message ran on ->apply"). 
variable(default,	'any|function*',get,  "Function for ->restore").

initialise(LI, Label:name, Default:'[any|function]*', Message:[code]*) :->
	default(Message, @nil, Msg),
	default(Default, @nil, Def),

	send(LI, send_super, initialise),
	send(LI, name, Label),
	send(LI, append_dialog_item,
	     new(T, text_item(string, '', message(LI, apply)))),
	send(LI, slot, align, T),
	send(LI, append_dialog_item,
	     new(M, menu(kind, cycle, message(LI, kind, @arg1))), right),
	send(M, label, ''),
	send(M, append, text),
	send(M, append, image),
	send(T, label, ?(T, label_name, Label)),
	send(LI, layout_dialog),
	send(LI, message, Msg),
	send(LI, default, Def).


selection(LI, Value:'char_array|image') :->
	"Set the selection"::
	(   send(Value, instance_of, image)
	->  send(LI, kind, image),
	    send(?(LI, member, string), selection, Value?name)
	;   send(LI, kind, text),
	    send(?(LI, member, string), selection, Value)
	).

selection(LI, Value:'char_array|image') :<-
	"Get the selection"::
	(   get(?(LI, member, kind), selection, image)
	->  get(?(LI, member, string), selection, Name),
	    (	get(@pce, convert, Name, image, Value)
	    ->	true
	    ;	%send(LI, report, warning, 'No such image'),
		fail
	    )
	;   get(?(LI, member, string), selection, Value)
	).
		

:- pce_global(@path_regex, new(regex('[^:]+'))).

kind(LI, Kind:{text,image}) :->
	"Specify textual or image label"::
	send(?(LI, member, kind), selection, Kind),
	get(LI, member, string, TextItem),
	(   Kind == text
	->  send(TextItem, type, name),
	    send(TextItem, value_set, @default)
	;   send(TextItem, type, image),
	    get(class(image), class_variable_value, path, Path),
	    new(ValueSet, chain),
	    send(@path_regex, for_all, Path,
		 and(assign(new(Dir, var),
			    create(directory,
				   ?(@arg1, register_value, @arg2, 0))),
		     if(message(Dir, exists),
			message(Dir, scan, ValueSet, ValueSet, '.*\\.bm$')))),
	    send(ValueSet, sort),
	    send(ValueSet, unique),
	    send(TextItem, value_set, ValueSet)
	).

kind(LI, Kind:{text,image}) :<-
	"Request current kind"::
	get(?(LI, member, kind), selection, Kind).


clear(LI) :->
	"Set selection to ''"::
	send(?(LI, member, string), selection, '').


default(LI, Default:'[any|function]*') :->
	(   Default == @default
	->  Def = @nil
	;   Def = Default
	),
	send(LI, slot, default, Def),
	send(LI, restore).


modified(LI, Modified:bool) :<-
	"Is selection modified?"::
	(   (   get(?(LI, member, kind), modified, @on)
	    ;   get(?(LI, member, string), modified, @on)
	    )
	->  Modified = @on
	;   Modified = @off
	).


apply(LI, Always:[bool]) :->	    
	(   (Always == @on ; get(LI, modified, @on)),
	    get(LI, message, Msg),
	    Msg \== @nil
	->  send(Msg, forward, LI?selection)
	;   true
	).
	    

restore(LI) :->
	get(LI, default, Function),
	(   Function == @nil
	->  true
	;   send(LI, selection, Function)
	).


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

auto_label_align(LI, Val:bool) :->
	send(LI?align, auto_label_align, Val).
auto_label_align(LI, Val:bool) :<-
	get(LI?align, auto_label_align, Val).

label_width(LI, W:int) :->
	get(LI?align, label_width, Old),
	send(LI?align, label_width, W),
	send(?(LI, member, kind), relative_move, point(W-Old, 0)).
label_width(LI, W:int) :<-
	get(LI?align, label_width, W).

alignment(LI, Alignment:name) :->
	send(LI?align, alignment, Alignment).
alignment(LI, Alignment:name) :<-
	get(LI?align, alignment, Alignment).


		 /*******************************
		 *	      TYPING		*
		 *******************************/

'_wants_keyboard_focus'(_) :->
	true.

:- pce_global(@compound_dialog_recogniser,
	      new(handler_group(handler(obtain_keyboard_focus,
					message(@receiver, advance))))).

event(D, Ev:event) :->
	(   send(@compound_dialog_recogniser, event, Ev)
	->  true
	;   send(D, send_super, event, Ev)
	).

:- pce_end_class.
