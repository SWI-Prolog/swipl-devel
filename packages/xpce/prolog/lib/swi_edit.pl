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

:- module(pce_nedit, []).
:- use_module(library(pce_meta)).

:- multifile
	prolog_edit:locate/3,		% +Partial, -FullSpec, -Location
	prolog_edit:locate/2,
	prolog_edit:edit_source/1.
	  

		 /*******************************
		 *      FINDING LOCATIONS	*
		 *******************************/

prolog_edit:(locate(ClassName, class(ClassName), Location) :-
	locate(class(ClassName), Location)).

prolog_edit:locate(class(ClassName), Location) :-	% class(Name)
	atom(ClassName),
	get(@pce, convert, ClassName, class, Class),
	\+ get(Class, creator, built_in),
	source(Class, Location).
prolog_edit:locate(Object, Location) :-			% @reference
	source(Object, Location).
prolog_edit:locate(Object, Location) :-			% @reference
	object(Object),
	get(Object, class, Class),
	source(Class, Location).
prolog_edit:locate(send(Receiver, Selector), Location) :-
	receiver_class(Receiver, Class),
	(   implements(Class, send(Selector), Method),
	    source(Method, Location)
	;   method_source(Class, send(Selector), Location)
	).
prolog_edit:locate(get(Receiver, Selector), Location) :-
	receiver_class(Receiver, Class),
	(   implements(Class, get(Selector), Method),
	    source(Method, Location)
	;   method_source(Class, get(Selector), Location)
	).
prolog_edit:(locate(->(Receiver, Selector), Location) :- !,
	locate(send(Receiver, Selector), Location)).
prolog_edit:(locate(<-(Receiver, Selector), Location) :- !,
	locate(get(Receiver, Selector), Location)).


source(Object, [file(Path)|T]) :-
	object(Object),
	send(Object, has_get_method, source),
	get(Object, source, Loc),
	Loc \== @nil,
	get(Loc, file_name, FileName),
	absolute_file_name(FileName, Path),
	get(Loc, line_no, Line),
	(   integer(Line)
	->  T = [line(Line)]
	;   T = []
	).
	     
receiver_class(Object, Class) :-
	object(Object), !,
	get(Object, class_name, Class).
receiver_class(Class, Class).

method_source(ClassName, send(Selector), [file(File),line(Line)]) :-
	var(ClassName),
	pce_principal:pce_lazy_send_method(Selector, ClassName, Binder),
	arg(4, Binder, source_location(File, Line)),
	\+ get(@classes, member, ClassName, _).
method_source(ClassName, get(Selector), [file(File),line(Line)]) :-
	var(ClassName),
	pce_principal:pce_lazy_get_method(Selector, ClassName, Binder),
	arg(4, Binder, source_location(File, Line)),
	\+ get(@classes, member, ClassName, _).
					   

		 /*******************************
		 *	     EDIT HOOK		*
		 *******************************/

prolog_edit:edit_source(Location) :-
	memberchk(file(File), Location),
	memberchk(line(Line), Location), !,
	Goal = start_emacs, Goal,	% fool xref
	send(@emacs, goto_source_location, source_location(File, Line)).
prolog_edit:edit_source(Location) :-
	memberchk(file(File), Location),
	Goal = start_emacs, Goal,	% fool xref
	send(@emacs, goto_source_location, source_location(File)).


		 /*******************************
		 *	       SELECT		*
		 *******************************/

%	Use GUI-based selection if the request comes from the GUI!

prolog_edit:select_location(Pairs, _Spec, Location) :-
	Pairs \= [_],					% direct match
	object(@event),
	send(@event, instance_of, event), !, 		% GUI initiated
	(   Pairs == []
	->  send(@event?receiver, report, error, 'No match'),
	    Location = []				% Cancel
	;   get(@event?receiver, frame, Frame),
	    new(D, dialog('Select object to edit')),
	    send(D, append, label(title, 'Click object to edit')),
	    length(Pairs, Len),
	    LBH is min(Len, 10),
	    send(D, append, new(LB, list_browser(@default, 30, LBH))),
	    fill_browser(Pairs, 1, LB),
	    send(LB, select_message, message(D, return, LB?selection?object)),
	    send(D, append,
		 new(C, button(cancel, message(D, destroy)))),
	    send(C, alignment, right),
	    send(D, resize_message, message(D, layout, @arg2)),
	    send(D, modal, transient),
	    send(D, transient_for, Frame),
	    (	get(D, confirm_centered, Frame?area?center, Rval)
	    ->	send(D, destroy),
		Location = Rval
	    ;	Location = []
	    )
	).

fill_browser([], _, _).
fill_browser([Location-Spec|T], N, LB) :-
	message_to_string(edit(target(Location-Spec, N)), Label),
	send(LB, append,
	     dict_item(Label, object := prolog(Location))),
	NN is N + 1,
	fill_browser(T, NN, LB).
