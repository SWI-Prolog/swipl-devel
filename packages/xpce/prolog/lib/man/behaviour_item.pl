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

:- module(pce_behaviour_item, []).
:- use_module(library(pce)).
:- use_module(util).
:- require([ forall/2
	   ]).

:- pce_begin_class(behaviour_item, text_item,
		   "Text item for entering XPCE behaviour or class").

initialise(FI, Name:[name], Def:[any|function], Msg:[code]*) :->
	send(FI, send_super, initialise, Name, Def, Msg),
	send(FI, style, combo_box).

completions(_MI, Spec:'char_array|tuple', Matches:chain) :<-
	new(Matches, chain),
	(   send(Spec, instance_of, char_array)
	->  send(@classes, for_all,
		 if(message(@arg1, prefix, Spec),
		    message(Matches, append, @arg1)))
	;   get(Spec, first, ClassPart),
	    get(Spec, second, SelPart),
	    new(Re, regex('\\s *\\(\\w+\\)\\s *\\(->\\|<-\\|-\\)')),
	    send(Re, match, ClassPart),
	    get(Re, register_value, ClassPart, 1, name, ClassName),
	    get(Re, register_value, ClassPart, 2, name, Arrow),
	    get(@pce, convert, ClassName, class, Class),
	    new(Code, if(message(@arg1?name, prefix, SelPart),
			 message(Matches, append, @arg1?name))),
	    (	Arrow == (-)
	    ->	send(Class?instance_variables, for_all, Code)
	    ;	Arrow == (->)
	    ->	forall(super_or_delegate_class(Class, TheClass),
		       (send(TheClass?send_methods, for_all, Code),
			send(TheClass?instance_variables, for_all,
			     if(message(@arg1, send_access), Code))))
	    ;	forall(super_or_delegate_class(Class, TheClass),
		       (send(TheClass?get_methods, for_all, Code),
			send(TheClass?instance_variables, for_all,
			     if(message(@arg1, get_access), Code))))
	    )
	),
	send(Matches, unique),
	send(Matches, sort).

split_completion(_MI, Value:char_array, RVal:'char_array|tuple') :<-
	new(Re, regex('\\s *\\(\\w+\\s *\\(->\\|<-\\|-\\)\\)\\(\\w*\\)')),
	(   send(Re, match, Value)
	->  get(Re, register_value, Value, 1, Class),
	    get(Re, register_value, Value, 3, Selector),
	    new(RVal, tuple(Class, Selector))
	;   RVal = Value
	).


selected_completion(MI, Selected:char_array, _Apply:[bool]) :->
	send(MI, send_super, selected_completion, Selected, @off),
	(   get(MI, selection, Selection),
	    send(Selection, instance_of, behaviour)
	->  send(MI, apply, @on)
	;   true
	).


selection(MI, S:'behaviour|class*') :<-
	"Get selection as behaviour or class"::
	get(MI, get_super, selection, Text),
	(   send(type('behaviour|class*'), validate, Text)
	->  S = Text
	;   get(Text, size, 0)
	->  S = @nil
	;   new(Re, regex('\\s *\\(\\w+\\)\\s *\\(->\\|<-\\|-\\)\\s *\\(\\w+\\)')),
	    (   (   send(Re, match, Text)
		->  get(Re, register_value, Text, 1, class, Class),
		    get(Re, register_value, Text, 2, name, Access),
		    get(Re, register_value, Text, 3, name, Selector),
		    super_or_delegate_class(Class, TheClass),
		    (	Access == (-)
		    ->	get(TheClass, instance_variable, Selector, S)
		    ;	Access == (->)
		    ->	get(TheClass, send_method, Selector, S)
		    ;	get(TheClass, get_method, Selector, S)
		    )
		;   get(@pce, convert, Text, class, S)
		)
	    ;   send(MI, error, cannot_convert_text, Text, 'behaviour|class'),
		fail
	    )
	).


:- pce_end_class.
