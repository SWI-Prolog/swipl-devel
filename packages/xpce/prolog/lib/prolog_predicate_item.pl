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

:- module(prolog_predicate_item, []).
:- use_module(library(pce)).
:- require([ atom_prefix/2
	   , current_functor/2
	   , atom_concat/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the XPCE  class   prolog_predicate_item,  which is a
subclass of class text_item for entering the names of prolog-predicates.

The item provides completion for modules and predicates in modules.  The
syntax used is:

	[<module> :] <name> [/ <arity>]

Completion is provided on all of the three fields.  It requests at least
one character before starting to avoid a too long waiting period.  Maybe
we should make this dynamic.

The  <-selection  is  returned   as   a    Prolog   term   of  the  form
Module:Name/Arity or Name/Arity.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(prolog_predicate_item, text_item,
		   "Item for entering a Prolog predicate").

class_variable(length,	int,	40, "Default # characters").

initialise(FI, Name:[name], Def:[char_array], Msg:[code]*) :->
	send_super(FI, initialise, Name, Def, Msg),
	send(FI, style, combo_box).


completions(FI, From:'tuple|name', Matches:chain) :<-
	(   send(From, instance_of, char_array)
	->  complete_module_or_predicate(From, Matches)
	;   get(From?first, value, ModulePlusColon),
	    atom_concat(Module, :, ModulePlusColon),
	    (	current_module(Module)
	    ->	new(Matches, chain),
		get(From?second, value, Pred),
		complete_predicate(Pred, Module, Matches),
		send(Matches, sort)
	    ;	send(FI, report, warning, 'Unknown module'),
		new(Matches, chain)
	    )
	).
	
		
split_completion(FI, Value:char_array, RVal:'tuple|char_array') :<-
	"Split into module part if present"::
	(   get(Value, index, :, Split)
	->  get(Value, sub, 0, Split+1, Module),
	    get(Value, sub, Split+1,  Rest),
	    new(RVal, tuple(Module, Rest))
	;   get(Value, size, Chars),
	    (	Chars >= 1
	    ->  RVal = Value
	    ;	send(FI, report, error,
		     'Please specify at least 1 character for completion'),
		fail
	    )
	).


indicate_directory(_FI, Text:string) :->
	"Indicate a module"::
	get(Text, value, Atom),
	(   current_module(Atom)
	->  send(Text, ensure_suffix, :)
	;   send(Text, ensure_suffix, /)
	).
	

complete_module_or_predicate(Prefix, Matches) :-
	new(Matches, chain),
	complete_module(Prefix, Matches),
	complete_predicate(Prefix, _, Matches),
	send(Matches, sort, @default, @on).

complete_module(Prefix, Matches) :-
	current_module(Module),
	atom_prefix(Module, Prefix),
	send(Matches, append, string('%s:', Module)),
	fail.
complete_module(_, _).
	
complete_predicate(Prefix, Module, Matches) :-
	get(Prefix, rindex, /, Idx), !,
	get(Prefix, sub, 0, Idx, Name),
	get(Prefix, sub, Idx+1, ArityPrefix),
	(   current_predicate(Name, Module:Head),
	    \+ predicate_property(Module:Head, imported_from(_)),
	    functor(Head, Name, Arity),
	    atom_prefix(Arity, ArityPrefix),
	    send(Matches, append, string('%s/%d', Name, Arity)),
	    fail
	;   true
	).
complete_predicate(Prefix, Module, Matches) :-
	var(Module), !,
	(   current_functor(Name, Arity),
	    atom_prefix(Name, Prefix),
	    functor(Head, Name, Arity),
	    current_predicate(Name, Module:Head),
	    \+ predicate_property(Module:Head, imported_from(_)),
	    send(Matches, append, string('%s/%d', Name, Arity)),
	    fail
	;   true
	).
complete_predicate(Prefix, Module, Matches) :-
	(   current_predicate(Name, Module:Head),
	    \+ predicate_property(Module:Head, imported_from(_)),
	    atom_prefix(Name, Prefix),
	    functor(Head, Name, Arity),
	    send(Matches, append, string('%s/%d', Name, Arity)),
	    fail
	;   true
	).


selection(TI, Sel:prolog) :<-
	"Get selection as Name/Arity or Module:Name/Arity"::
	get(TI, get_super, selection, Text),
	(   get(Text, scan, '%[^:]:%[^/]/%d', vector(Module0, Name0, Arity))
	;   get(Text, scan, '%[^:]:%s',	      vector(Module0, Name0))
	;   get(Text, scan, '%[^/]/%d',	      vector(Name0, Arity))
	;   get(Text, value, Name)
	), !,
	to_atom(Name0, Name),
	Name \== '',
	(   var(Module0)
	->  Sel = Name/Arity
	;   to_atom(Module0, Module),
	    Module \== ''
	->  Sel = Module:Name/Arity
	).

to_atom(X, A) :-
	object(X),
	send(X, instance_of, char_array),
	get(X, value, A).
to_atom(X, X).

:- pce_end_class.
