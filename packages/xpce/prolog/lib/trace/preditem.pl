/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_predicate_item,
	  [ prolog_predicates_from_selection/2	% +Sheet, -Preds
	  ]).

:- meta_predicate
	current_non_imported_predicate(?, :).

:- use_module(library(pce)).
:- require([ atom_prefix/2
	   , atom_concat/3
	   , current_functor/2
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

The <-selection is returned as a sheet  holding the entered module, name
and arity. The predicate prolog_predicates_from_selection/2  may be used
to expand this value to a list of terms of the form Module:Head.

See the commented test/0 the end of this file for an example.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(prolog_predicate_item, text_item,
		   "Item for entering a Prolog predicate").

class_variable(length,	int,	40, "Default # characters").

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
	send(Matches, sort).

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


selection(TI, Sel:sheet) :<-
	"Get selection as a sheet holding module, name and arity"::
	get(TI, get_super, selection, Text),
	(   get(Text, scan, '%[^:]:%[^/]/%d', vector(Module, Name, Arity))
	;   get(Text, scan, '%[^:]:%s',	      vector(Module, Name))
	;   get(Text, scan, '%[^/]/%d',	      vector(Name, Arity))
	;   get(Text, value, Name)
	), !,
	new(Sel, sheet),
	set_attribute(Sel, module, Module),
	set_attribute(Sel, name,   Name),
	set_attribute(Sel, arity,  Arity).

set_attribute(_, _, Val) :- var(Val), !.
set_attribute(Sheet, Name, Value) :-
	send(Sheet, value, Name, Value).

get_attribute(Sheet, Name, Value) :-
	get(Sheet, value, Name, RawValue), !,
	(   object(RawValue)
	->  get(RawValue, value, Value)
	;   Value = RawValue
	).
get_attribute(_, _, _).

prolog_predicates_from_selection(Sheet, Preds) :-
	get_attribute(Sheet, module, Module),
	get_attribute(Sheet, name,   Name),
	get_attribute(Sheet, arity,  Arity),
	P = Module:Head,
	(   var(Arity)
	->  findall(P, current_non_imported_predicate(Name, P), Preds)
	;   functor(Head, Name, Arity),
	    findall(P, current_non_imported_predicate(Name, P), Preds)
	).

current_non_imported_predicate(Name, P) :-
	current_predicate(Name, P),
	(   predicate_property(P, imported_from(_))
	->  predicate_property(P, built_in)
	;   true
	).

:- pce_end_class.
