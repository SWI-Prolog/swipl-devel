/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(prolog_predicate, []).
:- use_module(library(pce)).
:- require([ concat_atom/2
	   , term_to_atom/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class prolog_predicate is  a  rather  simple   class  to  represent  the
identity of a Prolog  predicate.  It   is  used  with predicate_item for
locating predicates and an important reason   for  the existence of this
class is to have the type prolog_predicate available.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(prolog_predicate, object,
		   "Represent a Prolog predicate").

variable(module,	name*,	 get, "Module of the predicate").
variable(name,		name,	 get, "Name of predicate").
variable(arity,		['0..'], get, "Arity of the predicate").

initialise(P, Term:prolog) :->
	"Create from [Module]:Name/Arity"::
	(   Term = Module:Name/Arity
	->  true
	;   Term = Name/Arity,
	    Module = @nil
	),
	(   var(Arity)
	->  TheArity = @default
	;   TheArity = Arity
	),
	send(P, slot, module, Module),
	send(P, slot, name, Name),
	send(P, slot, arity, TheArity).

convert(_, From:name, P:prolog_predicate) :<-
	"Convert textual and Prolog term"::
	term_to_atom(From, Term),
	(   (   Term = Module:Name/Arity
	    ;	Term = Name/Arity
	    )
	->  new(P, prolog_predicate(Term))
	;   Term = Module:Head,
	    callable(Head)
	->  functor(Head, Name, Arity),
	    new(P, prolog_predicate(Module:Name/Arity))
	;   callable(Head)
	->  new(P, prolog_predicate(Name/Arity))
	).

print_name(P, PN:name) :<-
	"Return as [Module:]Name/Arity"::
	get(P, name, Name),
	get(P, arity, Arity),
	(   Arity == @default
	->  TheArity = '_'
	;   TheArity = Arity
	),
	(   get(P, module, Module), Module \== @nil
	->  concat_atom([Module, :, Name, /, TheArity], PN)
	;   concat_atom([Name, /, TheArity], PN)
	).
	
head(P, Qualify:[bool], Head:prolog) :<-
	"Get a head-term"::
	get(P, module, Module),
	get(P, name, Name),
	get(P, arity, Arity),
	Arity \== @default,
	functor(Head0, Name, Arity),
	(   (   Qualify == @off
	    ;	Qualify == @default,
		Module == @nil
	    )
	->  Head = Head0
	;   Module \== @nil
	->  Head = Module:Head0
	;   Head = user:Head0
	).

%	TODO: Deal with multiple solutions

source(P, Loc:source_location) :<-
	"Return source-location from Prolog DB"::
	get(P, head, Head0),
	(   Head0 = _:_
	->  Head = Head0
	;   Head = _:Head0
	),
	predicate_property(Head, file(File)),
	(   predicate_property(Head, line_count(Line))
	->  new(Loc, source_location(File, Line))
	;   new(Loc, source_location(File))
	).
	
:- pce_end_class(prolog_predicate).
	  
