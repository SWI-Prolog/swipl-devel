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

:- module(prolog_predicate, []).
:- use_module(library(pce)).
:- require([ concat_atom/2
	   , term_to_atom/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class prolog_predicate represents the identity of a Prolog predicate. It
is used with predicate_item  for   locating  predicates and encapsulates
access to various parts of the development environment.
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
	;   Term = Name/Arity
	->  true
	;   Term = Module:Head,
	    callable(Head)
	->  functor(Head, Name, Arity)
	;   callable(Term)
	->  functor(Term, Name, Arity)
	),
	(   var(Arity)
	->  Arity = @default
	;   true
	),
	(   var(Module)
	->  Module = @nil
	;   true
	),
	send(P, slot, module, Module),
	send(P, slot, name, Name),
	send(P, slot, arity, Arity).

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

%	<-source:
%	
%	Get the source-location for this predicate. If not available and
%	the autoload argument is not @off, try to autoload the predicate
%	and try again.
%	
%	TBD: Deal with multiple solutions

source(P, Autoload:[bool], Loc:source_location) :<-
	"Return source-location from Prolog DB"::
	get(P, head, Head0),
	(   Head0 = _:_
	->  Head = Head0
	;   Head = _:Head0
	),
	(   predicate_property(Head, file(File))
	->  true
	;   Autoload \== @off,
	    send(P, autoload),
	    predicate_property(Head, file(File))
	),
	(   predicate_property(Head, line_count(Line))
	->  new(Loc, source_location(File, Line))
	;   new(Loc, source_location(File))
	).
	

autoload(P, Module:[name]) :->
	"Autoload the definition"::
	get(P, head, @off, Term),
	(   Module == @default
	->  '$define_predicate'(Term)
	;   '$define_predicate'(Module:Term)
	).

has_property(P, Prop:prolog) :->
	"Test predicate property"::
	get(P, head, Head),
	predicate_property(Head, Prop).

help(P) :->
	"Activate the help-system"::
	get(P, head, @off, Head),
	help(Head).

summary(P, Summary:name) :<-
	get(P, name, Name),
	get(P, arity, Arity),
	predicate(Name, Arity, Summary, _, _).

:- pce_end_class(prolog_predicate).
	  
