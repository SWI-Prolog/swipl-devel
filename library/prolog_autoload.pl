/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_autoload,
	  [ autoload/0,
	    autoload/1				% +Options
	  ]).
:- use_module(library(option)).

:- predicate_options(autoload/1, 1,
		     [ verbose(boolean)
		     ]).

/** <module> Autoload all dependencies

The autoloader is there to smoothen   program  development. It liberates
the programmer from finding the  library   that  defines some particular
predicate and including  the  proper   use_module/1,2  directive  in the
sources. This is even better at the toplevel, where just using maplist/3
is way more comfortable than  first   having  to load library(apply). In
addition, it reduces the startup time   of  applications by only loading
the necessary bits.

Of course, there is also a price. One   is  that it becomes less obvious
from where some predicate is loaded and  thus whether you have the right
definition.  The  second  issue  is  that  it  is  harder  to  create  a
stand-alone executable because this executable,   without  access to the
development system, can no longer rely  on autoloading. Finally, program
analysis becomes harder because the program may be incomplete.

This  library  provides  autoload/0  and   autoload/1  to  autoload  all
predicates that are referenced by the program. Now, this is not possible
in Prolog because the language allows   for constructing arbitrary goals
and runtime and calling them (e.g.,  read(X),   call(X)).

The classical version relies on the predicate_property =undefined=.
*/


%%	autoload is det.
%%	autoload(+Options) is det.
%
%	Force all necessary autoloading to be done _now_.  Options:
%
%	    * verbose(+Boolean)
%	    If =true=, report on the files loaded.

autoload :-
	autoload([]).

autoload(Options) :-
	option(verbose(Verbose), Options, true),
	setup_call_cleanup(
	    ( current_prolog_flag(access_level, OldLevel),
	      current_prolog_flag(autoload, OldAutoLoad),
	      set_prolog_flag(autoload, false),
	      set_prolog_flag(access_level, system)
	    ),
	    findall(Pred, needs_autoloading(Pred), Preds),
	    ( set_prolog_flag(autoload, OldAutoLoad),
	      set_prolog_flag(access_level, OldLevel)
	    )),
	(   Preds == []
	->  true
	;   setup_call_cleanup(
		( current_prolog_flag(autoload, OldAutoLoad),
		  current_prolog_flag(verbose_autoload, OldVerbose),
		  set_prolog_flag(autoload, true),
		  set_prolog_flag(verbose_autoload, Verbose)
		),
		defined_predicates(Preds),
		( set_prolog_flag(autoload, OldAutoLoad),
		  set_prolog_flag(verbose_autoload, OldVerbose)
		)),
	    autoload(Verbose)		% recurse for possible new
					% unresolved links
	).

defined_predicates([]).
defined_predicates([H|T]) :-
	'$define_predicate'(H),
	defined_predicates(T).

needs_autoloading(Module:Head) :-
	predicate_property(Module:Head, undefined),
	\+ predicate_property(Module:Head, imported_from(_)),
	functor(Head, Functor, Arity),
	'$in_library'(Functor, Arity, _).
