/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(modules,
	  [ in_temporary_module/3		% ?Module, :Setup, :Goal
	  ]).


/** <module> Module utility predicates
*/

:- meta_predicate
	in_temporary_module(?, 0, 0).

%%	in_temporary_module(?Module, :Setup, :Goal)
%
%	Run Goal on temporary loaded sources  and discard the module and
%	loaded predicates after completion.  This predicate performs the
%	following steps:
%
%	  1. If Module is unbound, create a unique identifier for it.
%	  2. Turn Module into a _temporary_ module using set_module/1.
%	     Note that this requires the module to be non-existent or
%	     empty.  If Module is specified, it should typically be set
%	     to a unique value as obtained from e.g. uuid/1.
%	  3. Run Setup in the context of Module.
%	  4. If setup succeeded possible choice points are discarded
%	     and Goal is started.
%
%	The  logical  result  of  this   predicate    is   the  same  as
%	`(Setup@Module -> Goal@Module)`, i.e., both   Setup and Goal are
%	resolved relative to the current  module,   but  executed in the
%	context of Module.  If  Goal  must   be  called  in  Module, use
%	`call(Goal)`.
%
%	The module and all  its  predicates   are  destroyed  after Goal
%	terminates, as defined by setup_call_cleanup/3.
%
%	*Discussion* This predicate is intended to   load programs in an
%	isolated   environment   and   reclaim   all   resources.   This
%	unfortunately is incomplete:
%
%	  - Running the code may leave side effects such as creating
%	    records, flags, changing Prolog flags, etc.  The system
%	    has no provisions to track this.
%	  - So called _functors_ (name/arity pairs) are not yet subject
%	    to garbage collection.  Functors are both used to define
%	    predicates and to create compound terms.
%
%	@see	library(sandbox) determines whether unknown goals are safe
%		to call.
%	@see	load_files/2 offers the option sandboxed(true) to load code
%		from unknown sources safely.

in_temporary_module(Module, Setup, Goal) :-
	(   var(Module)
	->  (   repeat,
	        I is random(1<<63),
		atom_concat('tmp-', I, Module),
		set_module(Module:class(temporary))
	    ->  true
	    )
	;   set_module(Module:class(temporary))
	),
	call_cleanup(
	    (   @(Setup, Module)
	    ->	@(Goal,  Module)
	    ),
	    '$destroy_module'(Module)).
