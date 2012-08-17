/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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
:- use_module(library(error)).
:- use_module(library(aggregate)).
:- use_module(library(prolog_codewalk)).

:- predicate_options(autoload/1, 1,
		     [ verbose(boolean),
		       undefined(oneof([ignore,error]))
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

The classical version relied on  the predicate_property =undefined=. The
current version relies on code analysis of the bodies of all clauses and
all initialization goals.
*/

:- thread_local
	autoloaded_count/1.

%%	autoload is det.
%%	autoload(+Options) is det.
%
%	Force all necessary autoloading to be done _now_.  Options:
%
%	    * verbose(+Boolean)
%	    If =true=, report on the files loaded.
%	    * undefined(+Action)
%	    Action defines what happens if the analysis finds a
%	    definitely undefined predicate.  One of =ignore= or
%	    =error=.

autoload :-
	autoload([]).

autoload(Options) :-
	must_be(list, Options),
	statistics(cputime, T0),
	aggregate_all(count, source_file(_), OldFileCount),
	autoload(0, Iterations, Options),
	aggregate_all(count, source_file(_), NewFileCount),
	statistics(cputime, T1),
	Time is T1-T0,
	information_level(Level, Options),
	NewFiles is NewFileCount - OldFileCount,
	print_message(Level, autoload(completed(Iterations, Time, NewFiles))).


autoload(Iteration0, Iterations, Options) :-
	statistics(cputime, T0),
	autoload_step(NewFiles, NewPreds, Options),
	statistics(cputime, T1),
	Time is T1-T0,
	succ(Iteration0, Iteration),
	(   NewFiles > 0
	->  information_level(Level, Options),
	    print_message(Level, autoload(reiterate(Iteration,
						    NewFiles, NewPreds, Time))),
	    autoload(Iteration, Iterations, Options)
	;   Iterations = Iteration
	).

information_level(Level, Options) :-
	(   option(verbose(true), Options, true)
	->  Level = informational
	;   Level = silent
	).

%%	autoload_step(-NewFiles, -NewPreds, +Options) is det.
%
%	Scan through the program and   autoload all undefined referenced
%	predicates.
%
%	@param NewFiles is unified to the number of files loaded
%	@param NewPreds is unified to the number of predicates imported
%	       using the autoloader.

autoload_step(NewFiles, NewPreds, Options) :-
	option(verbose(Verbose), Options, true),
	aggregate_all(count, source_file(_), OldFileCount),
	setup_call_cleanup(
	    ( current_prolog_flag(autoload, OldAutoLoad),
	      current_prolog_flag(verbose_autoload, OldVerbose),
	      set_prolog_flag(autoload, true),
	      set_prolog_flag(verbose_autoload, Verbose),
	      assert_autoload_hook(Ref),
	      asserta(autoloaded_count(0))
	    ),
	    prolog_walk_code(Options),
	    ( retract(autoloaded_count(Count)),
	      erase(Ref),
	      set_prolog_flag(autoload, OldAutoLoad),
	      set_prolog_flag(verbose_autoload, OldVerbose)
	    )),
	aggregate_all(count, source_file(_), NewFileCount),
	NewPreds = Count,
	NewFiles is NewFileCount - OldFileCount.

assert_autoload_hook(Ref) :-
	asserta((user:message_hook(autoload(Module:Name/Arity, Library), _, _) :-
			autoloaded(Module:Name/Arity, Library)), Ref).

:- public
	autoloaded/2.

autoloaded(_, _) :-
	retract(autoloaded_count(N)),
	succ(N, N2),
	asserta(autoloaded_count(N2)),
	fail.					% proceed with other hooks


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(autoload(reiterate(Iteration, NewFiles, NewPreds, Time))) -->
	[ 'Autoloader: iteration ~D resolved ~D predicates \c
	  and loaded ~D files in ~3f seconds.  Restarting ...'-
	  [Iteration, NewFiles, NewPreds, Time]
	].
prolog:message(autoload(completed(Iterations, Time, NewFiles))) -->
	[ 'Autoloader: loaded ~D files in ~D iterations in ~3f seconds'-
	  [NewFiles, Iterations, Time] ].
