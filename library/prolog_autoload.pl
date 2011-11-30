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
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

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

The classical version relied on  the predicate_property =undefined=. The
current version relies on code analysis of the bodies of all clauses and
all initialization goals.
*/

:- thread_local
	autoloaded_count/1,
	multifile_predicate/3.		% Name, Arity, Module

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
	    find_undefined(Options),
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


find_undefined(Options) :-
	forall(( current_module(M),
		 scan_module(M)
	       ),
	       find_undefined_from_module(M, Options)),
	undefined_from_multifile(Options),
	undefined_from_initialization(Options).

scan_module(M) :-
	module_property(M, class(Class)),
	scan_module_class(Class).

scan_module_class(user).
scan_module_class(library).


%%	undefined_from_initialization(+Options)
%
%	Find initialization/1,2 directives and  process   what  they are
%	calling.  Skip
%
%	@bug	Relies on private '$init_goal'/3 database.

undefined_from_initialization(Options) :-
	forall('$init_goal'(File, Goal, _SourceLocation),
	       undefined_from_initialization(File, Goal, Options)).

undefined_from_initialization(File, Goal, _Options) :-
	module_property(Module, file(File)),
	(   scan_module(Module)
	->  true
	;   undefined_called(Goal, Module)
	).
undefined_from_initialization(_, Goal, _Options) :-
	undefined_called(Goal, user).


%%	find_undefined_from_module(+Module) is det.
%
%	Find undefined calls from the bodies  of all clauses that belong
%	to Module.

find_undefined_from_module(M, _Options) :-
	debug(autoload, 'Analysing module ~q', [M]),
	forall(predicate_in_module(M, PI),
	       undefined_called_by_pred(M:PI)).

undefined_called_by_pred(Module:Name/Arity) :-
	multifile_predicate(Name, Arity, Module), !.
undefined_called_by_pred(Module:Name/Arity) :-
	functor(Head, Name, Arity),
	predicate_property(Module:Head, multifile), !,
	assertz(multifile_predicate(Name, Arity, Module)).
undefined_called_by_pred(Module:Name/Arity) :-
	functor(Head, Name, Arity),
	forall(catch(clause(Module:Head, Body), _, fail),
	       undefined_called_by_body(Body, Module)).

%%	undefined_from_multifile(+Options)
%
%	Process registered multifile predicates.

undefined_from_multifile(_Options) :-
	forall(retract(multifile_predicate(Name, Arity, Module)),
	       undefined_called_by_multifile(Module:Name/Arity)).

undefined_called_by_multifile(Module:Name/Arity) :-
	functor(Head, Name, Arity),
	forall(catch(clause_not_from_development(Module:Head, Body), _, fail),
	       undefined_called_by_body(Body, Module)).


%%	clause_not_from_development(:Head, -Body) is nondet.
%
%	Enumerate clauses for a multifile predicate, but omit those from
%	a module that is specifically meant to support development.

clause_not_from_development(Module:Head, Body) :-
	clause(Module:Head, Body, Ref),
	\+ ( clause_property(Ref, file(File)),
	     module_property(LoadModule, file(File)),
	     \+ scan_module(LoadModule)
	   ).

undefined_called_by_body(Body, Module) :-
	forall(undefined_called(Body, Module),
	       true).

%%	undefined_called(+Goal, +Module) is multi.
%
%	Perform abstract interpretation of Goal,  touching all sub-goals
%	that  are  directly  called  or  immediately  reachable  through
%	meta-calls.  The  actual  auto-loading  is    performed  by  the
%	predicate_property/2 call for meta-predicates.
%
%	If  Goal  is  disjunctive,  undefined_called   succeeds  with  a
%	choice-point.  Backtracking  analyses  the  alternative  control
%	path(s).
%
%	@tbd	Analyse e.g. assert((Head:-Body))?

undefined_called(Var, _) :-
	var(Var), !.				% Incomplete analysis
undefined_called(true,_) :- !.			% Common for facts
undefined_called(M:G, _) :- !,
	undefined_called(G, M).
undefined_called((A,B), M) :- !,
	undefined_called(A, M),
	undefined_called(B, M).
undefined_called((A;B), M) :- !,
	Goal = (A;B),
	setof(Goal,
	      (   undefined_called(A, M)
	      ;   undefined_called(B, M)
	      ),
	      Alts0),
	variants(Alts0, Alts),
	member(Goal, Alts).
undefined_called(A=B, _) :-
	unify_with_occurs_check(A,B), !.
undefined_called(Goal, M) :-
	prolog:called_by(Goal, Called),
	Called \== [], !,
	undefined_called_by(Called, M).
undefined_called(Meta, M) :-
	predicate_property(M:Meta, meta_predicate(Head)), !,
	undef_called_meta(1, Head, Meta, M).
undefined_called(_, _).

%%	undef_called_meta(+Index, +GoalHead, +MetaHead, +Module)

undef_called_meta(I, Head, Meta, M) :-
	arg(I, Head, AS), !,
	(   integer(AS)
	->  arg(I, Meta, MA),
	    extend(MA, AS, Goal),
	    undefined_called(Goal, M)
	;   true
	),
	succ(I, I2),
	undef_called_meta(I2, Head, Meta, M).
undef_called_meta(_, _, _, _).


%%	undefined_called_by(+Called:list, +Module)

undefined_called_by([], _).
undefined_called_by([H|T], M) :-
	(   H = G+N
	->  (   extend(G, N, G2)
	    ->	undefined_called(G2, M)
	    ;	true
	    )
	;   undefined_called(H, M)
	),
	undefined_called_by(T, M).

extend(Goal, 0, Goal) :- !.
extend(Goal, N, GoalEx) :-
	callable(Goal),
	Goal =.. List,
	length(Extra, N),
	append(List, Extra, ListEx),
	GoalEx =.. ListEx.

%%	variants(+SortedList, -Variants) is det.

variants([], []).
variants([H|T], List) :-
	variants(T, H, List).

variants([], H, [H]).
variants([H|T], V, List) :-
	(   H =@= V
	->  variants(T, V, List)
	;   List = [V|List2],
	    variants(T, H, List2)
	).

%%	predicate_in_module(+Module, ?PI) is nondet.
%
%	True if PI is a predicate locally defined in Module.

predicate_in_module(Module, PI) :-
	current_predicate(Module:PI),
	PI = Name/Arity,
	functor(Head, Name, Arity),
	\+ predicate_property(Module:Head, imported_from(_)).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(autoload(reiterate(Iteration, NewFiles, NewPreds, Time))) -->
	[ 'Autoloader: iteration ~D resolved ~D predicates and loaded ~D files in ~3f seconds. \c
	  Restarting ...'-[Iteration, NewFiles, NewPreds, Time]
	].
prolog:message(autoload(completed(Iterations, Time, NewFiles))) -->
	[ 'Autoloader: loaded ~D files in ~D iterations in ~3f seconds'-
	  [NewFiles, Iterations, Time] ].
