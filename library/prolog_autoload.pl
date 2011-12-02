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
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

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
	autoloaded_count/1,
	multifile_predicate/3.		% Name, Arity, Module

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
		 scan_module(M, Options)
	       ),
	       find_undefined_from_module(M, Options)),
	undefined_from_multifile(Options),
	undefined_from_initialization(Options).

scan_module(M, _) :-
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
	forall('$init_goal'(File, Goal, SourceLocation),
	       undefined_from_initialization(File, Goal,
					     [ initialization(SourceLocation)
					     | Options
					     ])).

undefined_from_initialization(File, Goal, Options) :-
	module_property(Module, file(File)),
	(   scan_module(Module, Options)
	->  true
	;   undefined_called(Goal, Module, _, Options)
	).
undefined_from_initialization(_, Goal, Options) :-
	undefined_called(Goal, user, _, Options).


%%	find_undefined_from_module(+Module, +Options) is det.
%
%	Find undefined calls from the bodies  of all clauses that belong
%	to Module.

find_undefined_from_module(M, Options) :-
	debug(autoload, 'Analysing module ~q', [M]),
	forall(predicate_in_module(M, PI),
	       undefined_called_by_pred(M:PI, Options)).

undefined_called_by_pred(Module:Name/Arity, _) :-
	multifile_predicate(Name, Arity, Module), !.
undefined_called_by_pred(Module:Name/Arity, _) :-
	functor(Head, Name, Arity),
	predicate_property(Module:Head, multifile), !,
	assertz(multifile_predicate(Name, Arity, Module)).
undefined_called_by_pred(Module:Name/Arity, Options) :-
	functor(Head, Name, Arity),
	forall(catch(clause(Module:Head, Body, ClauseRef), _, fail),
	       undefined_called_by_body(Body, Module,
					[ clause(ClauseRef)
					| Options
					])).

%%	undefined_from_multifile(+Options)
%
%	Process registered multifile predicates.

undefined_from_multifile(Options) :-
	forall(retract(multifile_predicate(Name, Arity, Module)),
	       undefined_called_by_multifile(Module:Name/Arity, Options)).

undefined_called_by_multifile(Module:Name/Arity, Options) :-
	functor(Head, Name, Arity),
	forall(catch(clause_not_from_development(
			 Module:Head, Body, ClauseRef, Options),
		     _, fail),
	       undefined_called_by_body(Body, Module,
					[ clause(ClauseRef)
					| Options
					])).


%%	clause_not_from_development(:Head, -Body, ?Ref, +Options) is nondet.
%
%	Enumerate clauses for a multifile predicate, but omit those from
%	a module that is specifically meant to support development.

clause_not_from_development(Module:Head, Body, Ref, Options) :-
	clause(Module:Head, Body, Ref),
	\+ ( clause_property(Ref, file(File)),
	     module_property(LoadModule, file(File)),
	     \+ scan_module(LoadModule, Options)
	   ).

%%	undefined_called_by_body(+Body, +Module, +Options) is det.
%
%	Check the Body term when  executed   in  the  context of Module.
%	Options:
%
%	  - undefined(+Action)
%	  One of =ignore=, =error=

undefined_called_by_body(True, _, _) :-
	True == true, !.		% quickly deal with facts
undefined_called_by_body(Body, Module, Options) :-
	catch(undefined_called(
		  Body, Module, _TermPos,
		  [ undecided(error),
		    evaluate(false)
		  | Options
		  ]),
	      missing(Missing),
	      undefined_called_by_body(Missing, Body, Module, Options)), !.
undefined_called_by_body(Body, _Module, _Options) :-
	format(user_error, 'Failed to analyse:~n'),
	portray_clause(('<head>' :- Body)),
	(   debugging(autoload(trace))
	->  gtrace
	;   true
	).

%%	undefined_called_by_body(+Missing, +Body, +Module, +Options)
%
%	Restart the analysis because  the   previous  analysis  provided
%	insufficient information.

undefined_called_by_body(Missing, Body, _, Options) :-
	debugging(autoload),
	format(user_error, 'Retrying due to ~w (~p)~n', [Missing, Options]),
	portray_clause(('<head>' :- Body)), fail.
undefined_called_by_body(undecided_call, Body, Module, Options) :-
	catch(forall(undefined_called(Body, Module, _TermPos, Options),
		     true),
	      missing(Missing),
	      undefined_called_by_body(Missing, Body, Module, Options)).
undefined_called_by_body(subterm_positions, Body, Module, Options) :-
	option(clause(ClauseRef), Options),
	(   clause_info(ClauseRef, _File, TermPos, _NameOffset),
	    TermPos = term_position(_,_,_,_,[_,BodyPos])
	->  forall(undefined_called(Body, Module, BodyPos, Options),
		   true)
	;   forall(undefined_called(Body, Module, BodyPos,
				    [source(false)|Options]),
		   true)
	).


%%	undefined_called(+Goal, +Module, +TermPos, +Options) is multi.
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
%	Options:
%
%	  * undecided(+Action)
%	  How to deal with insifficiently instantiated terms in the
%	  call-tree.  Values are:
%
%	    - ignore
%	    Silently ignore such goals
%	    - error
%	    Throw =undecided_call=
%
%	  * evaluate(+Boolean)
%	  If =true= (default), evaluate some goals.  Notably =/2.
%
%	@tbd	Analyse e.g. assert((Head:-Body))?

undefined_called(Var, _, TermPos, Options) :-
	var(Var), !,				% Incomplete analysis
	undecided(Var, TermPos, Options).
undefined_called(M:G, _, term_position(_,_,_,_,[MPos,Pos]), Options) :- !,
	(   nonvar(M)
	->  undefined_called(G, M, Pos, Options)
	;   undecided(M, MPos, Options)
	).
undefined_called((A,B), M, term_position(_,_,_,_,[PA,PB]), Options) :- !,
	undefined_called(A, M, PA, Options),
	undefined_called(B, M, PB, Options).
undefined_called((A;B), M, term_position(_,_,_,_,[PA,PB]), Options) :- !,
	(   option(evaluate(true), Options, true)
	->  Goal = (A;B),
	    setof(Goal,
		  (   undefined_called(A, M, PA, Options)
		  ;   undefined_called(B, M, PB, Options)
		  ),
		  Alts0),
	    variants(Alts0, Alts),
	    member(Goal, Alts)
	;   undefined_called(A, M, PA, Options),
	    undefined_called(B, M, PB, Options)
	).
undefined_called(Goal, Module, _, Options) :-
	evaluate(Goal, Module, Options), !.
undefined_called(Goal, M, TermPos, Options) :-
	prolog:called_by(Goal, Called),
	Called \== [], !,
	undefined_called_by(Called, M, TermPos, Options).
undefined_called(Meta, M, term_position(_,_,_,_,ArgPosList), Options) :-
	predicate_property(M:Meta, meta_predicate(Head)), !,
	undef_called_meta(1, Head, Meta, M, ArgPosList, Options).
undefined_called(Goal, Module, _, _) :-
	nonvar(Module),
	'$get_predicate_attribute'(Module:Goal, defined, 1), !.
undefined_called(Goal, Module, TermPos, Options) :-
	undefined(Module:Goal, TermPos, Options).

%%	undecided(+Variable, +TermPos, +Options)

undecided(Var, TermPos, Options) :-
	option(undecided(Action), Options, ignore),
	undecided(Action, Var, TermPos, Options).

undecided(ignore, _, _, _) :- !.
undecided(error,  _, _, _) :-
	throw(missing(undecided_call)).

%%	evaluate(Goal, Module, Options) is nondet.

evaluate(Goal, Module, Options) :-
	option(evaluate(true), Options, true),
	evaluate(Goal, Module).

evaluate(A=B, _) :-
	unify_with_occurs_check(A, B).

%%	undefined(:Goal, +TermPos, +Options)
%
%	The analysis trapped a definitely undefined predicate.

undefined(_, _, Options) :-
	option(undefined(ignore), Options, ignore), !.
undefined(Goal, TermPos, Options) :-
	option(clause(Clause), Options), !,
	goal_pi(Goal, PI),
	(   compound(TermPos),
	    arg(1, TermPos, CharCount),
	    integer(CharCount)
	->  clause_property(Clause, file(File)),
	    print_message(error, error(existence_error(procedure, PI),
				       file_char_count(File, CharCount)))
	;   option(source(false), Options)
	->  print_message(error, error(existence_error(procedure, PI),
				       clause(Clause)))
	;   throw(missing(subterm_positions))
	).
undefined(Goal, _, Options) :-
	option(initialization(File:Line), Options), !,
	goal_pi(Goal, PI),
	print_message(error, error(existence_error(procedure, PI),
				   file(File, Line, -1, _))).
undefined(Goal, _, _) :-
	goal_pi(Goal, PI),
	print_message(error, error(existence_error(procedure, PI), _)).

goal_pi(Goal, M:Name/Arity) :-
	strip_module(Goal, M, Head),
	callable(Head), !,
	functor(Head, Name, Arity).
goal_pi(Goal, Goal).


%%	undef_called_meta(+Index, +GoalHead, +MetaHead, +Module,
%%			  +ArgPosList, +Options)

undef_called_meta(I, Head, Meta, M, [ArgPos|ArgPosList], Options) :-
	arg(I, Head, AS), !,
	(   integer(AS)
	->  arg(I, Meta, MA),
	    extend(MA, AS, Goal, ArgPos, ArgPosEx, Options),
	    undefined_called(Goal, M, ArgPosEx, Options)
	;   true
	),
	succ(I, I2),
	undef_called_meta(I2, Head, Meta, M, ArgPosList, Options).
undef_called_meta(_, _, _, _, _, _).


%%	undefined_called_by(+Called:list, +Module, +TermPost, +Options)

undefined_called_by([], _, _, _).
undefined_called_by([H|T], M, TermPos, Options) :-
	(   H = G+N
	->  (   extend(G, N, G2, _, _, Options)
	    ->	undefined_called(G2, M, _, Options)
	    ;	true
	    )
	;   undefined_called(H, M, TermPos, Options)
	),
	undefined_called_by(T, M, TermPos, Options).

%%	extend(+Goal, +ExtraArgs, +TermPosIn, -TermPosOut, +Options)
%
%	@bug:

extend(Goal, 0, Goal, TermPos, TermPos, _) :- !.
extend(Goal, _, _, TermPos, TermPos, Options) :-
	var(Goal), !,
	undecided(Goal, TermPos, Options).
extend(M:Goal, N, M:GoalEx,
       term_position(F,T,FT,TT,[MPos,GPosIn]),
       term_position(F,T,FT,TT,[MPos,GPosOut]), Options) :- !,
	(   var(M)
	->  undecided(N, MPos, Options)
	;   true
	),
	extend(Goal, N, GoalEx, GPosIn, GPosOut, Options).
extend(Goal, N, GoalEx, TermPosIn, TermPosOut, _) :-
	callable(Goal),
	Goal =.. List,
	length(Extra, N),
	extend_term_pos(TermPosIn, N, TermPosOut),
	append(List, Extra, ListEx),
	GoalEx =.. ListEx.

extend_term_pos(Var, _, _) :-
	var(Var), !.
extend_term_pos(term_position(F,T,FT,TT,ArgPosIn),
		N,
		term_position(F,T,FT,TT,ArgPosOut)) :- !,
	length(Extra, N),
	maplist(=(0-0), Extra),
	append(ArgPosIn, Extra, ArgPosOut).
extend_term_pos(F-T, N, term_position(F,T,F,T,Extra)) :-
	length(Extra, N),
	maplist(=(0-0), Extra).


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

:- multifile
	prolog:message//1,
	prolog:message_location//1.

prolog:message(autoload(reiterate(Iteration, NewFiles, NewPreds, Time))) -->
	[ 'Autoloader: iteration ~D resolved ~D predicates and loaded ~D files in ~3f seconds. \c
	  Restarting ...'-[Iteration, NewFiles, NewPreds, Time]
	].
prolog:message(autoload(completed(Iterations, Time, NewFiles))) -->
	[ 'Autoloader: loaded ~D files in ~D iterations in ~3f seconds'-
	  [NewFiles, Iterations, Time] ].

prolog:message_location(file_char_count(File, CharCount)) -->
	{ filepos_line(File, CharCount, Line, LinePos) },
	[ '~w:~d:~d: '-[File, Line, LinePos] ].
prolog:message_location(clause(ClauseRef)) -->
	{ clause_property(ClauseRef, file(File)),
	  clause_property(ClauseRef, line_count(Line))
	}, !,
	[ '~w:~d: '-[File, Line] ].
prolog:message_location(clause(ClauseRef)) -->
	{ clause_name(ClauseRef, Name) },
	[ '~w: '-[Name] ].


filepos_line(File, CharPos, Line, LinePos) :-
	setup_call_cleanup(
	    ( open(File, read, In),
	      open_null_stream(Out)
	    ),
	    ( Skip is CharPos-1,
	      copy_stream_data(In, Out, Skip),
	      stream_property(In, position(Pos)),
	      stream_position_data(line_count, Pos, Line),
	      stream_position_data(line_position, Pos, LinePos)
	    ),
	    ( close(Out),
	      close(In)
	    )).
