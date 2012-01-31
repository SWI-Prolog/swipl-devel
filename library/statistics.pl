/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(prolog_statistics,
	  [ statistics/0,
	    time/1,			% :Goal
	    profile/1,			% :Goal
	    profile/3,			% :Goal, +Style, +Top
	    show_profile/2,		% +Style, +Top
	    show_profile/1		% +Top
	  ]).
:- use_module(library(lists)).

:- meta_predicate
	time(0),
	profile(0),
	profile(0, +, +).

/** <module> Get information about resource usage

This library provides predicates to   obtain  information about resource
usage by your program. The predicates of  this library are for human use
at the toplevel: information is _printed_.   All predicates obtain their
information using public low-level primitives.   These primitives can be
use to obtain selective statistics during execution.
*/

%%	statistics is det.
%
%	Print information about  resource  usage   to  the  =user_error=
%	stream.
%
%	@see	All statistics printed are obtained through statistics/2.

statistics :-
	statistics(user_error).

statistics(Out) :-
	statistics(trail, Trail),
	statistics(trailused, TrailUsed),
	statistics(local, Local),
	statistics(localused, LocalUsed),
	statistics(global, Global),
	statistics(globalused, GlobalUsed),
	statistics(process_cputime, Cputime),
	statistics(inferences, Inferences),
	statistics(atoms, Atoms),
	statistics(functors, Functors),
	statistics(predicates, Predicates),
	statistics(modules, Modules),
	statistics(codes, Codes),
	statistics(locallimit, LocalLimit),
	statistics(globallimit, GlobalLimit),
	statistics(traillimit, TrailLimit),

	format(Out, '~3f seconds cpu time for ~D inferences~n',
				    [Cputime, Inferences]),
	format(Out, '~D atoms, ~D functors, ~D predicates, ~D modules, ~D VM-codes~n~n',
				    [Atoms, Functors, Predicates, Modules, Codes]),
	format(Out, '                       Limit    Allocated       In use~n', []),
	format(Out, 'Local  stack :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n',
	       [LocalLimit, Local, LocalUsed]),
	format(Out, 'Global stack :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n',
	       [GlobalLimit, Global, GlobalUsed]),
	format(Out, 'Trail  stack :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n~n',
	       [TrailLimit, Trail, TrailUsed]),

	gc_statistics(Out),
	agc_statistics(Out),
	shift_statistics(Out),
	thread_statistics(Out).

gc_statistics(Out) :-
	statistics(collections, Collections),
	Collections > 0, !,
	statistics(collected, Collected),
	statistics(gctime, GcTime),

	format(Out, '~D garbage collections gained ~D bytes in ~3f seconds.~n',
	       [Collections, Collected, GcTime]).
gc_statistics(_).

agc_statistics(Out) :-
	catch(statistics(agc, Agc), _, fail),
	Agc > 0, !,
	statistics(agc_gained, Gained),
	statistics(agc_time, Time),
	format(Out, '~D atom garbage collections gained ~D atoms in ~3f seconds.~n',
	       [Agc, Gained, Time]).
agc_statistics(_).

shift_statistics(Out) :-
	statistics(local_shifts, LS),
	statistics(global_shifts, GS),
	statistics(trail_shifts, TS),
	(   LS > 0
	;   GS > 0
	;   TS > 0
	), !,
	statistics(shift_time, Time),
	format(Out, 'Stack shifts: ~D local, ~D global, ~D trail in ~3f seconds.~n',
	       [LS, GS, TS, Time]).
shift_statistics(_).

thread_statistics(Out) :-
	current_prolog_flag(threads, true), !,
	statistics(threads, Active),
	statistics(threads_created, Created),
	statistics(thread_cputime, CpuTime),
	Finished is Created - Active,
	format(Out, '~D threads, ~D finished threads used ~3f seconds.~n',
	       [Active, Finished, CpuTime]).
thread_statistics(_).


%%	time(:Goal) is nondet.
%
%	Execute Goal, reporting statistics to the user. If Goal succeeds
%	non-deterministically,  retrying  reports  the   statistics  for
%	providing the next answer.
%
%	Statistics  are  retrieved  using   thread_statistics/3  on  the
%	calling   thread.   Note   that   not    all   systems   support
%	thread-specific CPU time. Notable, this is lacking on MacOS X.
%
%	@bug Inference statistics are often a few off.
%	@see statistics/2 for obtaining statistics in your program and
%	     understanding the reported values.

time(Goal) :-
	time_state(State0),
	(   call_cleanup(catch(Goal, E, (report(State0,10), throw(E))),
			 Det = true),
	    time_true(State0),
	    (	Det == true
	    ->	!
	    ;	true
	    )
	;   report(State0, 11),
	    fail
	).

report(t(OldWall, OldTime, OldInferences), Sub) :-
	time_state(t(NewWall, NewTime, NewInferences)),
	UsedTime is NewTime - OldTime,
	UsedInf  is NewInferences - OldInferences - Sub,
	Wall     is NewWall - OldWall,
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	),
	print_message(information, time(UsedInf, UsedTime, Wall, Lips)).

time_state(t(Wall, Time, Inferences)) :-
	get_time(Wall),
	statistics(cputime, Time),
	statistics(inferences, Inferences).

time_true(State0) :-
	report(State0, 12).		% leave choice-point
time_true(State) :-
	get_time(Wall),
	statistics(cputime, Time),
	statistics(inferences, Inferences0),
	plus(Inferences0, -3, Inferences),
	nb_setarg(1, State, Wall),
	nb_setarg(2, State, Time),
	nb_setarg(3, State, Inferences),
	fail.


		 /*******************************
		 *     EXECUTION PROFILING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides a simple backward compatibility frontend on the new
(in version 5.1.10) execution profiler  with  a   hook  to  the  new GUI
visualiser for profiling results defined in library('swi/pce_profile').

Later we will add a proper textual report-generator.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	prolog:show_profile_hook/2.

%%	profile(:Goal, +ShowStyle, +TopN)
%
%	Run Goal under the execution profiler and show the top TopN
%	goals using ShowStyle.

profile(Goal) :-
	profile(Goal, plain, 25).
profile(Goal0, Style, N) :-
	expand_goal(Goal0, Goal),
	call_cleanup('$profile'(Goal),
		     prolog_statistics:show_profile(Style, N)).


%%   show_profile(N)
%
%   Show the top N functions' profile. Negative numbers or 0 show ALL
%   functions that have been called during profiling.

show_profile(N) :-
	show_profile(plain, N).

show_profile(How, N) :-
	profiler(Old, false),
	show_profile_(How, N),
	profiler(_, Old).

show_profile_(How, N) :-
	prolog:show_profile_hook(How, N), !.
show_profile_(How, N) :-
	prof_statistics(Stat),
	prof_statistics(time, Stat, Time),
	sort_on(How, SortKey),
	findall(KeyedNode, prof_node(SortKey, KeyedNode), Nodes),
	keysort(Nodes, Sorted),
	reverse(Sorted, HighFirst),
	format('~61t~69|~n'),
	format('Total time: ~2f seconds~n', [Time]),
	format('~61t~69|~n'),
	format('~w~t~w =~45|~t~w~60|~t~w~69|~n',
	       [ 'Predicate', 'Box Entries', 'Calls+Redos', 'Time'
	       ]),
	format('~61t~69|~n'),
	show_plain(HighFirst, N, Stat, SortKey).

sort_on(plain, ticks_self).
sort_on(cumulative, ticks).

show_plain([], _, _, _).
show_plain(_, 0, _, _) :- !.
show_plain([_-H|T], N, Stat, Key) :-
	show_plain(H, Stat, Key),
	N2 is N - 1,
	show_plain(T, N2, Stat, Key).

show_plain(Node, Stat, Key) :-
	value(label,			   Node, Pred),
	value(call,			   Node, Call),
	value(redo,			   Node, Redo),
	value(time(Key, percentage, Stat), Node, Percent),
	IntPercent is round(Percent*10),
	Entry is Call + Redo,
	format('~w~t~D =~45|~t~D+~55|~D ~t~1d%~69|~n',
	       [Pred, Entry, Call, Redo, IntPercent]).


		 /*******************************
		 *	   DATA GATHERING	*
		 *******************************/

%%	prof_statistics(-Node) is det.
%
%	Get overall statistics
%
%	@param Node	term of the format prof(Ticks, Account, Time, Nodes)

prof_statistics(prof(Ticks, Account, Time, Nodes)) :-
	'$prof_statistics'(Ticks, Account, Time, Nodes).

prof_statistics(ticks, Term, Ticks) :-
	arg(1, Term, Ticks).
prof_statistics(accounting, Term, Ticks) :-
	arg(2, Term, Ticks).
prof_statistics(time, Term, Ticks) :-
	arg(3, Term, Ticks).
prof_statistics(nodes, Term, Ticks) :-
	arg(4, Term, Ticks).


%%	prof_node(+Field, -Pairs) is nondet.
%
%	Collect data for each of the interesting predicate.
%
%	@param Field specifies the field to use as key in each pair.
%	@param Pair is a term of the following format:
%
%	  ==
%	  KeyValue-node(Pred,
%		        TimeSelf, TimeSiblings,
%			Calls, Redo, Recursive,
%		        Parents)
%	  ==
%

prof_node(KeyOn, Node) :-
	setup_call_cleanup(
	    ( current_prolog_flag(access_level, Old),
	      set_prolog_flag(access_level, system)
	    ),
	    get_prof_node(KeyOn, Node),
	    set_prolog_flag(access_level, Old)).

get_prof_node(KeyOn, Key-Node) :-
	Node = node(M:H,
		    TicksSelf, TicksSiblings,
		    Call, Redo,
		    Parents, Siblings),
	current_predicate(_, M:H),
	\+ predicate_property(M:H, imported_from(_)),
	'$prof_procedure_data'(M:H,
			       TicksSelf, TicksSiblings,
			       Call, Redo,
			       Parents, Siblings),
	value(KeyOn, Node, Key).

key(predicate,	    1).
key(ticks_self,	    2).
key(ticks_siblings, 3).
key(call,	    4).
key(redo,	    5).
key(callers,	    6).
key(callees,	    7).

value(name, Data, Name) :- !,
	arg(1, Data, Pred),
	predicate_functor_name(Pred, Name).
value(label, Data, Label) :- !,
	arg(1, Data, Pred),
	predicate_label(Pred, Label).
value(ticks, Data, Ticks) :- !,
	arg(2, Data, Self),
	arg(3, Data, Siblings),
	Ticks is Self + Siblings.
value(time(Key, percentage, Stat), Data, Percent) :- !,
	value(Key, Data, Ticks),
	prof_statistics(ticks, Stat, Total),
	prof_statistics(accounting, Stat, Account),
	(   Total-Account > 0
	->  Percent is 100 * (Ticks/(Total-Account))
	;   Percent is 0.0
	).
value(Name, Data, Value) :-
	key(Name, Arg),
	arg(Arg, Data, Value).

%%	predicate_label(+Head, -Label)
%
%	Create a human-readable label for the given head

predicate_label(M:H, Label) :- !,
	functor(H, Name, Arity),
	(   hidden_module(M, H)
	->  atomic_list_concat([Name, /, Arity], Label)
	;   atomic_list_concat([M, :, Name, /, Arity], Label)
	).
predicate_label(H, Label) :- !,
	functor(H, Name, Arity),
	atomic_list_concat([Name, /, Arity], Label).

hidden_module(system, _).
hidden_module(user, _).
hidden_module(M, H) :-
	predicate_property(system:H, imported_from(M)).

%%	predicate_functor_name(+Head, -Name)
%
%	Return the (module-free) name of the predicate for sorting
%	purposes.

predicate_functor_name(_:H, Name) :- !,
	predicate_functor_name(H, Name).
predicate_functor_name(H, Name) :-
	functor(H, Name, _Arity).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(time(UsedInf, UsedTime, Wall, Lips)) -->
	[ '~D inferences, ~3f CPU in ~3f seconds (~w% CPU, ~w Lips)'-
	  [UsedInf, UsedTime, Wall, Perc, Lips] ],
	{   Wall > 0
	->  Perc is round(100*UsedTime/Wall)
	;   Perc = ?
	}.
