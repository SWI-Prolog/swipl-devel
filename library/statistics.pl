/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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


:- module(prolog_statistics,
	  [ time/1,			% :Goal
	    profile/1,			% :Goal
	    profile/3,			% :Goal, +Style, +Top
	    show_profile/1		% +Top
	  ]).

:- module_transparent
	time/1.

%	time(:Goal)
%
%	Time the execution of Goal.  Possible choice-points of Goal are
%	removed.

time(Goal) :-
	statistics(cputime, OldTime), 
	statistics(inferences, OldInferences), 
	(   catch(Goal, E, true)
	->  Result = yes
	;   Result = no
	),
	statistics(inferences, NewInferences), 
	statistics(cputime, NewTime), 
	UsedTime is NewTime - OldTime, 
	UsedInf  is NewInferences - OldInferences - 3, 
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	), 
	print_message(informational, time(UsedInf, UsedTime, Lips)),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).


		 /*******************************
		 *     EXECUTION PROFILING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides a simple backward compatibility frontend on the new
(in version 5.1.10) execution profiler  with  a   hook  to  the  new GUI
visualiser for profiling results defined in library('swi/pce_profile').

Later we will add a proper textual report-generator.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module_transparent
	profile/1,
	profile/3.
:- multifile
	prolog:show_profile_hook/2.

%	profile(:Goal, +ShowStyle, +TopN)
%	
%	Run Goal under the execution profiler and show the top TopN
%	goals using ShowStyle.

profile(Goal) :-
	profile(Goal, plain, 25).
profile(Goal, Style, N) :-
	call_cleanup('$profile'(Goal),
		     prolog_statistics:show_profile(Style, N)).


%   show_profile(N)
%
%   Show the top N functions' profile. Negative numbers or 0 show ALL
%   functions that have been called during profiling.

show_profile(N) :-
	show_profile(plain, N).

show_profile(How, N) :-
	prolog:show_profile_hook(How, N), !.
show_profile(How, N) :-
	prof_statistics(Stat),
	sort_on(How, SortKey),
	findall(KeyedNode, prof_node(SortKey, KeyedNode), Nodes),
	keysort(Nodes, Sorted),
	reverse(Sorted, HighFirst), 
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

%	prof_statistics(prof(Ticks, Account, Time, Nodes))
%	
%	Get overall statistics

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


%	prof_node(+KeyOn
%		  Key-node(Pred,
%		           TimeSelf, TimeSiblings,
%		           Calls, Redo, Recursive,
%		           Parents))
%
%	Collect data for each of the interesting predicates.

prof_node(KeyOn, Node) :-
	style_check(+dollar),
	call_cleanup(get_prof_node(KeyOn, Node), style_check(-dollar)).

get_prof_node(KeyOn, Key-Node) :-
	Node = node(M:H,
		    TicksSelf, TicksSiblings,
		    Call, Redo, Recursive,
		    Parents, Siblings),
	current_predicate(_, M:H),
	\+ predicate_property(M:H, imported_from(_)),
	'$prof_procedure_data'(M:H,
			       TicksSelf, TicksSiblings,
			       Call, Redo, Recursive,
			       Parents, Siblings),
	value(KeyOn, Node, Key).

key(predicate,	    1).
key(ticks_self,	    2).
key(ticks_siblings, 3).
key(call,	    4).
key(redo,	    5).
key(recursive,	    6).
key(callers,	    7).
key(callees,	    8).

value(name, Data, Name) :- !,
	arg(1, Data, Pred),
	predicate_name(Pred, Name).
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

%	predicate_label(+Head, -Label)
%	
%	Create a human-readable label for the given head

predicate_label(M:H, Label) :- !,
	functor(H, Name, Arity),
	(   hidden_module(M, H)
	->  concat_atom([Name, /, Arity], Label)
	;   concat_atom([M, :, Name, /, Arity], Label)
	).
predicate_label(H, Label) :- !,
	functor(H, Name, Arity),
	concat_atom([Name, /, Arity], Label).

hidden_module(system, _).
hidden_module(user, _).
hidden_module(M, H) :-
	predicate_property(system:H, imported_from(M)).

%	predicate_name(+Head, -Name)
%	
%	Return the (module-free) name of the predicate for sorting
%	purposes.

predicate_name(_:H, Name) :- !,
	predicate_name(H, Name).
predicate_name(H, Name) :-
	functor(H, Name, _Arity).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(time(UsedInf, UsedTime, Lips)) -->
	[ '~D inferences in ~2f seconds (~w Lips)'-[UsedInf, UsedTime, Lips] ].
