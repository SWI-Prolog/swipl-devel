/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/


:- module(prolog_statistics,
	  [ time/1,
	    profiler/2,
	    show_profile/1,
	    profile/3
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

%    profile(-Old, +New)
%    change or query profiling status.

profiler(Old, New) :-
	'$profile'(OldInt, OldInt), 
	map_profile(Old, OldInt), 
	atom(New), 
	map_profile(New, NewInt), !, 
	'$profile'(_, NewInt).
profiler(_, New) :-
	throw(error(domain_error(profile_type, New), _)).


map_profile(off, 	0).
map_profile(cumulative, 1).
map_profile(plain, 	2).


%   show_profile(N)
%   Show the top N functions' profile. Negative numbers or 0 show ALL
%   functions that have been called during profiling.

show_profile(N) :-
	findall( triple(Perc, Calls, Module:Head), 
		 enum_profile_count(Module:Head, Calls, Perc), 
		 List), 
	sort(List, Sorted), 
	reverse(Sorted, HighFirst), 
	format('~w~t~w =~41|~t~w~57| = ~w ~t~w~79|~n',
	       [ 'Predicate', 'Box Entries', 'Calls+Redos'
	       , 'Exits+Fails', 'Time'
	       ]),
	format('~61t~79|~n'),
	show_profile(N, HighFirst).

enum_profile_count(Head, Calls, Perc) :-
	current_predicate(_, Head), 
	\+ predicate_property(Head, imported_from(_)), 
	profile_count(Head, Calls, Perc), 
	Calls \== 0.

show_profile(0, _) :- !.
show_profile(_, []) :- !.
show_profile(N, [triple(Prom, Total, Pred)|Rest]) :-
	predicate_name(Pred, Name),
	profile_box(Pred, Calls, Redos, Exits, Fails),
	format('~w~t~D =~41|~t~D+~D~57| = ~D+~D ~t~1d%~79|~n',
	       [Name, Total, Calls, Redos, Exits, Fails, Prom]), 
	succ(M, N), 
	show_profile(M, Rest).

:- module_transparent
	profile/3.

profile(Goal, Style, N) :-
	profiler(_, off), 
	reset_profiler, 
	profiler(_, Style), 
	(   catch(time(Goal), E, fail)
	->  Rval = true
	;   Rval = fail
	),
	profiler(_, off), 
	show_profile(N), !, 
	(   nonvar(E)
	->  throw(E)
	;   Rval == true
	).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(time(UsedInf, UsedTime, Lips)) -->
	[ '~D inferences in ~2f seconds (~w Lips)'-[UsedInf, UsedTime, Lips] ].


:- module_transparent
	predicate_name/2.

%	predicate_name(+Head, -String)
%	Convert `Head' into a predicate name.

predicate_name(Goal, String) :-
	'$strip_module'(Goal, Module, Head), 
	functor(Head, Name, Arity), 
	(   memberchk(Module, [user, system])
	->  sformat(String, '~w/~w',	[Name, Arity])
	;   sformat(String, '~w:~w/~w',	[Module, Name, Arity])
	).


