/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: profiler interface
*/

:- module($profile,
	[ profiler/2
	, show_profile/1
	, profile/3
	]).

%    profile(-Old, +New)
%    change or query profiling status.

profiler(Old, New) :-
	$profile(OldInt, OldInt), 
	$map_profile(Old, OldInt), 
	atom(New), 
	$map_profile(New, NewInt), !, 
	$profile(_, NewInt).

$map_profile(off, 	  0).
$map_profile(cumulative,  1).
$map_profile(plain, 	  2).


%   show_profile(N)
%   Show the top N functions' profile. Negative numbers or 0 show ALL
%   functions that have been called during profiling.

show_profile(N) :-
	findall( triple(Perc, Calls, Module:Head), 
		 $profile_count(Module:Head, Calls, Perc), 
		 List), 
	sort(List, Sorted), 
	reverse(Sorted, HighFirst), 
	format('~w~t~w =~41|~t~w~57| = ~w ~t~w~79|~n',
	       [ 'Predicate', 'Box Entries', 'Calls+Redos'
	       , 'Exits+Fails', 'Time'
	       ]),
	format('~61t~79|~n'),
	$show_profile(N, HighFirst).

$profile_count(Head, Calls, Perc) :-
	current_predicate(_, Head), 
	\+ predicate_property(Head, imported_from(_)), 
	profile_count(Head, Calls, Perc), 
	Calls \== 0.

$show_profile(0, _) :- !.
$show_profile(_, []) :- !.
$show_profile(N, [triple(Prom, Total, Pred)|Rest]) :-
	$predicate_name(Pred, Name),
	profile_box(Pred, Calls, Redos, Exits, Fails),
	format('~w~t~D =~41|~t~D+~D~57| = ~D+~D ~t~1d%~79|~n',
	       [Name, Total, Calls, Redos, Exits, Fails, Prom]), 
	succ(M, N), 
	$show_profile(M, Rest).

:- module_transparent
	profile/3,
	$time_rval/2.

profile(Goal, Style, N) :-
	memberchk(Style, [plain, cumulative]), !, 
	profiler(_, off), 
	reset_profiler, 
	profiler(_, Style), 
	$time_rval(Goal, Rval), 
	profiler(_, off), 
	show_profile(N), !, 
	Rval == true.
profile(_, _, _) :-
	$warning('profile/3: second argument should be one of {plain, cumulative}'), 
	fail.

$time_rval(Goal, true) :-
	time(Goal), !.
$time_rval(_, false).	
