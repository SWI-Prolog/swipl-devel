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
profile(_, Style, _) :-
	throw(error(domain_error(profile_type, Style), _)).

$time_rval(Goal, true) :-
	time(Goal), !.
$time_rval(_, false).	
