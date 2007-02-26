/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(prolog_stack,
	  [ get_prolog_backtrace/2,	% +MaxDepth, -Stack
	    get_prolog_backtrace/3,	% +Frame, +MaxDepth, -Stack
	    print_prolog_backtrace/2,	% +Stream, +Stack
	    backtrace/1			% +MaxDepth
	  ]).
:- use_module(library(prolog_clause)).
:- use_module(library(debug)).
:- use_module(library(lists)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines more high-level primitives for examining the Prolog
stack.  It is defined for debugging purposes.

Status
------

This module is in an early development  status. Please be aware that the
Prolog representation of a backtrace as  well   as  the printed form are
subject to change.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	get_prolog_backtrace(+MaxDepth, -Backtrace)
%	
%	Return a Prolog structure representing a backtrace from the
%	current location.  The backtrace is a list of frames.  Each
%	frame is represented as one of
%	
%		* frame(Level, Clause, PC)
%		* frame(Level, foreign(Name/Arity), foreign)
%	
%	MaxDepth defines the maximum number of frames returned.

get_prolog_backtrace(MaxDepth, Stack) :-
	prolog_current_frame(Fr),
	prolog_frame_attribute(Fr, pc, PC),
	prolog_frame_attribute(Fr, parent, Parent),
	backtrace(MaxDepth, Parent, PC, Stack).

get_prolog_backtrace(Fr, MaxDepth, Stack) :-
	backtrace(MaxDepth, Fr, call, Stack).

backtrace(0, _, _, []) :- !.
backtrace(MaxDepth, Fr, PC, [frame(Level, Where)|Stack]) :-
	prolog_frame_attribute(Fr, level, Level),
	(   PC == foreign
	->  prolog_frame_attribute(Fr, goal, Goal),
	    predicate_indicator(Goal, Pred),
	    Where = foreign(Pred)
	;   PC == call
	->  prolog_frame_attribute(Fr, goal, Goal),
	    predicate_indicator(Goal, Pred),
	    Where = call(Pred)
	;   prolog_frame_attribute(Fr, clause, Clause),
	    Where = clause(Clause, PC)
	),
	(   prolog_frame_attribute(Fr, pc, PC2)
	->  true
	;   PC2 = foreign
	),
	(   prolog_frame_attribute(Fr, parent, Parent)
	->  D2 is MaxDepth - 1,
	    backtrace(D2, Parent, PC2, Stack)
	;   Stack = []
	).

predicate_indicator(M:G, PI) :- !,
	(   public_module(M)
	->  predicate_indicator(G, PI)
	;   PI = M:PI2,
	    predicate_indicator(G, PI2)
	).
predicate_indicator(G, Name/Arity) :-
	functor(G, Name, Arity).

public_module(user) :- !.
public_module(system) :- !.
public_module(M) :-
	sub_atom(M, 0, _, _, $).


%%	print_prolog_backtrace(+Stream, +Backtrace)
%	
%	Print a stacktrace in human readable form.

print_prolog_backtrace(Stream, Backtrace) :-
	phrase(message(Backtrace), Lines),
	print_message_lines(Stream, '', Lines).

message([]) -->
	[].
message([H|T]) -->
	message(H),
	(   {T == []}
	->  []
	;   [nl],
	    message(T)
	).

message(frame(Level, Where)) -->
	level(Level),
	where(Where).

where(foreign(PI)) -->
	[ '~w <foreign>'-[PI] ].
where(call(PI)) -->
	[ '~w'-[PI] ].
where(clause(Clause, PC)) -->
	{ subgoal_position(Clause, PC, File, CharA, _CharZ),
	  File \= @(_),			% XPCE Object reference
	  lineno(File, CharA, Line),
	  (   user:prolog_clause_name(Clause, PredName)
	  ->  true
	  ;   nth_clause(Head, _N, Clause),
	      predicate_name(user:Head, PredName)
	  )
	}, !,
	[ '~w at ~w:~d'-[PredName, File, Line] ].
where(clause(Clause, _PC)) -->
	{ clause_name(Clause, ClauseName)
	},
	[ '~w <no source>'-[ClauseName] ].

level(Level) -->
	[ '~|~t[~D]~8+ '-[Level] ].


%%	backtrace(+MaxDepth)
%	
%	Get and print a stacktrace to the user_error stream.

backtrace(MaxDepth) :-
	get_prolog_backtrace(MaxDepth, Stack),
	print_prolog_backtrace(user_error, Stack).


subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
	clause_info(ClauseRef, File, TPos, _),
	'$clause_term_position'(ClauseRef, PC, List),
	debug(clause, 'Term-position: ~w~n', [List]),
	find_subgoal(List, TPos, PosTerm),
	arg(1, PosTerm, CharA),
	arg(2, PosTerm, CharZ).

find_subgoal([], Pos, Pos).
find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
	nth1(A, PosL, Pos),
	find_subgoal(T, Pos, SPos).


%%	lineno(+File, +Char, -Line)
%
%	Translate a character location to a line-number.

lineno(File, Char, Line) :-
	open(File, read, Fd),
	lineno_(Fd, Char, Line0),
	close(Fd),
	Line = Line0.

lineno_(Fd, Char, L) :-
	stream_property(Fd, position('$stream_position'(C,L0,_,_))),
	C > Char, !,
	L is L0-1.
lineno_(Fd, Char, L) :-
	skip(Fd, 10),
	lineno_(Fd, Char, L).
